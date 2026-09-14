"""Offline tests of release selection, permissions, and recovery after failure."""

import contextlib
import io
from pathlib import Path
import tempfile
import unittest
from unittest.mock import Mock, call, patch
from urllib.error import HTTPError

import cran_release as release


class HistoryTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.directory.cleanup)
        self.repo = Path(self.directory.name)
        release.git("init", "-q", "-b", "main", cwd=self.repo)
        release.git("config", "user.name", "Release test", cwd=self.repo)
        release.git("config", "user.email", "release-test@example.invalid", cwd=self.repo)
        release.git("config", "commit.gpgsign", "false", cwd=self.repo)
        release.git("config", "core.hooksPath", "/dev/null", cwd=self.repo)
        self.commit("0.3.18", "Old release")

    def commit(self, version, message):
        (self.repo / "DESCRIPTION").write_text(
            f"Package: languageserver\nVersion: {version}\nTitle: {message}\n")
        (self.repo / "NEWS.md").write_text(
            "# languageserver 0.3.19\n\n- Released fix.\n\n"
            "# languageserver 0.3.18\n\n- Older fix.\n")
        release.git("add", ".", cwd=self.repo)
        release.git("commit", "-qm", message, cwd=self.repo)
        return release.git("rev-parse", "HEAD", cwd=self.repo)

    def test_selects_release_before_unbumped_and_bumped_development(self):
        expected = self.commit("0.3.19", "Release")
        self.commit("0.3.19", "Development without version bump")
        self.commit("0.3.19.9000", "Development version")
        (self.repo / "DESCRIPTION").write_text("Package: languageserver\nVersion: 9.9.9\n")
        self.assertEqual(release.release_commit("languageserver", "0.3.19", self.repo), expected)
        self.assertEqual(release.release_notes("languageserver", "0.3.19", expected, self.repo),
                         "- Released fix.\n")

    def test_selects_merge_where_version_enters_default_branch(self):
        release.git("checkout", "-qb", "release", cwd=self.repo)
        self.commit("0.3.19", "Release branch")
        release.git("checkout", "-q", "main", cwd=self.repo)
        release.git("merge", "--no-ff", "-qm", "Merge release", "release", cwd=self.repo)
        expected = release.git("rev-parse", "HEAD", cwd=self.repo)
        self.commit("0.3.19.9000", "Development version")
        self.assertEqual(release.release_commit("languageserver", "0.3.19", self.repo), expected)

    def test_unmerged_and_prefix_versions_do_not_match(self):
        release.git("checkout", "-qb", "release", cwd=self.repo)
        self.commit("0.3.19", "Unmerged release")
        release.git("checkout", "-q", "main", cwd=self.repo)
        self.commit("0.3.19.9000", "Development version")
        with self.assertRaisesRegex(RuntimeError, "No committed DESCRIPTION"):
            release.release_commit("languageserver", "0.3.19", self.repo)

    def test_explicit_default_branch_ref_excludes_local_feature_commits(self):
        default_head = release.git("rev-parse", "HEAD", cwd=self.repo)
        self.commit("0.3.19", "Unmerged feature version")
        with self.assertRaisesRegex(RuntimeError, "No committed DESCRIPTION"):
            release.release_commit("languageserver", "0.3.19", self.repo, ref=default_head)

    def test_missing_release_notes_abort(self):
        expected = self.commit("0.3.20", "Release with missing notes")
        with self.assertRaisesRegex(RuntimeError, "Missing nonempty NEWS"):
            release.release_notes("languageserver", "0.3.20", expected, self.repo)


class FakeGitHub:
    def __init__(self, target=None, published=False, annotated=False):
        self.target = target
        self.annotated = annotated
        self.release = ({"html_url": "https://example.invalid/v0.3.19",
                         "draft": False, "prerelease": False} if published else None)
        self.writes = []
        self.fail_release = False

    def request(self, method, path, data=None, allow_missing=False):
        if method == "GET" and path == "/git/ref/tags/v0.3.19":
            if self.target is None:
                return None
            return {"object": {"type": "tag" if self.annotated else "commit", "sha": self.target}}
        if method == "GET" and path == f"/git/tags/{self.target}":
            return {"object": {"type": "commit", "sha": self.target}}
        if method == "GET" and path == "/releases/tags/v0.3.19":
            return self.release
        if method == "POST":
            self.writes.append((path, data))
            if path == "/git/refs":
                self.target = data["sha"]
                return {}
            if path == "/releases":
                if self.fail_release:
                    raise RuntimeError("GitHub temporary error")
                self.release = {"html_url": "https://example.invalid/v0.3.19",
                                "draft": data["draft"], "prerelease": data["prerelease"]}
                return self.release
        raise AssertionError(f"Unexpected API call: {method} {path}")


class PublishTests(unittest.TestCase):
    def setUp(self):
        self.output = contextlib.redirect_stdout(io.StringIO())
        self.output.__enter__()
        self.addCleanup(self.output.__exit__, None, None, None)

    def publish(self, api, publish=True):
        return release.publish_release(api, "0.3.19", "release-commit", "Release notes\n", publish)

    def test_preview_makes_no_writes(self):
        api = FakeGitHub()
        self.publish(api, publish=False)
        self.assertEqual(api.writes, [])

    def test_publish_uses_exact_commit_and_notes_and_is_idempotent(self):
        api = FakeGitHub()
        self.publish(api)
        self.publish(api)
        self.assertEqual(api.writes, [
            ("/git/refs", {"ref": "refs/tags/v0.3.19", "sha": "release-commit"}),
            ("/releases", {"tag_name": "v0.3.19", "target_commitish": "release-commit",
                           "name": "v0.3.19", "body": "Release notes\n",
                           "draft": False, "prerelease": False}),
        ])

    def test_existing_lightweight_and_annotated_tags_are_reused(self):
        for annotated in (False, True):
            with self.subTest(annotated=annotated):
                api = FakeGitHub(target="release-commit", annotated=annotated)
                self.publish(api)
                self.assertEqual([path for path, _ in api.writes], ["/releases"])

    def test_conflicting_tag_is_never_modified_even_with_existing_release(self):
        for published in (False, True):
            with self.subTest(published=published):
                api = FakeGitHub(target="wrong-commit", published=published)
                with self.assertRaisesRegex(RuntimeError, "Refusing to move"):
                    self.publish(api)
                self.assertEqual(api.writes, [])

    def test_existing_draft_prerelease_or_missing_tag_requires_review(self):
        for state in ("draft", "prerelease", "missing-tag"):
            with self.subTest(state=state):
                api = FakeGitHub(target="release-commit", published=True)
                if state == "missing-tag":
                    api.target = None
                else:
                    api.release[state] = True
                with self.assertRaisesRegex(RuntimeError, "manual review"):
                    self.publish(api)
                self.assertEqual(api.writes, [])

    def test_recovers_from_release_failure_after_tag_creation(self):
        api = FakeGitHub()
        api.fail_release = True
        with self.assertRaisesRegex(RuntimeError, "temporary error"):
            self.publish(api)
        api.fail_release = False
        self.publish(api)
        self.assertEqual([path for path, _ in api.writes].count("/git/refs"), 1)
        self.assertIsNotNone(api.release)

    def test_changed_tag_aborts_publication(self):
        api = FakeGitHub()
        with patch.object(release, "tag_commit", side_effect=[None, "another-commit"]):
            with self.assertRaisesRegex(RuntimeError, "changed before release"):
                self.publish(api)
        self.assertEqual([path for path, _ in api.writes], ["/git/refs"])


class InputAndAPITests(unittest.TestCase):
    def test_cran_index_requires_unique_exact_package(self):
        index = "Package: other\nVersion: 9.0\n\nPackage: languageserver\nVersion: 0.3.19\n"
        self.assertEqual(release.cran_version("languageserver", index), "0.3.19")
        for invalid in ("", index + "\n" + index, "Package: languageserver\nVersion: bad",
                        "Package: languageserver"):
            with self.subTest(index=invalid):
                with self.assertRaisesRegex(RuntimeError, "exactly one valid version"):
                    release.cran_version("languageserver", invalid)

    def test_live_runs_resolve_the_remote_default_branch_commit(self):
        api = Mock()
        api.request.side_effect = [{"default_branch": "main"},
                                   {"object": {"type": "commit", "sha": "a" * 40}}]
        self.assertEqual(release.default_branch_head(api), "a" * 40)
        self.assertEqual(api.request.call_args_list,
                         [call("GET", ""), call("GET", "/git/ref/heads/main")])

    def test_workflow_token_is_sent_as_bearer_without_pat_validation(self):
        api = release.GitHub("owner/repo", "ghs_workflow-token")
        with patch.object(release, "urlopen", return_value=io.BytesIO(b'{}')) as open_url:
            api.request("GET", "/releases/tags/v0.3.19")
        request = open_url.call_args.args[0]
        self.assertEqual(request.get_header("Authorization"), "Bearer ghs_workflow-token")
        self.assertEqual(request.get_header("X-github-api-version"), "2022-11-28")

    def test_only_404_is_treated_as_absent(self):
        api = release.GitHub("owner/repo")
        for status in (401, 403, 404, 429, 500):
            with self.subTest(status=status):
                error = HTTPError("https://example.invalid", status, "error", {}, None)
                with patch.object(release, "urlopen", side_effect=error):
                    if status == 404:
                        self.assertIsNone(api.request("GET", "/releases/tags/v0.3.19", allow_missing=True))
                    else:
                        with self.assertRaisesRegex(RuntimeError, f"HTTP {status}"):
                            api.request("GET", "/releases/tags/v0.3.19", allow_missing=True)

    def test_publish_cannot_use_offline_inputs(self):
        for option in ("--plan-only", "--cran-index=PACKAGES"):
            with self.subTest(option=option):
                with patch("sys.argv", ["cran_release.py", "--publish", option]):
                    with contextlib.redirect_stderr(io.StringIO()):
                        with self.assertRaises(SystemExit) as error:
                            release.main()
                self.assertEqual(error.exception.code, 2)


if __name__ == "__main__":
    unittest.main()
