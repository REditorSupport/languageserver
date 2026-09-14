#!/usr/bin/env python3
"""Publish the current CRAN release from its original default-branch commit.

Uses only Python's standard library and git. The default is a read-only preview;
--publish explicitly enables GitHub writes. GH_TOKEN accepts the workflow token
without imposing personal-access-token format restrictions.
"""

import argparse
import gzip
import json
import os
from pathlib import Path
import re
import subprocess
import sys
from urllib.error import HTTPError
from urllib.parse import quote
from urllib.request import Request, urlopen


CRAN_INDEX = "https://cran.r-project.org/src/contrib/PACKAGES.gz"


def dcf_records(text):
    """Read the Package/Version fields from CRAN's DCF index or DESCRIPTION."""
    for paragraph in re.split(r"\n\s*\n", text.strip()):
        fields = {}
        for line in paragraph.splitlines():
            if line and not line[0].isspace() and ":" in line:
                key, value = line.split(":", 1)
                fields[key] = value.strip()
        if fields:
            yield fields


def cran_version(package, index=None):
    if index is None:
        with urlopen(CRAN_INDEX, timeout=30) as response:
            index = gzip.decompress(response.read()).decode("utf-8")
    versions = [record.get("Version", "") for record in dcf_records(index)
                if record.get("Package") == package]
    if len(versions) != 1 or not re.fullmatch(r"[0-9]+(?:[.-][0-9]+)+", versions[0]):
        raise RuntimeError(f"CRAN must list exactly one valid version of {package}")
    return versions[0]


def git(*args, cwd=None):
    return subprocess.check_output(["git", *args], cwd=cwd, text=True).strip()


def release_commit(package, version, cwd=None, ref="HEAD"):
    """Find when the release version first entered the default branch.

    Walking only DESCRIPTION changes, oldest first, avoids tagging subsequent
    development commits even if maintainers have not bumped Version yet. The
    workflow checks out the default branch with its full history, and live runs
    resolve ref from GitHub's current default branch rather than the local HEAD.
    """
    commits = git("log", "--first-parent", "--reverse", "--format=%H",
                  ref, "--", "DESCRIPTION", cwd=cwd).splitlines()
    for commit in commits:
        description = git("show", f"{commit}:DESCRIPTION", cwd=cwd)
        fields = next(dcf_records(description))
        if fields.get("Package") == package and fields.get("Version") == version:
            return commit
    raise RuntimeError(f"No committed DESCRIPTION matches CRAN's {package} {version}")


def release_notes(package, version, commit, cwd=None):
    news = git("show", f"{commit}:NEWS.md", cwd=cwd)
    heading = re.compile(r"^# " + re.escape(package) + r" " + re.escape(version) + r"\s*$")
    lines = news.splitlines()
    for start, line in enumerate(lines):
        if heading.fullmatch(line):
            end = next((i for i in range(start + 1, len(lines))
                        if lines[i].startswith("# ")), len(lines))
            notes = "\n".join(lines[start + 1:end]).strip()
            if notes:
                return notes + "\n"
    raise RuntimeError(f"Missing nonempty NEWS.md section for {package} {version} at {commit}")


class GitHub:
    def __init__(self, repository, token=None):
        if not re.fullmatch(r"[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+", repository):
            raise RuntimeError("Specify a GitHub repository as owner/name")
        self.base = f"https://api.github.com/repos/{repository}"
        self.token = token

    def request(self, method, path, data=None, allow_missing=False):
        headers = {"Accept": "application/vnd.github+json",
                   "X-GitHub-Api-Version": "2022-11-28",
                   "User-Agent": "languageserver-cran-release"}
        if self.token:
            headers["Authorization"] = f"Bearer {self.token}"
        payload = None if data is None else json.dumps(data).encode("utf-8")
        if payload is not None:
            headers["Content-Type"] = "application/json"
        request = Request(self.base + path, data=payload, headers=headers, method=method)
        try:
            with urlopen(request, timeout=30) as response:
                return json.load(response)
        except HTTPError as error:
            if allow_missing and error.code == 404:
                return None
            raise RuntimeError(f"GitHub {method} {path} returned HTTP {error.code}") from error


def default_branch_head(api):
    branch = api.request("GET", "")["default_branch"]
    obj = api.request("GET", f"/git/ref/heads/{quote(branch, safe='')}")["object"]
    if obj["type"] != "commit" or not re.fullmatch(r"[0-9a-f]{40}", obj["sha"]):
        raise RuntimeError("GitHub's default branch must resolve to a commit")
    return obj["sha"]


def tag_commit(api, tag):
    ref = api.request("GET", f"/git/ref/tags/{tag}", allow_missing=True)
    if ref is None:
        return None
    obj = ref["object"]
    # Annotated tags may in turn reference another annotated tag.
    for _ in range(10):
        if obj["type"] == "commit":
            return obj["sha"]
        if obj["type"] != "tag":
            break
        obj = api.request("GET", f"/git/tags/{obj['sha']}")["object"]
    raise RuntimeError(f"Tag {tag} does not resolve to a commit")


def publish_release(api, version, commit, notes, publish=False):
    tag = f"v{version}"
    target = tag_commit(api, tag)
    if target is not None and target != commit:
        raise RuntimeError(f"Refusing to move {tag}: points to {target}, expected {commit}")
    release = api.request("GET", f"/releases/tags/{tag}", allow_missing=True)
    if release is not None:
        if target is None or release["draft"] or release["prerelease"]:
            raise RuntimeError(f"Existing release {tag} needs manual review")
        print(f"Already published: {release['html_url']} ({commit})")
        return release
    print(f"{'Publish' if publish else 'Preview'}: {tag} at {commit}")
    print(notes)
    if not publish:
        return None
    if target is None:
        api.request("POST", "/git/refs", {"ref": f"refs/tags/{tag}", "sha": commit})
    # Check again before publishing: never silently release a conflicting tag.
    if tag_commit(api, tag) != commit:
        raise RuntimeError(f"Tag {tag} changed before release publication")
    release = api.request("POST", "/releases", {
        "tag_name": tag, "target_commitish": commit, "name": tag,
        "body": notes, "draft": False, "prerelease": False,
    })
    print(f"Published: {release['html_url']}")
    return release


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repository", default=os.environ.get("GITHUB_REPOSITORY"))
    parser.add_argument("--cran-index", type=Path, help="Read a local PACKAGES file for offline review")
    parser.add_argument("--plan-only", action="store_true", help="Skip GitHub reads for offline review")
    parser.add_argument("--publish", action="store_true", help="Create the GitHub tag and release")
    args = parser.parse_args()
    if args.publish and (args.plan_only or args.cran_index):
        parser.error("--publish requires the live CRAN index and GitHub checks")
    token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
    if args.publish and not token:
        parser.error("--publish requires GH_TOKEN or GITHUB_TOKEN")
    if not args.plan_only and not args.repository:
        parser.error("--repository or GITHUB_REPOSITORY is required")
    api = None if args.plan_only else GitHub(args.repository, token)
    ref = "HEAD" if args.plan_only else default_branch_head(api)
    package = next(dcf_records(git("show", f"{ref}:DESCRIPTION")))["Package"]
    index = args.cran_index.read_text() if args.cran_index else None
    version = cran_version(package, index)
    commit = release_commit(package, version, ref=ref)
    notes = release_notes(package, version, commit)
    if args.plan_only:
        print(f"Preview: v{version} at {commit}\n\n{notes}")
        return
    publish_release(api, version, commit, notes, args.publish)


if __name__ == "__main__":
    try:
        main()
    except (RuntimeError, OSError, subprocess.CalledProcessError) as error:
        sys.exit(str(error))
