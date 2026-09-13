# CRAN and GitHub releases

Finalize the release notes in `NEWS.md` and set the release `Version` in
`DESCRIPTION` when the release is ready. Merge that release to the default branch
before submitting to CRAN. After release, bump `Version` to a development version
and remove `Date`; start a new development section in `NEWS.md`.

The **CRAN release** workflow checks CRAN's source package index every six hours.
It uses the first commit on the default branch whose committed `DESCRIPTION`
matches CRAN's package and version. This is normally the release merge commit.
It takes release notes from that commit's matching `NEWS.md` section, creates
`v<version>` at that exact commit, and publishes a GitHub release. Later changes
to `DESCRIPTION`, `NEWS.md`, or other files cannot change the selected source.

The version bump must identify the finalized release. If additional release
fixes were made after that version first entered the default branch, review the
source commit manually before publication. The workflow deliberately fails when
an existing tag points elsewhere; it never moves tags or rewrites releases.
Existing drafts and prereleases also require manual review. A failed run that
created the correct tag can safely be rerun to finish publishing its release.

Use **Actions → CRAN release → Run workflow** to preview the selected commit and
release notes. `dry_run` is enabled by default. Uncheck it to publish immediately
after reviewing the preview. The workflow always checks out the default branch;
the script also reads GitHub's current default-branch commit before selecting
release history, including when invoked from a local feature branch.

The script requires Python 3 and Git, without third-party Python or R packages.
It uses the built-in `GITHUB_TOKEN` through `GH_TOKEN`; no personal token is
needed. Only the publication job has `contents: write`. Publications run one at
a time. Pull requests affecting the workflow or script run offline safety tests
with read-only permissions.

For local review, first fetch the default branch's full history. An authenticated
shell with `GH_TOKEN` or `GITHUB_TOKEN` can then preview the live CRAN/GitHub state:

```sh
python3 .github/scripts/cran_release.py --repository REditorSupport/languageserver
```

Adding `--publish` creates the tag and release. It requires a token, the live
CRAN index, and access to GitHub. No token contents are printed. A complete
offline preview instead uses the local `HEAD` and an uncompressed CRAN-style
`PACKAGES` file containing `Package:` and `Version:` fields:

```sh
python3 .github/scripts/cran_release.py --cran-index /tmp/PACKAGES --plan-only
python3 -m unittest discover -s .github/scripts -p 'test_cran_release.py' -v
```

Offline inputs cannot be combined with `--publish`. If the current remote default
branch commit is missing locally, fetch it and retry; the script will not fall
back to a potentially stale local branch.
