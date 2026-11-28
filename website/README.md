# Build the R languageserver guide

The site uses Markdown under `docs/` and the navigation in
`docs/.vitepress/config.mjs`. Use Node.js 24 and the pnpm version pinned in
`package.json`. From this directory, run:

```sh
pnpm install --frozen-lockfile
pnpm run docs:dev
```

To check the static site as GitHub Pages will serve it, run:

```sh
pnpm run docs:build
python3 scripts/check-providers.py
python3 scripts/check-links.py
pnpm run docs:preview
```

Open the local URL printed by VitePress, including `/languageserver/`. The
two checks use the Python 3 standard library. The provider check compares the
feature index with the server’s registered requests, notifications, and push
diagnostics. When adding a provider, link its entry to a worked example.

Verify new examples against the current CRAN release. Mark examples requiring
unreleased behavior as **Development** in their guide and link them from the
development overview. When they ship, update the release baseline in the
feature index and guides. The release history comes from the package’s
`NEWS.md`. When changing settings, update the defaults in the reference and
link to that entry from the guides.

The Pages workflow builds pull requests that change the site, the server's
handler registry, `NEWS.md`, or the workflow itself. Matching pushes to
`master` publish the build. Running **Pages** manually on `master` also
publishes it.
