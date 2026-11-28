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
python3 scripts/check-links.py
pnpm run docs:preview
```

Open the local URL printed by VitePress, including `/languageserver/`. The
link check uses the Python 3 standard library.

Verify new examples against the current CRAN release. Put examples requiring
unreleased behavior on the development features page, and move them to the
appropriate guide when they ship. When changing settings, update the defaults
in the reference and link to that entry from the guides.

The Pages workflow builds pull requests. A push to `master` that changes the
site or workflow publishes the build. Running **Pages** manually on `master`
also publishes it.
