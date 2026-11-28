"""Check links, assets and heading fragments against the built Pages site."""

from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urljoin, urlsplit


class Page(HTMLParser):
    def __init__(self, path):
        super().__init__()
        self.ids = set()
        self.links = []
        self.feed(path.read_text(encoding="utf-8"))
        self.close()

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if "id" in attrs:
            self.ids.add(attrs["id"])
        for key in ("href", "src"):
            if attrs.get(key):
                self.links.append(attrs[key])


dist = Path(__file__).resolve().parents[1] / "docs/.vitepress/dist"
base = "/languageserver/"
if not (dist / "index.html").is_file():
    raise SystemExit("Build the site before checking links.")

pages = {path: Page(path) for path in dist.rglob("*.html")}
errors = []
for path, page in pages.items():
    url = base + path.relative_to(dist).as_posix()
    for link in page.links:
        target = urlsplit(urljoin(url, link))
        if target.scheme or target.netloc:
            continue
        target_path = unquote(target.path)
        if not target_path.startswith(base):
            errors.append(f"{path.relative_to(dist)}: {link} escapes the Pages base")
            continue
        file = dist / target_path[len(base):]
        if file.is_dir():
            file = file / "index.html"
        elif not file.is_file():
            file = Path(str(file) + ".html")
        if not file.is_file():
            errors.append(f"{path.relative_to(dist)}: missing {link}")
        elif target.fragment and file in pages and unquote(target.fragment) not in pages[file].ids:
            errors.append(f"{path.relative_to(dist)}: missing heading for {link}")

if errors:
    raise SystemExit("\n".join(sorted(set(errors))))
print(f"Checked local links, assets and fragments on {len(pages)} HTML pages.")
