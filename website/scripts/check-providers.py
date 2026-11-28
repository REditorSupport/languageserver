"""Check that the feature index documents every registered LSP method."""

import re
from pathlib import Path


def registered_methods(source, registry):
    assignments = re.findall(
        rf"^[ \t]*self\${registry}\s*<-\s*list\s*\((.*?)^[ \t]*\)",
        source,
        flags=re.DOTALL | re.MULTILINE,
    )
    if len(assignments) != 1 or not assignments[0].strip():
        raise SystemExit(f"Expected one nonempty {registry} list in R/languageserver.R")

    # Require every item in the list to be recognizable; never skip a new entry.
    methods = set()
    body = re.sub(r"#[^\n]*", "", assignments[0])
    for entry in body.split(","):
        handler = re.fullmatch(r"(`[^`]+`|\w+)\s*=\s*[\w.]+", entry.strip())
        if not handler:
            raise SystemExit(f"Unrecognized {registry} entry in R/languageserver.R: {entry.strip()!r}")
        methods.add(handler[1].strip("`"))
    return methods


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[2]
    source = (root / "R/languageserver.R").read_text(encoding="utf-8")
    index = Path("website/docs/reference/capabilities.md")
    documented = set(re.findall(r"`([^`\n]+)`", (root / index).read_text(encoding="utf-8")))

    requests = registered_methods(source, "request_handlers")
    notifications = registered_methods(source, "notification_handlers")
    # Diagnostics are pushed to the editor, so they have no incoming handler.
    expected = requests | notifications | {"textDocument/publishDiagnostics"}
    missing = sorted(expected - documented)
    if missing:
        raise SystemExit(f"Missing LSP methods in {index}:\n" + "\n".join(missing))
    print(f"Documented {len(requests)} requests, {len(notifications)} notifications and push diagnostics.")
