#!/usr/bin/env bash
# Copy the built page into a checkout of the website repo, as its /got/ folder.
#
#   scripts/sync_site.sh /path/to/website
#
# Run `Rscript -e 'targets::tar_make()'` first so web/data and the report are
# current. Nothing is committed or pushed; review the diff in the website repo.
set -euo pipefail

site="${1:?usage: scripts/sync_site.sh /path/to/website}"
here="$(cd "$(dirname "$0")/.." && pwd)"

[ -f "$site/wrangler.jsonc" ] || { echo "error: $site does not look like the website repo" >&2; exit 1; }
[ -f "$here/web/data/model.json" ] || { echo "error: web/data/model.json is missing; run the pipeline first" >&2; exit 1; }
[ -f "$here/report/index.html" ] || { echo "error: report/index.html is missing; run the pipeline first" >&2; exit 1; }

dest="$site/got"
rm -rf "$dest"
mkdir -p "$dest/js" "$dest/data" "$dest/report"
cp "$here/web/index.html" "$here/web/got.css" "$dest/"
cp "$here"/web/js/*.js "$dest/js/"
cp "$here"/web/data/*.json "$dest/data/"
cp "$here/report/index.html" "$dest/report/index.html"

echo "Synced to $dest"
echo "Preview: (cd \"$site\" && python3 -m http.server 8000) then open http://localhost:8000/got/"
