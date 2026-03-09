#!/usr/bin/env bash
# generate-index.sh — Scan docs/*/index.html for <title> tags and build
# a root docs/index.html linking to each sub-page.
#
# Usage: bash scripts/generate-index.sh

set -euo pipefail

DOCS_DIR="docs"
OUT="$DOCS_DIR/index.html"

# Pull project title from CITATION.cff (falls back to repo directory name)
PROJECT_NAME=$(sed -n 's/^title: *"\(.*\)"/\1/p' CITATION.cff 2>/dev/null | head -1)
if [ -z "$PROJECT_NAME" ]; then
  PROJECT_NAME=$(basename "$(pwd)")
fi

# Helper: extract entries from a list of index.html files
extract_entries() {
  local html_file rel_path rel_dir display_name title
  while IFS= read -r html_file; do
    [ -z "$html_file" ] && continue
    rel_path="${html_file#$DOCS_DIR/}"
    rel_dir="$(dirname "$rel_path")"
    display_name="$(basename "$rel_dir")"
    title=$(sed -n 's/.*<title>\(.*\)<\/title>.*/\1/p' "$html_file" | head -1)
    [ -z "$title" ] && title="$display_name"
    echo "$rel_dir|$title"
  done
}

# Collect chunks (docs/chunks/*/index.html)
chunks=()
while IFS= read -r line; do
  chunks+=("$line")
done < <(find "$DOCS_DIR/chunks" -mindepth 2 -maxdepth 2 -name "index.html" 2>/dev/null | sort | extract_entries)

# Collect viewers (docs/*viewer*/index.html)
viewers=()
while IFS= read -r line; do
  viewers+=("$line")
done < <(find "$DOCS_DIR" -mindepth 2 -maxdepth 2 -name "index.html" -not -path "$DOCS_DIR/chunks/*" -not -path "$OUT" | sort | extract_entries)
# Filter to only folders containing "viewer"
filtered_viewers=()
other=()
for entry in "${viewers[@]}"; do
  rel_dir="${entry%%|*}"
  if [[ "$rel_dir" == *viewer* ]]; then
    filtered_viewers+=("$entry")
  else
    other+=("$entry")
  fi
done

# Build the HTML
cat > "$OUT" <<HEADER
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${PROJECT_NAME}</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      max-width: 720px;
      margin: 0 auto;
      padding: 40px 20px;
      background: #fafafa;
      color: #333;
    }
    h1 { font-size: 1.5rem; margin-bottom: 0.3rem; }
    h2 { font-size: 1.15rem; margin-top: 2rem; margin-bottom: 0.5rem; color: #555; }
    .subtitle { color: #666; font-size: 0.95rem; margin-bottom: 2rem; }
    ul { list-style: none; padding: 0; }
    li { margin-bottom: 0.75rem; }
    a {
      color: #0366d6;
      text-decoration: none;
      font-size: 1.05rem;
    }
    a:hover { text-decoration: underline; }
    .folder {
      color: #999;
      font-size: 0.85rem;
      margin-left: 0.5rem;
    }
    footer {
      margin-top: 3rem;
      padding-top: 1rem;
      border-top: 1px solid #ddd;
      color: #999;
      font-size: 0.8rem;
    }
  </style>
</head>
<body>
  <h1>${PROJECT_NAME}</h1>
  <p class="subtitle">Interactive viewers and modular outputs</p>
HEADER

# Write a section with heading and list of entries
write_section() {
  local heading="$1"
  shift
  local items=("$@")
  if [ ${#items[@]} -gt 0 ]; then
    echo "  <h2>$heading</h2>" >> "$OUT"
    echo "  <ul>" >> "$OUT"
    for entry in "${items[@]}"; do
      rel_dir="${entry%%|*}"
      title="${entry#*|}"
      cat >> "$OUT" <<LINK
    <li><a href="${rel_dir}/">${title}</a> <span class="folder">${rel_dir}/</span></li>
LINK
    done
    echo "  </ul>" >> "$OUT"
  fi
}

write_section "Chunks" "${chunks[@]}"
write_section "Viewers" "${filtered_viewers[@]}"
if [ ${#other[@]} -gt 0 ]; then
  write_section "Other" "${other[@]}"
fi

cat >> "$OUT" <<'FOOTER'
  <footer>Auto-generated index &mdash; updated on each push.</footer>
</body>
</html>
FOOTER

total=$(( ${#chunks[@]} + ${#filtered_viewers[@]} + ${#other[@]} ))
echo "Generated $OUT with $total entries (${#chunks[@]} chunks, ${#filtered_viewers[@]} viewers)."
