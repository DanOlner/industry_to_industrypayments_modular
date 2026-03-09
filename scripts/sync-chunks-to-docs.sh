#!/usr/bin/env bash
# sync-chunks-to-docs.sh — Copy rendered chunk outputs (HTML, PDF, and
# supporting assets) from chunks/ into docs/chunks/ so they're served
# by GitHub Pages.
#
# Each chunk subfolder gets its own directory under docs/chunks/.
# The main HTML is copied as index.html so the existing index generator
# can discover it. The PDF and all asset directories (images, *_files)
# are copied alongside.
#
# Usage: bash scripts/sync-chunks-to-docs.sh

set -euo pipefail

CHUNKS_SRC="chunks"
CHUNKS_DST="docs/chunks"

# Skip the template folder
SKIP="chunk_template"

synced=0

for chunk_dir in "$CHUNKS_SRC"/*/; do
  folder_name="$(basename "$chunk_dir")"

  # Skip template
  [ "$folder_name" = "$SKIP" ] && continue

  # Find rendered HTML files (Quarto output-file, not .qmd source)
  html_files=( "$chunk_dir"*.html )
  if [ ! -e "${html_files[0]}" ]; then
    echo "  Skipping $folder_name — no HTML output found"
    continue
  fi

  dest="$CHUNKS_DST/$folder_name"
  mkdir -p "$dest"

  # Copy the first HTML as index.html (for index generator discovery)
  main_html="${html_files[0]}"
  cp "$main_html" "$dest/index.html"
  echo "  ✓ $folder_name: $(basename "$main_html") → index.html"

  # Copy any PDF files
  for pdf in "$chunk_dir"*.pdf; do
    [ -e "$pdf" ] || continue
    cp "$pdf" "$dest/"
    echo "    + $(basename "$pdf")"
  done

  # Copy asset directories: images/ and any *_files/ dirs (Quarto libs)
  for asset_dir in "$chunk_dir"images "$chunk_dir"*_files; do
    [ -d "$asset_dir" ] || continue
    asset_name="$(basename "$asset_dir")"
    # Use rsync for efficient incremental copies; fall back to cp -r
    if command -v rsync &>/dev/null; then
      rsync -a --delete "$asset_dir/" "$dest/$asset_name/"
    else
      rm -rf "$dest/$asset_name"
      cp -r "$asset_dir" "$dest/$asset_name"
    fi
    echo "    + $asset_name/"
  done

  synced=$((synced + 1))
done

if [ "$synced" -eq 0 ]; then
  echo "No chunks with rendered HTML found."
else
  echo ""
  echo "Synced $synced chunk(s) to $CHUNKS_DST."
  echo "Run 'bash scripts/generate-index.sh' to update the docs index."
fi
