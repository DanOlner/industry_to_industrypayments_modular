#!/usr/bin/env bash
# Update CITATION.cff and .zenodo.json with version, date, and Zenodo DOI.
# Called by the update-citation-on-release GitHub Action.
#
# Usage: update-citation-metadata.sh <tag>
#   e.g. update-citation-metadata.sh v0.2.0

set -euo pipefail

TAG="${1:?Usage: update-citation-metadata.sh <tag>}"
VERSION="${TAG#v}"  # strip leading 'v' if present
TODAY=$(date +%Y-%m-%d)

echo "Updating metadata: version=$VERSION date=$TODAY"

# --- Update CITATION.cff ---
sed -i "s/^version: .*/version: ${VERSION}/" CITATION.cff
sed -i "s/^date-released: .*/date-released: \"${TODAY}\"/" CITATION.cff

# --- Update .zenodo.json ---
# Use a temp file for jq (in-place not supported)
jq --arg v "$VERSION" '.version = $v' .zenodo.json > .zenodo.json.tmp
mv .zenodo.json.tmp .zenodo.json

# --- Fetch DOI from Zenodo API ---
# Zenodo needs time to process the release. Poll up to ~5 minutes.
REPO="${GITHUB_REPO:?GITHUB_REPO not set}"
ZENODO_API="https://zenodo.org/api/records"
MAX_ATTEMPTS=15
SLEEP_SECONDS=60

echo "Polling Zenodo for DOI (repo: ${REPO})..."

DOI=""
for i in $(seq 1 $MAX_ATTEMPTS); do
    echo "  Attempt ${i}/${MAX_ATTEMPTS}..."

    # Search Zenodo for records linked to this GitHub repo
    RESPONSE=$(curl -s "${ZENODO_API}?q=github.com/${REPO}&sort=mostrecent&size=1" || true)
    CANDIDATE=$(echo "$RESPONSE" | jq -r '.hits.hits[0].doi // empty' 2>/dev/null || true)

    if [ -n "$CANDIDATE" ]; then
        DOI="$CANDIDATE"
        echo "  Found DOI: ${DOI}"
        break
    fi

    if [ "$i" -lt "$MAX_ATTEMPTS" ]; then
        echo "  Not found yet, waiting ${SLEEP_SECONDS}s..."
        sleep "$SLEEP_SECONDS"
    fi
done

if [ -z "$DOI" ]; then
    echo "WARNING: Could not fetch DOI from Zenodo after ${MAX_ATTEMPTS} attempts."
    echo "You may need to add the DOI manually after Zenodo finishes processing."
    exit 0  # Don't fail the workflow — version/date are still updated
fi

# Add DOI to CITATION.cff (add or update the doi field)
if grep -q "^doi:" CITATION.cff; then
    sed -i "s|^doi: .*|doi: \"${DOI}\"|" CITATION.cff
else
    # Insert doi after the version line
    sed -i "/^version: /a doi: \"${DOI}\"" CITATION.cff
fi

# Add DOI to .zenodo.json
jq --arg doi "$DOI" '.doi = $doi' .zenodo.json > .zenodo.json.tmp
mv .zenodo.json.tmp .zenodo.json

echo "Done. Files updated with DOI: ${DOI}"
