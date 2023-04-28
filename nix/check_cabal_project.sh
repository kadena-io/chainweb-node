#!/bin/bash

while read -r line; do
    case $line in
        "source-repository-package")
            unset location tag sha256
            ;;
        location*)
            location="${line#*: }"
            ;;
        tag*)
            tag="${line#*: }"
            ;;
        --sha256*)
            sha256="${line#*: }"
            if [[ -n "$location" && -n "$tag" && -n "$sha256" ]]; then
                computed_sha256=$(nix-prefetch-git --url "$location" --rev "$tag" --quiet | jq '.sha256' -r)
                if [[ "$computed_sha256" != "$sha256" ]]; then
                    echo "Error: Mismatched sha256 for $location@$tag"
                    echo "Expected: $sha256"
                    echo "Actual: $computed_sha256"
                    exit 1
                fi
            fi
            ;;
    esac
done < "${CABAL_PROJECT_PATH:? CABAL_PROJECT_PATH is not set}"

echo "All sha256 values match."
