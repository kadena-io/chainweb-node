#!/usr/bin/env sh

# this script will take all of the diffs in `./old-diffs/`, and re-run them into `./diffs/`

[ -z "${CHAINWEB_DB_DIR}" ] &&
    echo "CHAINWEB_DB_DIR must be set to the mainnet db directory" && exit 1

# do not even re-run discreps in these classes.
# maybe they're too large, maybe they're intentional semantic divergences
unchecked_classes=$(cat unchecked-diff-classes)
# this builds a find-invocation that ignores those classes
prune_stmt=$(for cls in $unchecked_classes; do echo -n "-not ( -path old-diffs/$cls -prune ) "; done)

cabal build chainweb-node

# we can't use parallelism here because of a mysterious SQLITE_BUSY when we do
# the unquoted $prune_stmt is intentional
echo "Rerunning #diffs $(find old-diffs/ $prune_stmt -type f | wc -l)"
find old-diffs/ $prune_stmt -type f -print0 | parallel -j1 -0 ./rerun-diff {}
./classify-diffs
for f in $unchecked_classes; do
    rm -rf "diffs/$f"
done
rm -rf /tmp/rerun-diff*
