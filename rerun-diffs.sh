#!/usr/bin/env sh

# this script will take all of the diffs in `./old-diffs/`, and re-run them into `./diffs/`

[ -z "${CHAINWEB_DB_DIR}" ] &&
    echo "CHAINWEB_DB_DIR must be set to the mainnet db directory" && exit 1

# do not even re-run discreps in these classes. maybe they're too large, maybe they're intentional semantic divergences
unchecked_classes="gas-exceeded keyset-failure no-such-member loaded-module-hash minimum-precision lago-finance expected-bool-got-unit capability-already-installed output-differs-int-double cap-is-not-managed kadena-mining-club cannot-find-module no-pact-exec-in-cr interface-loaded keyset-defined desugar-syntax-failure invalid-call-to-if db-internal-error b64-diffs invalid-def-in-term-var marmalade-mints marmalade-v2 too-many-arguments read-keyset-error read-function-failures interface-impl-errors list-commas interface-as-mod-ref nft-mint-mystery module-admin sort-object-divergence incompatible-types"
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
