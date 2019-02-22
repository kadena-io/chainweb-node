# Future Work

In the drive for TestNet, certain sacrifices are made as we work. This file
tracks ideas for future work, clean up, performance improvements, etc., that can
be considered once a TestNet-ready Chainweb has been achieved.

## Git Store

### Performance

#### Cache certain disk lookups

Consider caching the lookups made over shared calls to `seekHighest`.
`seekHighest` is still very fast (`O(logn)`), but for very long chains this
could show a performance improvement for `entries` calls.

#### Efficient encoding for tag filenames

Currently, tag filenames take the following shape:

```
0000000000000020.Hc-ixI8lSPr39uzlsO_qbFv52veiJhlL_ozLY1F_2hY
0000000000000040.8lqxArVy0fw_mv5LPv94Qrek0DT8zL5miH_LF9mCvRM
0000000000000080.H_hTtC-oj8KpiY5weioJj6Tkn4pI-6EnIvwelfrQWO8
0000000000000100.PwntUkxmI2pTGlI0N9Gdcwlhb94WSdO9HY_CQXtyaoU
...
```

where the value before the `.` is a fixed-width Hex encoding of `BlockHeight`.
The value after the `.` is a base64 URL-safe encoding of `BlockHashBytes`. This
format ensures that:

1. git will store these filenames in the correct order (lowest-to-highest)
   within its "tree entries"
2. `BlockHashBytes` can encoded in a way that git considers "legal" for
   filenames

Unfortunately, the leading zeros consume bytes on disk that may not be
necessary. Some encoding other than Hex may be better for this.

Futhermore, during tag lookups (which we do often), matched tags must have the
`BlockHashBytes` portion of the filename decoded in order to become useful
again. The target hash must also be reencoded once to even ensure a match in the
first place. Suffice to say, a lot of redundant encoding/decoding occurs. Both
lookups and traversal would receive a substantial performance improvement (50%?)
if `BlockHashBytes` were pre-encoded in their URL-safe forms, as lookups here
could then occur without any extra encoding/decoding.

#### Update `hlibgit2` bindings

[`hlibgit2`](https://hackage.haskell.org/package/hlibgit2) binds to version
`0.18.0` of `libgit2`, which was released in mid-2013. Since then, the following
improvements (which are relevant to our use-case) have been made:

| Version | Date     | Improvements                            |
|---------|----------|-----------------------------------------|
|  0.19.0 | Jul 2013 | Bug fixes, perf improvements            |
|  0.20.0 | Nov 2013 | Perf improvements                       |
|  0.21.2 | Oct 2014 | Fixed mem leak in `git_tag_delete()`    |
|  0.22.0 | Jan 2015 | Changes to names of functions we use    |
|  0.24.0 | Mar 2016 | Tree objects assumed sorted (perf ++)   |
|  0.24.1 | Apr 2016 | Change to structure of `git_tree` type  |
|  0.27.3 | Jul 2018 | Fix out-of-bounds reads from a packfile |

`0.24.0` in particular may give us a nice improvement "for free".

#### Cache leaves

The complexity of `leaves` (and therefore `leafEntries`) is "`O(n)`-ish", in
that, due to a detail of the `libgit2` library, the number of tags in `bh/`
affects the speed at which we can read the tags in `leaf/`. Ideally, fetching
leaves would be `O(1)`. To restore this, a `Map` of the current leaves could be
kept in memory (say within `GitStoreData`), where it would be updated as leaves
were added to or deleted from `leaf/`. `leaves` then becomes a `toList` of the
`Map`, which would speed up a few operations elsewhere that depend on it.

#### Custom `branchEntries`

Using `walk` and `seekHighest`, a faster implementation of `branchEntries` could
probably be written. Currently we rely on the default provided by `TreeDb`,
which experimentally has been shown to be "fast enough".

### Correctness

#### Expand `prune`

Currently `prune` will perform a `git repack -A` and `git gc`, but does not
manually clean out old tags in `bh/` or `leaf/`.

See [this
discussion](https://github.com/kadena-io/chainweb/pull/201#discussion_r252262465)
for more information.

## Pact Execution Service

### Catching Exceptions

This paragraph only concerns the proper execution schedule of the
on-disk check-pointer. Figure out how to catch exceptions properly in
monad transformer stacks that have IO at the bottom. Upon receipt (or
catch) of an exception during the execution of pact code, make sure to
close any back-end database, currently in use, properly.
