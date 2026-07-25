# Hierarchical resampling in `bootcoldist()`

**Status: implemented on `bootcoldist_heirarchical`.** Kept for the rationale; the code
below is the sketch, not the final version (see `R/bootcoldist.R` for that).

Design sketch for adding cluster-aware (nested/crossed) bootstrapping. Nothing here changes
the point estimate — only which rows get drawn in each replicate, so CIs change and
`dS.mean`/`dL.mean` do not.

## The problem

`bootcoldist()` currently draws rows i.i.d. within each level of `by`:

```r
its <- lapply(samplesizes, function(x) sample.int(x, x * boot.n, replace = TRUE))
```

That assumes every row is an independent observation. In the sicalis example the rows
are 7 individuals × 3 patches, `by` is the patch, and the three rows contributed by
`ind01` are drawn independently of each other. Two things go wrong:

1. The pairing between patches on the same individual is destroyed, so the CI on
   `C-T` ignores the fact that it is a within-individual comparison.
2. The effective sample size is treated as *rows*, not *individuals*. With repeated
   measures nested inside individuals (say 5 spectra per bird), the interval shrinks
   like `sqrt(n_spectra)` when it should shrink like `sqrt(n_birds)`.

The fix is the nonparametric cluster bootstrap: resample whole clusters with replacement
and carry all of their rows along.

## API

```r
bootcoldist(vismodeldata, by, boot.n = 1000, alpha = 0.95, raw = FALSE, ...,
            cluster = NULL, nesting = c("auto", "crossed", "nested"))
```

- `cluster = NULL` → byte-identical behaviour to today, including the RNG stream
  (the null branch keeps the same `sample.int()` call), so existing tests and any
  seeded user scripts are untouched.
- New args go *after* `...`, so they must be named in full. This is not cosmetic:
  a formal placed before `...` captures any `...` argument it is a prefix-match for,
  and coldist()'s `n` is a prefix of `nesting`, so `n = c(1, 2, 2, 4)` was silently
  bound to the nesting structure. Everything before `...` keeps its position, so
  positional calls (`bootcoldist(vm, gr, 500)`) still work.

Two designs need distinguishing, hence `nesting`:

**Crossed** — clusters span groups. Sicalis: each individual supplies a crown, throat
and breast measurement. One draw of individuals per replicate, shared across all groups,
which preserves the within-individual pairing. Intervals on within-individual contrasts
typically get *narrower*, for the same reason a paired t-test beats an unpaired one.

**Nested** — each cluster sits inside exactly one group. E.g. `by` = population,
cluster = individual, rows = repeated spectra. Draw clusters independently within each
group. Intervals get *wider*, because the naive version was pseudoreplicating.

`"auto"` detects this by checking whether any cluster appears under more than one `by`
level. The override matters for the ugly middle case where clusters are mostly nested but
a couple stray across groups.

## Implementation

Everything downstream of the resampling (`groupcolmeans`, `bootgrouped`, the attribute
copying, `future_lapply`) stays as is. Factor the index generation into one helper that
returns, for each group, a list of `boot.n` integer vectors of row positions *within that
group's block* — which is exactly what `bootbygroup` already consumes.

```r
# Data are assumed pre-sorted by `by` (as they are after `sortinggroups`).
# Returns: list over groups -> list over replicates -> integer row positions.
bootindices <- function(by, cluster = NULL, boot.n, nesting = "auto") {

  groups <- unique(by)

  ## Current behaviour --------------------------------------------------------
  if (is.null(cluster)) {
    ns <- table(by)[groups]
    return(lapply(ns, function(n) {
      split(sample.int(n, n * boot.n, replace = TRUE),
            rep(seq_len(boot.n), each = n))
    }))
  }

  ## Row positions within each group, keyed by cluster id ---------------------
  rows <- lapply(groups, function(g) split(which(by == g) - min(which(by == g)) + 1L,
                                           cluster[by == g]))
  names(rows) <- groups

  if (identical(nesting, "auto")) {
    spread <- vapply(split(as.character(by), cluster),
                     function(x) length(unique(x)) > 1L, logical(1))
    nesting <- if (any(spread)) "crossed" else "nested"
  }

  if (nesting == "crossed") {
    ids  <- unique(as.character(cluster))
    draw <- replicate(boot.n, sample(ids, length(ids), replace = TRUE),
                      simplify = FALSE)
    out <- lapply(groups, function(g) {
      have <- names(rows[[g]])
      lapply(draw, function(d) unlist(rows[[g]][d[d %in% have]], use.names = FALSE))
    })
  } else {
    out <- lapply(groups, function(g) {
      ids <- names(rows[[g]])
      replicate(boot.n,
                unlist(rows[[g]][sample(ids, length(ids), replace = TRUE)],
                       use.names = FALSE),
                simplify = FALSE)
    })
  }

  names(out) <- groups
  out
}
```

Call site: in `bootcoldist()`, immediately after `by <- by[sortinggroups]` add

```r
if (!is.null(cluster)) {
  if (length(cluster) != nrow(vismodeldata)) {
    stop("`cluster` must have one entry per row of `vismodeldata`.", call. = FALSE)
  }
  if (anyNA(cluster)) stop("`cluster` cannot contain missing values.", call. = FALSE)
  cluster <- as.character(cluster)[sortinggroups]
}
```

— reordering `cluster` alongside `by` is the easiest thing to forget and fails silently —
then replace the `its` / `bootsamples` / `bootindex` / `bootbygroup` block with

```r
idx <- bootindices(by, cluster, boot.n, nesting)

bootbygroup <- lapply(seq_along(bygroup), function(g) {
  lapply(idx[[g]], function(i) bygroup[[g]][i, , drop = FALSE])
})
```

That is the whole change. The rest of the function never learns that clustering exists.
As a bonus the new form drops the `as.character()` split, which is where a good chunk of
the current runtime goes for large `boot.n`.

## Edge cases

- **Empty groups.** In a crossed design where a group is measured on only a few clusters,
  a replicate can draw none of them and that group contributes zero rows. Right now it
  would fall through `tryCatch` into the unhelpful "Bootstrap sampling encountered errors".
  Either redraw that replicate (cheap, slightly biased, standard practice) or fail early
  with a message naming the group.
- **Unbalanced clusters.** Group means become cluster-size weighted, since a big cluster
  brings more rows. That matches the empirical estimate, which is what you want for
  consistency between `dS.mean` and the interval.
- **Few clusters.** The effective n is the number of clusters. Below ~10 the percentile
  interval is unreliable; worth a one-off `message()` rather than silence.
- **Singleton clusters.** If every cluster has one row, crossed cluster bootstrap reduces
  to the current behaviour up to the RNG stream. Fine, but no reason for a user to do it.

## Later extensions

- Two-stage bootstrap (`boot.type = "two.stage"`): resample clusters, then resample rows
  within each drawn cluster. Field & Welsh (2007) show it helps when within-cluster sizes
  are large and variable; the one-stage version is consistent for the common case. Slots
  into the same helper as a third branch.
- More than two levels (`cluster` accepting a data frame of nested factors) — the helper
  generalises, but the empty-group logic gets fiddly, so probably not worth it yet.

## Testing

- `cluster = NULL` with a fixed seed reproduces current output exactly.
- Simulated nested data with a large between-cluster and small within-cluster variance:
  cluster CIs should be clearly wider than naive ones, and interval coverage over ~500
  simulated datasets should sit near `alpha` where the naive version undercovers badly.
- Crossed sicalis data (`ind <- substr(rownames(vm), 1, 5)`): check the CI on a
  within-individual contrast narrows, and that `dS.mean` is unchanged from the current
  release.
- Clusters present in one group but absent from another, to hit the `d %in% have` path.
