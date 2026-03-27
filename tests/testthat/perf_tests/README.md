# Performance Tests

Performance tests measure the throughput (operations per second) of server-side functions and compare against baseline rates stored in profile CSV files.

## How it works

Each performance test:

1. Runs a function in a loop for 30 seconds and calculates the current rate (ops/sec).
2. Looks up the baseline rate for that test in the active profile CSV.
3. If no entry exists, a new one is saved to the profile using the current rate and the profile-level default tolerances.
4. Asserts that the current rate falls within `[baseline * lower_tolerance, baseline * upper_tolerance]`.

## Profiles

Profile CSVs live in `perf_files/` and contain columns:

| Column | Description |
|--------|-------------|
| `refer_name` | Unique test identifier (e.g. `meanDS::perf::numeric::0`) |
| `rate` | Baseline ops/sec |
| `lower_tolerance` | Multiplier for the lower bound (e.g. `0.5` = 50% of baseline) |
| `upper_tolerance` | Multiplier for the upper bound (e.g. `2.0` = 200% of baseline) |

Available profiles:

- `default_perf_profile.csv` -- default baseline
- `performance_refactor_profile.csv` -- for local development; no effective upper limit
- `azure-pipeline.csv`, `circleci.csv` -- CI-specific baselines

## Switching profiles

Set `.perf.reference.filename` in `setup.R` before sourcing `perf_rate.R`:

```r
.perf.reference.filename <- "perf_files/performance_refactor_profile.csv"
source("perf_tests/perf_rate.R")
```

If not set, `perf_rate.R` defaults to `perf_files/default_perf_profile.csv`.

## Self-populating entries

When a test has no entry in the active profile, `perf.reference.save()` creates one using the current measured rate and the profile-level tolerances (`perf.profile.tolerance.lower/upper()`), which are read from the first row of the profile CSV. This means new tests automatically inherit the tolerance policy of whichever profile is active.

## Skipping

Performance tests are skipped on CRAN (`skip_on_cran()`) and CI (`skip_on_ci()`) by default, since results are hardware-dependent.
