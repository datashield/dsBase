# Tests for correlation between paired samples

This function is similar to R function `cor.test`.

## Usage

``` r
corTestDS(x, y, method, exact, conf.level)
```

## Arguments

- x:

  a character string providing the name of a numerical vector.

- y:

  a character string providing the name of a numerical vector.

- method:

  a character string indicating which correlation coefficient is to be
  used for the test. One of "pearson", "kendall", or "spearman", can be
  abbreviated.

- exact:

  a logical indicating whether an exact p-value should be computed. Used
  for Kendall's tau and Spearman's rho.

- conf.level:

  confidence level for the returned confidence interval. Currently only
  used for the Pearson product moment correlation coefficient if there
  are at least 4 complete pairs of observations.

## Value

the results of the correlation test.

## Details

The function runs a two-sided correlation test

## Author

Demetris Avraam, for DataSHIELD Development Team
