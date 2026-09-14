# Basis for a piecewise linear spline with meaningful coefficients

This function is based on the native R function `lspline` from the
`lspline` package. This function computes the basis of piecewise-linear
spline such that, depending on the argument marginal, the coefficients
can be interpreted as (1) slopes of consecutive spline segments, or (2)
slope change at consecutive knots.

## Usage

``` r
lsplineDS(x = x, knots = NULL, marginal = FALSE, names = NULL)
```

## Arguments

- x:

  the name of the input numeric variable

- knots:

  numeric vector of knot positions

- marginal:

  logical, how to parametrize the spline, see Details

- names, :

  character, vector of names for constructed variables

## Value

an object of class "lspline" and "matrix", which its name is specified
by the `newobj` argument (or its default name "lspline.newobj"), is
assigned on the serverside.

## Details

If marginal is FALSE (default) the coefficients of the spline correspond
to slopes of the consecutive segments. If it is TRUE the first
coefficient correspond to the slope of the first segment. The
consecutive coefficients correspond to the change in slope as compared
to the previous segment.

## Author

Demetris Avraam for DataSHIELD Development Team
