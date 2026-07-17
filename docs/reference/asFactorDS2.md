# Converts a numeric vector into a factor

This function is an assign DataSHIELD function that converts a numeric
vector into a factor type that presented as a vector or as a matrix with
dummy variables.

## Usage

``` r
asFactorDS2(
  input.var.name = NULL,
  all.unique.levels.transmit = NULL,
  fixed.dummy.vars = NULL,
  baseline.level = NULL
)
```

## Arguments

- input.var.name:

  the name of the variable that is to be converted to a factor.

- all.unique.levels.transmit:

  the levels that the variable will be transmitted to.

- fixed.dummy.vars:

  a boolean that determines whether the new object will be represented
  as a vector or as a matrix of dummy variables indicating the factor
  level of each data point. If this argument is set to FALSE (default)
  then the input variable is converted to a factor and assigned as a
  vector. If is set to TRUE then the input variable is converted to a
  factor but assigned as a matrix of dummy variables.

- baseline.level:

  a number indicating the baseline level to be used in the creation of
  the matrix of dummy variables.

## Value

an object of class factor

## Details

The functions converts the input variable into a factor which is
presented as a vector if the `fixed.dummy.vars` is set to FALSE or as a
matrix with dummy variables if the `fixed.dummy.vars` is set to TRUE
(see the help file of ds.asFactor.b for more details).
