# glmDS1 called by ds.glm

This is the first server-side aggregate function called by ds.glm

## Usage

``` r
glmDS1(formula, family, weights, offset, data)
```

## Arguments

- formula:

  a glm() formula consistent with R syntax eg U~x+y+Z to regress
  variables U on x,y and Z

- family:

  a glm() family consistent with R syntax eg "gaussian", "poisson",
  "binomial"

- weights:

  an optional variable providing regression weights

- offset:

  the offset

- data:

  an optional character string specifying a data.frame object holding
  the data to be analysed under the specified model

## Value

List with values from GLM model.

## Details

It is an aggregation function that sets up the model structure and
creates the starting beta.vector that feeds, via ds.glm, into glmDS2 to
enable iterative fitting of the generalized linear model that has been
been specified. For more details please see the extensive header for
ds.glm.

## Author

Burton PR for DataSHIELD Development Team
