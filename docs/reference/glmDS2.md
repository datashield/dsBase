# glmDS2 called by ds.glm

This is the second server-side aggregate function called by ds.glm.

## Usage

``` r
glmDS2(formula, family, beta.vect, offset, weights, dataName)
```

## Arguments

- formula:

  a glm() formula consistent with R syntax eg U~x+y+Z to regress
  variables U on x, y and Z

- family:

  a glm() family consistent with R syntax eg "gaussian", "poisson",
  "binomial"

- beta.vect:

  a numeric vector created by the clientside function specifying the
  vector of regression coefficients at the current iteration

- offset:

  an optional variable providing a regression offset

- weights:

  an optional variable providing regression weights

- dataName:

  an optional character string specifying a data.frame object holding
  the data to be analysed under the specified model same

## Value

List with values from GLM model

## Details

It is an aggregate function that uses the model structure and starting
beta.vector constructed by glmDS1 to iteratively fit the generalized
linear model that has been specified. The function glmDS2 also carries
out a series of disclosure checks and if the arguments or data fail any
of those tests, model construction is blocked and an appropriate
serverside error message is created and returned to ds.glm on the
clientside. For more details please see the extensive header for ds.glm.

## Author

Paul Burton, for DataSHIELD Development Team
