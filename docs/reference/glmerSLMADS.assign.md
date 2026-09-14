# Fitting generalized linear mixed effect models - serverside function

glmerSLMADS.assign is the same as glmerSLMADS2 which fits a generalized
linear mixed effects model (glme) per study and saves the outcomes in
each study

## Usage

``` r
glmerSLMADS.assign(
  formula,
  offset,
  weights,
  dataName,
  family,
  control_type = NULL,
  control_value.transmit = NULL,
  nAGQ = 1L,
  verbose = 0,
  theta = NULL,
  fixef = NULL
)
```

## Arguments

- formula:

  see help for ds.glmerSLMA

- offset:

  see help for ds.glmerSLMA

- weights:

  see help for ds.glmerSLMA

- dataName:

  see help for ds.glmerSLMA

- family:

  see help for ds.glmerSLMA

- control_type:

  see help for ds.glmerSLMA

- control_value.transmit:

  see help for argument \<control_value\> for function ds.glmerSLMA

- nAGQ:

  integer scalar, defaulting to 1L. IN PRACTICE, IT MAY BE NECESSARY TO
  SET nAGQ TO 0L when the model appears to converge perfectly well (e.g.
  verbose=2 demonstrates good initial convergence of both the
  log-likelihood and regression coefficients) but formal convergence
  does not get declared - so no output is produced - despite running the
  model for many iterations. The nAGQ argument is set by the nAGQ
  argument for ds.glmerSLMA and further details can be found in
  help(ds.glmerSLMA) and in the native R help for glmer()

- verbose:

  see help for ds.glmerSLMA

- theta:

  see help for argument \<start_theta\> for function ds.glmerSLMA

- fixef:

  see help for argument \<start_fixef\> for function ds.glmerSLMA

## Value

writes glmerMod object summarising the fitted model to the serverside.
For more detailed information see help for ds.glmerSLMA.

## Details

glmerSLMADS.assign is a serverside function called by ds.glmerSLMA on
the clientside. The analytic work engine is the glmer function in R
which sits in the lme4 package. glmerSLMADS.assign fits a generalized
linear mixed effects model (glme) - e.g. a logistic or Poisson
regression model including both fixed and random effects - on data from
each single data source and saves the regression outcomes on the
serverside.

## Author

Demetris Avraam for DataSHIELD Development Team
