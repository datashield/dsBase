# Fitting linear mixed effect models - serverside function

lmerSLMADS.assign is the same as lmerSLMADS2 which fits a linear mixed
effects model (lme) per study and saves the outcomes in each study

## Usage

``` r
lmerSLMADS.assign(
  formula,
  offset,
  weights,
  dataName,
  REML = TRUE,
  control_type,
  control_value.transmit,
  optimizer,
  verbose = 0
)
```

## Arguments

- formula:

  see help for ds.lmerSLMA

- offset:

  see help for ds.lmerSLMA

- weights:

  see help for ds.lmerSLMA

- dataName:

  see help for ds.lmerSLMA

- REML:

  see help for ds.lmerSLMA

- control_type:

  see help for ds.lmerSLMA

- control_value.transmit:

  see help for argument \<control_value\> for function ds.lmerSLMA

- optimizer:

  see help for ds.lmerSLMA

- verbose:

  see help for ds.lmerSLMA

## Value

writes lmerMod object summarising the fitted model to the serverside.
For more detailed information see help for ds.lmerSLMA.

## Details

lmerSLMADS.assign is a serverside function called by ds.lmerSLMA on the
clientside. The analytic work engine is the lmer function in R which
sits in the lme4 package. lmerSLMADS.assign fits a linear mixed effects
model (lme) including both fixed and random effects - on data from each
single data source and saves the regression outcomes on the serverside.

## Author

TDemetris Avraam for DataSHIELD Development Team
