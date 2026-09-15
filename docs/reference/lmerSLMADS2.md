# Fitting linear mixed effect models - serverside function

lmerSLMADS2 is a serverside function which fits a linear mixed effects
model (lme) - i.e. can include both fixed and random effects - on data
from one or multiple sources with pooling via SLMA (study level
meta-analysis)

## Usage

``` r
lmerSLMADS2(
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

all key model components see help for ds.lmerSLMA

## Details

lmerSLMADS2 is a serverside function called by ds.lmerSLMA on the
clientside. The analytic work engine is the lmer function in R which
sits in the lme4 package. ds.lmerSLMA fits a linear mixed effects model
(lme) - can include both fixed and random effects - on data from a
single or multiple sources. When there are multiple data sources, the
lme is fitted to convergence in each data source independently and the
estimates and standard errors returned to the client thereby enabling
cross-study pooling using study level meta-analysis (SLMA). By default
the SLMA is undertaken using the metafor package, but as the SLMA occurs
on the clientside which, as far as the user is concerned is just a
standard R environment, the user can choose to use any approach to
meta-analysis they choose. For more detailed help about any aspect of
lmerSLMDS2 please see the extensive help for ds.lmerSLMA. Additional
information about fitting lmes using the lmer engine can be obtained
using R help for lmer and the lme4 package

## Author

Tom Bishop, with some additions by Paul Burton
