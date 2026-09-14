# Fit a Generalized Linear Model (GLM) with pooling via Study Level Meta-Analysis (SLMA)

Fits a generalized linear model (GLM) on data from single or multiple
sources with pooled co-analysis across studies being based on SLMA
(Study Level Meta Analysis).

## Usage

``` r
glmSLMADS1(formula, family, weights, offset, data)
```

## Arguments

- formula:

  a glm formula, specified in call to ds.glmSLMA

- family:

  a glm family, specified in call to ds.glmSLMA

- weights:

  a character string specifying a variable to be used as regression
  weights. Specified in call to ds.glmSLMA. Specified in call to
  ds.glmSLMA.

- offset:

  a character string specifying a variable to be used as an offset.
  Specified in call to ds.glmSLMA.

- data:

  a character string specifying the name of a data.frame holding the
  data for the model. Specified as dataName in call to ds.glmSLMA.

## Value

assesses and returns information about failure to pass disclosure traps
such as test of model complexity (saturation). For more detailed
information see help for ds.glmSLMA.

## Details

glmSLMADS.assign is an aggregate function called by clientside function
ds.glmSLMA. ds.glmSLMA also calls another aggregate function glmSLMADS2
and an assign function glmSLMADS.assign For more detailed information
see help for ds.glmSLMA.

## Author

Paul Burton for DataSHIELD Development Team (14/7/20)
