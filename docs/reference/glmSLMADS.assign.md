# Fit a Generalized Linear Model (GLM) with pooling via Study Level Meta-Analysis (SLMA)

Fits a generalized linear model (GLM) on data from single or multiple
sources with pooled co-analysis across studies being based on SLMA
(Study Level Meta Analysis).

## Usage

``` r
glmSLMADS.assign(formula, family, offsetName, weightsName, dataName)
```

## Arguments

- formula:

  a glm formula, specified in call to ds.glmSLMA

- family:

  a glm family, specified in call to ds.glmSLMA

- offsetName:

  a character string specifying a variable to be used as an offset.
  Specified in call to ds.glmSLMA.

- weightsName:

  a character string specifying a variable to be used as regression
  weights. Specified in call to ds.glmSLMA. Specified in call to
  ds.glmSLMA.

- dataName:

  a character string specifying the name of a data.frame holding the
  data for the model. Specified in call to ds.glmSLMA.

## Value

writes glm object summarising the fitted model to the serverside. For
more detailed information see help for ds.glmSLMA.

## Details

glmSLMADS.assign is an assign function called by clientside function
ds.glmSLMA. ds.glmSLMA also calls two aggregate functions glmSLMADS1 and
glmSLMADS2. For more detailed information see help for ds.glmSLMA.

## Author

Paul Burton for DataSHIELD Development Team (14/7/20)
