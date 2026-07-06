# Fit a Generalized Linear Model (GLM) with pooling via Study Level Meta-Analysis (SLMA)

Fits a generalized linear model (GLM) on data from single or multiple
sources with pooled co-analysis across studies being based on SLMA
(Study Level Meta Analysis).

## Usage

``` r
glmSLMADS2(formula, family, offset, weights, newobj, dataName)
```

## Arguments

- formula:

  a glm formula, specified in call to ds.glmSLMA

- family:

  a glm family, specified in call to ds.glmSLMA

- offset:

  a character string specifying a variable to be used as an offset.
  Specified in call to ds.glmSLMA.

- weights:

  a character string specifying a variable to be used as regression
  weights. Specified in call to ds.glmSLMA. Specified in call to
  ds.glmSLMA.

- newobj:

  a character string specifying the name of the glm object written to
  the serverside by glmSLMADS.assign. This is either the name specified
  by the newobj argument in ds.glmSLMA or if newobj was unspecified or
  NULL it is called new.glm.obj.

- dataName:

  a character string specifying the name of a data.frame holding the
  data for the model. Specified in call to ds.glmSLMA.

## Value

All quantitative, Boolean, and character objects required to enable the
SLMA pooling of the separate glm models fitted to each study - in
particular including the study-specific regression coefficients and
their corresponding standard errors.

## Details

glmSLMADS.assign is an aggregate function called by clientside function
ds.glmSLMA. ds.glmSLMA also calls another aggregate function glmSLMADS2
and an assign function glmSLMADS.assign For more detailed information
see help for ds.glmSLMA.

## Author

Paul Burton for DataSHIELD Development Team (14/7/20)
