# summarize a glm object on the serverside

summarize a glm object on the serverside to create a summary_glm object.
Also identify and return components of both the glm object and the
summary_glm object that can safely be sent to the clientside without a
risk of disclosure

## Usage

``` r
glmSummaryDS.as(x.transmit)
```

## Arguments

- x.transmit:

  a character string specifying the name of the glm object on the
  serverside that is to be summarised. This is specified by x.name
  argument in ds.glmSummary

## Value

writes object to serverside which is precisely equivalent to summary(glm
object) in native R

## Details

Serverside assign function called by ds.glmSummary summarises a glm
object that has already been created on the serverside by fitting
ds.glmSLMA and writes the summary_glm object to the serverside. For
further details see help for ds.glmSLMA and help in native R for glm()
and summary.glm

## Author

Paul Burton for DataSHIELD Development Team (20/7/20)
