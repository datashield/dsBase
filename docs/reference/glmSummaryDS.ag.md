# summarize a glm object on the serverside

returns the non-disclosive elements to the clientside of a glm object
and the corresponding object holding the output of summary(glm object)
on the serverside.

## Usage

``` r
glmSummaryDS.ag(x.transmit)
```

## Arguments

- x.transmit:

  a character string specifying the name of the glm object on the
  serverside that is to be summarised. This is specified by x.name
  argument in ds.glmSummary

## Value

returns to the clientside all of the non-disclosive elements (and only
the non-disclosive elements) of a specified serverside glm and its
corresponding summary_glm object.

## Details

Serverside aggregate function called by ds.glmSummary. ds.glmSummary
first calls glmSummaryDS.ag to create a glm_summary object on the
serverside based on applying native R's summary.glm() to a serverside
glm object previously created by ds.glmSLMA. Then it calls
glmSummaryDS.ag to return to the clientside all of the non-disclosive
elements (and only the non-disclosive elements) of the serverside glm
and its corresponding summary_glm object.

## Author

Paul Burton for DataSHIELD Development Team (20/7/20)
