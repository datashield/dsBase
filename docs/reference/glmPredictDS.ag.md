# predict regression responses from a glm object

identify and return key components/summaries of a serverside glm_predict
object that can safely be returned to the clientside without disclosure
risk

## Usage

``` r
glmPredictDS.ag(
  glmname.transmit,
  newdataname.transmit,
  output.type,
  se.fit,
  dispersion,
  terms.transmit,
  na.action
)
```

## Arguments

- glmname.transmit:

  a character string specifying the name of the glm object on the
  serverside that is to be used for prediction. Fully specified by
  glmname argument in ds.glmPredict

- newdataname.transmit:

  a character string specifying an (optional) dataframe on the
  serverside in which to look for (potentially) new covariate values on
  which to base the predictions. Fully specified by newdataname argument
  in ds.glmPredict.

- output.type:

  a character string taking the values 'response', 'link' or 'terms'.
  Fully specified by corresponding argument in ds.glmPredict.

- se.fit:

  logical if standard errors for the fitted predictions are required.
  Fully specified by corresponding argument in ds.glmPredict.

- dispersion:

  numeric value specifying the dispersion of the GLM fit to be assumed
  in computing the standard errors. Fully specified by corresponding
  argument in ds.glmPredict.

- terms.transmit:

  a character vector specifying a subset of terms to return in the
  prediction. Fully specified by 'terms' argument in ds.glmPredict.

- na.action:

  character string determining what should be done with missing values
  in the data.frame identified by \<newdataname.transmit\>. Fully
  specified by na.action argument in ds.glmPredict.

## Value

components/summarising statistics of a serverside predict_glm object
that can safely be transmitted to the clientside without a risk of
disclosure. For further details see DataSHIELD help for ds.glmPredict
and glmPredict.as and help in native R for predict.glm predict.glm in
native R

## Details

Serverside aggregate function called by ds.glmPredict. It is called
immediately after the assign function glmPredict.as has created a
predict_glm object on the serverside by applying the equivalent of
predict.glm() in native R to a glm object on the serverside. The
aggregate function, glmPredict.ag, then identifies and returns
components of that predict_glm object that can safely be returned to the
clientside without a risk of disclosure. For further details see
DataSHIELD help for ds.glmPredict and glmPredict.as and help in native R
for predict.glm

## Author

Paul Burton for DataSHIELD Development Team (20/7/20)
