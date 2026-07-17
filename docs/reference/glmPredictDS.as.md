# predict regression responses from a glm object

create a predict_glm object on the serverside by applying the equivalent
of predict.glm() in native R to a glm object on the serverside. Identify
and return components of the predict_glm object that can safely be sent
to the clientside without a risk of disclosure

## Usage

``` r
glmPredictDS.as(
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

glmPredict.as writes a new object to the serverside containing output
precisely equivalent to the output from predict.glm in native R. For
more details see DataSHIELD help for ds.glmPredict and help for
predict.glm in native R

## Details

Serverside assign function called by ds.glmPredict makes predictions of
regression responses based on a serverside glm object that has already
been created on the serverside by ds.glmSLMA and and writes the
predict_glm object to the serverside. For further details see help for
ds.glmPredict and help in native R for predict.glm

## Author

Paul Burton for DataSHIELD Development Team (20/7/20)
