# predictDS

Generates server-side predictions using the client-side output from
`ds.glm`.

## Usage

``` r
predictDS(
  newdataname,
  traindataname,
  type = c("response", "link"),
  na.action = "na.pass"
)
```

## Arguments

- newdataname:

  A character string specifying the name of the new dataset to be used
  for predictions.

- traindataname:

  A character string specifying the name of the dataset used for model
  training.

- type:

  A character string specifying the type of prediction. Options are
  `"response"` or `"link"`.

- na.action:

  A character string to specify the action to take if missing values are
  present. Default is `"na.pass"`.

## Value

a numeric vector containing the predicted values

## Details

This function uses the components supplied by the client-side function
(coefficients, family, formula, and any categorical variables) to
generate predictions on the server. To use the base R
[`predict()`](https://rdrr.io/r/stats/predict.html) function, a "dummy"
glm object is created using the same model formula, family, and link
function as the original model. The dummy model's coefficients are then
replaced with the client-side coefficient estimates.

To avoid mismatches between the factors used in the original glm and
those in the dummy glm, the categorical variables saved by the
client-side function are applied to the newdata.

For intercept-only models, the function simply returns a vector of
predicted values equal to the model intercept, with the appropriate
length based on the row length of `newdataname`.

## Author

Zulal Bekerecioglu
