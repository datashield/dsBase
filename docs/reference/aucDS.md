# aucDS an aggregate function called by ds.auc

This function calculates the C-statistic or AUC for logistic regression
models.

## Usage

``` r
aucDS(pred = pred, y = y)
```

## Arguments

- pred:

  the name of the vector of the predicted values

- y:

  the name of the outcome variable. Note that this variable should
  include the complete cases that are used in the regression model.

## Value

returns the AUC and its standard error

## Details

The AUC determines the discriminative ability of a model.

## Author

Demetris Avraam for DataSHIELD Development Team
