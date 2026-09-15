# Computes the sum of each variable and the sum of products for each pair of variables

This function computes the sum of each vector of variable and the sum of
the products of each two variables (i.e. the scalar product of each two
vectors).

## Usage

``` r
corDS(x = NULL, y = NULL)
```

## Arguments

- x:

  a character, the name of a vector, matrix or dataframe of variables(s)
  for which the correlation(s) is (are) going to calculated for.

- y:

  NULL (default) or the name of a vector, matrix or dataframe with
  compatible dimensions to x.

## Value

a list that includes a matrix with elements the sum of products between
each two variables, a matrix with elements the sum of the values of each
variable, a matrix with elements the number of complete cases in each
pair of variables, a list with the number of missing values in each
variable separately (columnwise) and the number of missing values
casewise, and a vector with elements the sum of squares of each
variable. The first disclosure control checks that the number of
variables is not bigger than a percentage of the individual-level
records (the allowed percentage is pre-specified by the 'nfilter.glm').
The second disclosure control checks that none of them is dichotomous
with a level having fewer counts than the pre-specified 'nfilter.tab'
threshold.

## Details

computes the sum of each vector of variable and the sum of the products
of each two variables

## Author

Paul Burton, and Demetris Avraam for DataSHIELD Development Team
