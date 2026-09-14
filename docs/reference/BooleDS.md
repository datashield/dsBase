# BooleDS

Converts the individual elements of a vector or other object into
Boolean indicators.

## Usage

``` r
BooleDS(
  V1.name = NULL,
  V2.name = NULL,
  Boolean.operator.n = NULL,
  na.assign.text,
  numeric.output = TRUE
)
```

## Arguments

- V1.name:

  A character string specifying the name of the vector to which the
  Boolean operator is to be applied

- V2.name:

  A character string specifying the name of the vector or scalar to
  which \<V1\> is to be compared.

- Boolean.operator.n:

  An integer value (1 to 6) providing a numeric coding for the character
  string specifying one of six possible Boolean operators: '==', '!=',
  '\>', '\>=','\<', '\<=' that could legally be passed from client to
  server via DataSHIELD parser

- na.assign.text:

  A character string taking values 'NA', '1' or '0'. If 'NA' then any NA
  values in the input vector remain as NAs in the output vector. If '1'
  or '0' NA values in the input vector are all converted to 1 or 0
  respectively.

- numeric.output:

  a TRUE/FALSE indicator defaulting to TRUE determining whether the
  final output variable should be of class numeric (1/0) or class
  logical (TRUE/FALSE).

## Value

the levels of the input variable.

## Details

The function converts the input vector into Boolean indicators.

## Author

DataSHIELD Development Team
