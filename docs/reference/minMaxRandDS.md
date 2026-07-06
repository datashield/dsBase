# Secure ranking of "V2BR" (vector to be ranked) across all sources

Creates a minimum value that is more negative, and less positive than
any real value in V2BR and a maximum value that is more positive and
less negative than any value of V2BR.

## Usage

``` r
minMaxRandDS(input.var.name)
```

## Arguments

- input.var.name:

  a character string specifying the name of V2BR. This argument is set
  by the argument with the same name in the clientside function
  ds.ranksSecure

## Value

the data frame objects containing the global ranks and quantiles. For
more details see the associated document entitled
"secure.global.ranking.docx"

## Details

Severside aggregate function called by ds.ranksSecure. The minimum and
maximum values it creates are used to replace missing values (NAs) in
V2BR if the argument \<NA.manag\>e is set to "NA.low" or "NA.hi"
respectively. For more details about the cluster of functions that
collectively enable secure global ranking and estimation of global
quantiles see the associated document entitled
"secure.global.ranking.docx". Also see the header file for
ds.ranksSecure

## Author

Paul Burton 9th November, 2021
