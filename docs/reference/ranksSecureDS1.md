# Secure ranking of "V2BR" (vector to be ranked) across all sources

takes key non-disclosive components of the serverside data frame
blackbox.output.df over to the clientside to enable global ranking.

## Usage

``` r
ranksSecureDS1()
```

## Value

the non-disclosive elements of blackbox.output.df (see details) on the
serverside as a data frame object (called blackbox.output) on the
clientside. After processing to create the global ranks across all
studies, this is returned to the serverside as the data frame sR4.df
using the clientside function ds.dmtC2S

## Details

Severside aggregate function called by ds.ranksSecure. The
non-disclosive components of blackbox.output.df that are transmitted to
the clientside are: (1) final values of the "combined real+pseudo data
vector" after all seven rounds of encryption have been completed; (2) a
set of sequential IDs allocated after sorting the "combined real+pseudo
data vector" by value (in ascending order). This allows later re-linkage
of values back on the serverside and confirmation that that linkage is
correct. For more details about the cluster of functions that
collectively enable secure global ranking and estimation of global
quantiles see the associated document entitled
"secure.global.ranking.docx". Also see the header file for
ds.ranksSecure

## Author

Paul Burton 9th November, 2021
