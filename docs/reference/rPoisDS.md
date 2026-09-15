# rPoisDS serverside assign function

primary serverside assign function called by ds.rPois

## Usage

``` r
rPoisDS(n, lambda = 1)
```

## Arguments

- n:

  length of the pseudorandom number vector to be generated as specified
  by the argument \<samp.size\> in the function ds.rPois

- lambda:

  a numeric scalar specifying the expected count of the Poisson
  distribution used to generate the random counts. Specified directly by
  the lambda argument in ds.rPois. May be a scalar or a vector allowing
  lambda to vary from observation to observation.

## Value

Writes the pseudorandom number vector with the characteristics specified
in the function call as a new serverside vector on the data source on
which it has been called. Also returns key information to the
clientside: the random seed as specified by you in each source + (if
requested) the full 626 length random seed vector this generated in each
source (see info for the argument \<return.full.seed.as.set\>). It also
returns a vector reporting the length of the pseudorandom vector created
in each source.

## Details

Generates the vector of pseudorandom numbers (non-negative integers)
from a Poisson distribution in each data source as specified by the
arguments of ds.rPois. This serverside function is effectively the same
as the function rpois() in native R and its arguments are the same.

## Author

Paul Burton for DataSHIELD Development Team
