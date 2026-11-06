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

the vector of pseudorandom non-negative integers from a Poisson
distribution, which is written to the serverside as the object named by
the \<newobj\> argument of ds.rPois.

## Details

Generates the vector of pseudorandom numbers (non-negative integers)
from a Poisson distribution in each data source as specified by the
arguments of ds.rPois. This serverside function is effectively the same
as the function rpois() in native R and its arguments are the same.

## Author

Paul Burton for DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
