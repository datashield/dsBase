# rBinomDS serverside assign function

primary serverside assign function called by ds.rBinom

## Usage

``` r
rBinomDS(n, size = 1, prob = 0.5)
```

## Arguments

- n:

  length of the pseudorandom number vector to be generated as specified
  by the argument \<samp.size\> in the function ds.rBinom

- size:

  a scalar that must be a positive integer. Value set directly by
  \<size\> argument of ds.rBinom - for details see help for ds.rBinom.
  May be a scalar or a vector allowing the size to vary from observation
  to observation.

- prob:

  a numeric scalar in range 0 \> prob \> 1 which specifies the
  probability of a positive response. Value set directly by \<prob\>
  argument of ds.rBinom - for details see help for ds.rBinom May be a
  scalar or a vector allowing the size to vary from observation to
  observation.

## Value

the vector of pseudorandom numbers from a binomial distribution, which
is written to the serverside as the object named by the \<newobj\>
argument of ds.rBinom.

## Details

Generates the vector of pseudorandom numbers from a binomial
distribution in each data source as specified by the arguments of
ds.rBinom. This serverside function is effectively the same as the
function rbinom() in native R and its arguments are the same.

## Author

Paul Burton for DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
