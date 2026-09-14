# Computes the WHO Growth Reference z-scores of anthropometric data

Calculate WHO Growth Reference z-score for a given anthropometric
measurement This function is similar to R function `getWGSR` from the
`zscorer` package.

## Usage

``` r
getWGSRDS(sex, firstPart, secondPart, index, standing = NA, thirdPart = NA)
```

## Arguments

- sex:

  the name of the binary variable that indicates the sex of the subject.
  This must be coded as 1 = male and 2 = female. If in your project the
  variable sex has different levels, you should recode the levels to 1
  for males and 2 for females using the `ds.recodeValues` DataSHIELD
  function before the use of the `ds.getWGSR`.

- firstPart:

  Name of variable specifying:  
  Weight (kg) for BMI/A, W/A, W/H, or W/L  
  Head circumference (cm) for HC/A  
  Height (cm) for H/A  
  Length (cm) for L/A  
  MUAC (cm) for MUAC/A  
  Sub-scapular skinfold (mm) for SSF/A  
  Triceps skinfold (mm) for TSF/A  
  Give a quoted variable name as in (e.g.) "weight". Be careful with
  units (weight in kg; height, length, head circumference, and MUAC in
  cm, skinfolds in mm).

- secondPart:

  Name of variable specifying:  
  Age (days) for H/A, HC/A, L/A, MUAC/A, SSF/A, or TSF/A  
  Height (cm) for BMI/A, or W/H  
  Length (cm) for W/L  
  Give a quoted variable name as in (e.g.) "age". Be careful with units
  (age in days; height and length in cm).

- index:

  The index to be calculated and added to data. One of:  
  bfa BMI for age  
  hca Head circumference for age  
  hfa Height for age  
  lfa Length for age  
  mfa MUAC for age  
  ssa Sub-scapular skinfold for age  
  tsa Triceps skinfold for age  
  wfa Weight for age  
  wfh Weight for height  
  wfl Weight for length  
  Give a quoted index name as in (e.g.) "wfh".

- standing:

  Variable specifying how stature was measured. If NA (default) then age
  (for "hfa" or "lfa") or height rules (for "wfh" or "wfl") will be
  applied. This must be coded as 1 = Standing; 2 = Supine; 3 = Unknown.
  Missing values will be recoded to 3 = Unknown. Give a single value
  (e.g."1"). If no value is specified then height and age rules will be
  applied.

- thirdPart:

  Name of variable specifying age (in days) for BMI/A. Give a quoted
  variable name as in (e.g.) "age". Be careful with units (age in days).

## Value

`ds.getWGSR` assigns a numeric vector that includes the z-scores for the
specified index.

## Details

The function computes the WHO Growth Reference z-scores of
anthropometric data for weight, height or length, MUAC, head
circumference, sub-scapular skinfold and triceps skinfold. Note that the
function might fail or return NAs when the variables are outside the
ranges given in the WGS (WHO Child Growth Standards) reference (i.e. 45
to 120 cm for height and 0 to 60 months for age). It is up to the user
to check the ranges and the units of their data.

## Author

Demetris Avraam for DataSHIELD Development Team
