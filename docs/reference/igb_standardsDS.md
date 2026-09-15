# Converts birth measurements to intergrowth z-scores/centiles

Converts birth measurements to INTERGROWTH z-scores/centiles (generic)

## Usage

``` r
igb_standardsDS(
  gagebrth = gagebrth,
  z = z,
  p = p,
  val = val,
  var = var,
  sex = sex,
  fun = fun
)
```

## Arguments

- gagebrth:

  the name of the "gestational age at birth in days" variable.

- z:

  z-score(s) to convert (must be between 0 and 1). Default value is 0.
  This value is used only if `fun` is set to "igb_zscore2value".

- p:

  centile(s) to convert (must be between 0 and 100). Default value is
  p=50. This value is used only if `fun` is set to "igb_centile2value".

- val:

  the name of the anthropometric variable to convert.

- var:

  the name of the measurement to convert ("lencm", "wtkg", "hcircm",
  "wlr")

- sex:

  the name of the sex factor variable. The variable should be coded as
  Male/Female. If it is coded differently (e.g. 0/1), then you can use
  the ds.recodeValues function to recode the categories to Male/Female
  before the use of ds.igb_standards

- fun:

  the name of the function to be used. This can be one of:
  "igb_centile2value", "igb_zscore2value", "igb_value2zscore" (default),
  "igb_value2centile".

## Value

assigns the converted measurement as a new object on the server-side

## Note

For gestational ages between 24 and 33 weeks, the INTERGROWTH very early
preterm standard is used.

## Author

Demetris Avraam for DataSHIELD Development Team
