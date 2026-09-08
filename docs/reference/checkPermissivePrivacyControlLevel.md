# checkPermissivePrivacyControlLevel

This server-side function check that the server is running in
"permissive" privacy control level.

## Usage

``` r
checkPermissivePrivacyControlLevel(privacyControlLevels)
```

## Arguments

- privacyControlLevels:

  is a vector of strings which contains the privacy control level names
  which are permitted by the calling method.

## Value

No return value, called for side effects

## Details

Tests whether the R option "datashield.privacyControlLevel" is set to
"permissive", if it isn't will cause a call to stop() with the message
"BLOCKED: The server is running in 'non-permissive' mode which has
caused this method to be blocked".

## Author

Wheater, Dr SM., DataSHIELD Development Team.
