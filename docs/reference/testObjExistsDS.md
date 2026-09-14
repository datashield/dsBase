# testObjExistsDS

The server-side function called by ds.testObjExists

## Usage

``` r
testObjExistsDS(test.obj.name = NULL)
```

## Arguments

- test.obj.name:

  a client-side provided character string specifying the variable whose
  presence is to be tested in each data source

## Value

List with \`test.obj.exists\` and \`test.obj.class\`

## Details

Tests whether a given object exists in all sources. It is called at the
end of all recently written assign functions to check the new (assigned)
object has been created in all sources

## Author

Burton PR
