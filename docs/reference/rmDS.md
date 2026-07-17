# rmDS an aggregate function called by ds.rm

deletes an R object on the serverside

## Usage

``` r
rmDS(x.names.transmit)
```

## Arguments

- x.names.transmit, :

  the names of the objects to be deleted converted into transmissible
  form, a comma separated list of character string. The argument is
  specified via the \<x.names\> argument of ds.rm

## Value

the specified object is deleted from the serverside. If this is
successful the message "Object \<x.names\> successfully deleted" is
returned to the clientside (where x.names are the names of the object to
be deleted). If the objects to be deleted is already absent on a given
source, that source will return the message: "Object to be deleted, i.e.
\<x.names\>, does not exist so does not need deleting".

## Details

this is a serverside function based on the rm() function in native R. It
is an aggregate function which may be surprising because it modifies an
object on the serverside, and would therefore be expected to be an
assign function. However, as an assign function the last step in running
it would be to write the modified object as newobj. But this would fail
because the effect of the function is to delete the object and so it
would be impossible to write it anywhere.

## Author

Paul Burton for DataSHIELD Development Team
