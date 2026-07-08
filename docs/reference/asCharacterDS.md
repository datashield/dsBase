# Coerces an R object into class character

this function is based on the native R function `as.character`

## Usage

``` r
asCharacterDS(x.name)
```

## Arguments

- x.name:

  the name of the input object to be coerced to class character. Must be
  specified in inverted commas. But this argument is usually specified
  directly by `x.name` argument of the clientside function
  `ds.asCharacter`

## Value

the object specified by the `newobj` argument (or its default name
"ascharacter.newobj") which is written to the serverside. For further
details see help on the clientside function `ds.asCharacter`

## Details

See help for function `as.character` in native R

## Author

Amadou Gaye, Paul Burton, Demetris Avraam for DataSHIELD Development
Team
