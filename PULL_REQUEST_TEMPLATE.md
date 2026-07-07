## Instructions & checklist for PR author

### Description of changes
[Add descriptions of changes made]

### Refactor instructions
- [ ] Replaced  `x <- eval(parse(text = x.name), envir = parent.frame())` with `x <- .loadServersideObject(x)`
- [ ] If necessary, check the class of the object using `.checkClass()`

### Testing instructions
- [ ] Writen server-side unit tests for unhappy flow
- [ ] Run `devtools::test(filter = "smk-|disc|arg")` and check it passes
- [ ] Run `devtools::check(args = '--no-tests')` and check it passes (we run tests separately to skip performance checks)
- [ ] Run `devtools::build()` and check it builds without errors

## Instructions & checklist for PR reviewers
- [ ] Run `devtools::test(filter = "smk-|disc|arg")` and check it passes
- [ ] Run `devtools::check(args = '--no-tests')` and check it passes (we run tests separately to skip performance checks)
- [ ] Run `devtools::build()` and check it builds without errors

