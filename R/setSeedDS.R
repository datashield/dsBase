#' @title setSeedDs called by ds.setSeed, ds.rNorm, ds.rUnif,
#' ds.rPois and ds.rBinom
#' @description An aggregate serverside function that primes
#' the pseudorandom number generator in a data source
#' @details setSeedDS is effectively equivalent to the native R function
#' set.seed() and so the help for that function can provide many
#' additional details. The only very minor difference is that the
#' first argument of setSeedDS, <seedtext> takes the integer priming
#' seed in character format. However, for the user that integer is still
#' specified directly as an integer as the <seed.as.integer> argument
#' of one of the clientside functions ds.setSeed, ds.rNorm .....
#' Each of these clientside functions coerces the integer to character format
#' calls setSeedDS and the first active line of code in setSeedDS converts
#' the character string back to an integer and treats it as the first
#' argument <seed> of the native R function set.seed(). The two other
#' arguments of set.seed() in native R, <kind> and <normal.kind> are both
#' defaulted by specifying them as NULL. This defaulting is hard wired into
#' the setSeedDS function and as this cannot be changed by the analyst it means
#' that setSeedDS is much less flexible than native R's set.seed() function.
#' If any DataSHIELD user requires some aspect of this flexibility returned
#' the development team can be approached, but unless you are actually doing
#' theoretical work with random number generators it is likely that the
#' @param seedtext this is simply the value of the <seed.as.integer> argument
#' of ds.setSeed, ds.rNorm, ds.rUnif, ds.rPois of ds.rBinom coerced
#' into character format. This is done by the clientside functions themselves
#' and does not require the DataSHIELD user to do anything. Please see the
#' help for these clientside functions, and in particular, the information
#' for the argument <seed.as.integer> for more details.
#' @param kind see help for set.seed() function in native R
#' @param normal.kind see help for set.seed() function in native R
#' @return Sets the values of the vector of integers of length 626 known as
#' .Random.seed on each data source that is the true current state of the
#' random seed in each source.
#' @author Paul Burton for DataSHIELD Development Team
#' @export
setSeedDS<-function (seedtext=NULL, kind = NULL, normal.kind = NULL)
{
    # Check Permissive Privacy Control Level.
    dsBase::checkPermissivePrivacyControlLevel(c('permissive', 'avocado'))

    seed<-eval(parse(text=seedtext), envir = parent.frame())
    set.seed(seed,kind,normal.kind)
    return(list(seed.as.set=.Random.seed))
}
#AGGREGATE
# setSeedDS
