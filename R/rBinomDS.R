#' @title rBinomDS serverside assign function
#' @description primary serverside assign function called by ds.rBinom
#' @details Generates the vector of pseudorandom numbers from a binomial
#' distribution in each data source as specified by the arguments of
#' ds.rBinom. This serverside function is effectively the same as
#' the function rbinom() in native R and its arguments are the same.
#' @param n length of the pseudorandom number vector to be generated
#' as specified by the argument <samp.size> in the function ds.rBinom
#' @param size a scalar that must be a positive integer. Value set directly
#' by <size> argument of ds.rBinom - for details see help for ds.rBinom.
#' May be a scalar or a vector allowing the size to vary from
#' observation to observation.
#' @param prob a numeric scalar in range 0 > prob > 1 which specifies the
#' probability of a positive response. Value set directly
#' by <prob> argument of ds.rBinom - for details see help for ds.rBinom
#' May be a scalar or a vector allowing the size to vary from
#' observation to observation.
#' @return Writes the pseudorandom number vector with the characteristics specified
#' in the function call as a new serverside vector on the data source on which
#' it has been called. Also returns key information to the clientside:
#' the random seed as specified by you in each
#' source + (if requested) the full 626 length random seed vector this generated in
#' each source (see info for the argument <return.full.seed.as.set>). It
#' also returns a vector reporting the length of the pseudorandom vector
#' created in each source.
#' @author Paul Burton for DataSHIELD Development Team
#' @export
rBinomDS<-function (n, size = 1, prob = 0.5){

#If size or prob are defined by serverside vectors
#first convert their names into the corresponding active vectors

	if(is.character(size)){
	command.text<-size
	size<-eval(parse(text=command.text), envir = parent.frame())
	}

	if(is.character(prob)){
	command.text<-prob
	prob<-eval(parse(text=command.text), envir = parent.frame())
	}

	stats::rbinom(n, size=size, prob=prob)
}
#ASSIGN FUNCTION
# rBinomDS
