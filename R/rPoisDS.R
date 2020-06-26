#' @title rPoisDS serverside assign function
#' @description primary serverside assign function called by ds.rPois
#' @details Generates the vector of pseudorandom numbers (non-negative
#' integers) from a Poisson distribution in each data source as specified
#' by the arguments of ds.rPois. This serverside
#' function is effectively the same as the function rpois() in native R
#' and its arguments are the same.
#' @param n length of the pseudorandom number vector to be generated
#' as specified by the argument <samp.size> in the function ds.rPois
#' @param lambda a numeric scalar specifying the expected count of the Poisson
#' distribution used to generate the random counts. Specified directly
#' by the lambda argument in ds.rPois. May be a scalar or a vector allowing lambda
#' to vary from observation to observation.
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
rPoisDS<-function (n, lambda = 1){

#If lambda is defined by a serverside vector
#first convert its name into the corresponding active vectors

	if(is.character(lambda)){
	command.text<-lambda
	lambda<-eval(parse(text=command.text), envir = parent.frame())
	}

	stats::rpois(n, lambda=lambda)
}
#ASSIGN FUNCTION
# rPoisDS
