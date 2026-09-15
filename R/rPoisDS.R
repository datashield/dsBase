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
#' @return the vector of pseudorandom non-negative integers from a Poisson
#' distribution, which is written to the serverside as the object named by the
#' <newobj> argument of ds.rPois.
#' @author Paul Burton for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
rPoisDS<-function (n, lambda = 1){

        # Check Permissive Privacy Control Level.
        dsBase::checkPermissivePrivacyControlLevel(c('permissive', 'avocado'))

#If lambda is defined by a serverside vector
#first convert its name into the corresponding active vectors

	if(is.character(lambda)){
	lambda<-.loadServersideObject(lambda)
	}

	stats::rpois(n, lambda=lambda)
}
#ASSIGN FUNCTION
# rPoisDS
