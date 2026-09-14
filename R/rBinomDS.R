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
#' @return the vector of pseudorandom numbers from a binomial distribution, which
#' is written to the serverside as the object named by the <newobj> argument
#' of ds.rBinom.
#' @author Paul Burton for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
rBinomDS<-function (n, size = 1, prob = 0.5){

        # Check Permissive Privacy Control Level.
        dsBase::checkPermissivePrivacyControlLevel(c('permissive', 'avocado'))

#If size or prob are defined by serverside vectors
#first convert their names into the corresponding active vectors

	if(is.character(size)){
	size<-.loadServersideObject(size)
	}

	if(is.character(prob)){
	prob<-.loadServersideObject(prob)
	}

	stats::rbinom(n, size=size, prob=prob)
}
#ASSIGN FUNCTION
# rBinomDS
