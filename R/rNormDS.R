#' @title rNormDS serverside assign function
#' @description primary serverside assign function called by ds.rNorm
#' @details Generates the vector of pseudorandom numbers from a normal
#' distribution in each data source as specified by the arguments of
#' ds.rNorm. This serverside function is effectively the same as
#' the function rnorm() in native R and its arguments are the same.
#' @param n length of the pseudorandom number vector to be generated
#' as specified by the argument <samp.size> in the function ds.rNorm
#' @param mean this specifies the mean of the pseudorandom number vector
#' to be generated as specified by the argument <mean> in the
#' function ds.rNorm. May be a scalar or a vector allowing the mean
#' to vary from observation to observation.
#' @param sd this specifies the standard deviation of the pseudorandom
#' number vector to be generated as specified by the argument <sd> in the
#' function ds.rNorm May be a scalar or a vector allowing the sd
#' to vary from observation to observation.
#' @param force.output.to.k.decimal.places scalar integer. Forces the output random
#' number vector to have k decimal places. If 0 rounds it coerces
#' decimal random number output to integer, a k in range 1-8 forces output to
#' have k decimal places. If k = 9, no rounding occurs of native output.
#' Default=9. Value specified by <force.output.to.k.decimal.places> argument
#' in ds.rNorm
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
rNormDS<-function (n, mean = 0, sd = 1, force.output.to.k.decimal.places=9){

#If mean or sd are defined by serverside vectors
#first convert their names into the corresponding active vectors

	if(is.character(mean)){
	command.text<-mean
	mean<-eval(parse(text=command.text), envir = parent.frame())
	}

	if(is.character(sd)){
	command.text<-sd
	sd<-eval(parse(text=command.text), envir = parent.frame())
	}

	random.number.vector<-stats::rnorm(n, mean=mean, sd=sd)

	if(force.output.to.k.decimal.places<9){
	random.number.vector<-round(random.number.vector,force.output.to.k.decimal.places)
	}

	return(random.number.vector)
	
}
#ASSIGN FUNCTION
# rNormDS
