#' @title rUnifDS serverside assign function
#' @description primary serverside assign function called by ds.rUnif
#' @details Generates the vector of pseudorandom numbers from a uniform
#' distribution in each data source as specified by the arguments of
#' ds.rUnif. This serverside function is effectively the same as
#' the function runif() in native R and its arguments are the same.
#' @param n length of the pseudorandom number vector to be generated
#' as specified by the argument <samp.size> in the function ds.rUnif
#' @param min a numeric scalar specifying the minimum of the range across which
#' the random numbers will be generated in each source. Specified directly
#' by the min argument in ds.rUnif. May be a scalar or a vector allowing the min
#' to vary from observation to observation.
#' @param max a numeric scalar specifying the maximum of the range across which
#' the random numbers will be generated in each source. Specified directly
#' by the max argument in ds.rUnif. May be a scalar or a vector allowing the min
#' to vary from observation to observation.
#' @param force.output.to.k.decimal.places scalar integer. Forces the output random
#' number vector to have k decimal places. If 0 rounds it coerces
#' decimal random number output to integer, a k in range 1-8 forces output to
#' have k decimal places. If k = 9, no rounding occurs of native output.
#' Default=9. Value specified by <force.output.to.k.decimal.places> argument
#' in ds.rUnif
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
rUnifDS<-function (n, min = 0, max = 1, force.output.to.k.decimal.places=9){

#If min or max are defined by serverside vectors
#first convert their names into the corresponding active vectors

	if(is.character(min)){
	command.text<-min
	min<-eval(parse(text=command.text), envir = parent.frame())
	}

	if(is.character(max)){
	command.text<-max
	max<-eval(parse(text=command.text), envir = parent.frame())
	}

	random.number.vector<-stats::runif(n, min=min, max=max)

	if(force.output.to.k.decimal.places<9){
	random.number.vector<-round(random.number.vector,force.output.to.k.decimal.places)
	}

	return(random.number.vector)
}

#ASSIGN FUNCTION
# rUnifDS
