#' @title tableDS is the second of two serverside aggregate functions
#' called by ds.table
#' @description Helps creates 1-dimensional, 2-dimensional and 3-dimensional
#' tables using the {table} function in native R.
#' @details If the <table.assign> argument of {ds.table} is set to TRUE,
#' this aggregate function returns non-disclosive information about
#' the table object written to the serverside by {tableDS.assign}. For more
#' information see help for {ds.table}, {tableDS.assign} and {tableDS}
#' in DataSHIELD and the {table} function in native R.
#' @param newobj this a character string providing a name for the output
#' table object to be written to the serverside if <table.assign> is TRUE.
#' If no explicit name for the table object is specified, but <table.assign>
#' is nevertheless TRUE, the name for the serverside table object defaults
#' to 'newObj'. Fully specified by <newobj> argument in {ds.table}.
#' For more information see help for {ds.table}
#' @param rvar.transmit is a character string (in inverted commas) specifiying the
#' name of the variable defining the rows in all of the 2 dimensional
#' tables that form the output. Fully specified by <rvar> argument in {ds.table}.
#' For more information see help for {ds.table}
#' @param cvar.transmit is a character string specifiying the
#' name of the variable defining the columns in all of the 2 dimensional
#' tables that form the output. Fully specified by <cvar> argument in {ds.table}.
#' For more information see help for {ds.table}
#' @param stvar.transmit is a character string specifiying the
#' name of the variable that indexes the separate two dimensional
#' tables in the output if the call specifies a 3 dimensional table.
#' Fully specified by <stvar> argument in {ds.table}.
#' For more information see help for {ds.table}
#' @return For information see help for {ds.table}
#' @author Paul Burton for DataSHIELD Development Team, 13/11/2019
#' @export
tableDS2 <- function(newobj,rvar.transmit,cvar.transmit,stvar.transmit){


calltext1<-paste0("out.table.real<-",newobj,"[[2]]")
eval(parse(text=calltext1), envir = parent.frame())
out.table.cell.IDs<-as.vector(1:length(out.table.real))

calltext2<-paste0("out.table.dim<-",newobj,"[[3]]")
eval(parse(text=calltext2), envir = parent.frame())

calltext3<-paste0("out.table.dimnames<-",newobj,"[[4]]")
eval(parse(text=calltext3), envir = parent.frame())

out.table.structure<-array(out.table.cell.IDs,dim=out.table.dim,dimnames=out.table.dimnames)

#table dimension names
if(!is.null(rvar.transmit)&&!is.null(cvar.transmit)&&!is.null(stvar.transmit))
{
names(dimnames(out.table.structure))<-c(rvar.transmit,cvar.transmit,stvar.transmit)
}

if(!is.null(rvar.transmit)&&!is.null(cvar.transmit)&&is.null(stvar.transmit))
{
names(dimnames(out.table.structure))<-c(rvar.transmit,cvar.transmit)
}

if(!is.null(rvar.transmit)&&is.null(cvar.transmit)&&is.null(stvar.transmit))
{
names(dimnames(out.table.structure))<-c(rvar.transmit)
}

out.list<-list(table.cell.IDs=out.table.cell.IDs,table.dim=out.table.dim,table.dimnames=out.table.dimnames,table.structure_and_cell.order=out.table.structure)
return(out.list)
}

#AGGREGATE FUNCTION
# tableDS2
