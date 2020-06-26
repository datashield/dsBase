#' @title tableDS.assign is the serverside assign function
#' called by ds.table
#' @description helps creates 1-dimensional, 2-dimensional and 3-dimensional
#' tables using the {table} function in native R.
#' @details If the <table.assign> argument of {ds.table} is set to TRUE,
#' this assign function writes the
#' the table requested in the format specified by {ds.table} function
#' as an object named by the <newobj> argument of {ds.table}. For more
#' information see help for {ds.table} in DataSHIELD and the {table} function
#' in native R.
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
#' @param rvar.all.unique.levels.transmit is a character string containing all
#' unique level in rvar, across the studies, separated by ','.
#' @param cvar.all.unique.levels.transmit is a character string containing all
#' unique level in cvar, across the studies, separated by ','.
#' @param stvar.all.unique.levels.transmit is a character string containing all
#' unique level in stvar, across the studies, separated by ','.
#' @param exclude.transmit for information see help on <exclude> argument
#' of {ds.table}. Fully specified by <exclude> argument of {ds.table}
#' @param useNA.transmit for information see help on <useNA> argument
#' of {ds.table}. Fully specified by <useNA> argument of {ds.table}
#' @return For information see help for {ds.table}
#' @author Paul Burton for DataSHIELD Development Team, 13/11/2019
#' @export
tableDS.assign<-function(rvar.transmit, cvar.transmit, stvar.transmit, rvar.all.unique.levels.transmit, cvar.all.unique.levels.transmit,
                  stvar.all.unique.levels.transmit, exclude.transmit, useNA.transmit){

#Activate via eval when needed
#rvar
rvar<-eval(parse(text=rvar.transmit), envir = parent.frame())

#coerce to factor if required
if(!is.factor(rvar))
{
  rvar.all.unique.levels<- unlist(strsplit(rvar.all.unique.levels.transmit,split=","))
  rvar<-factor(as.factor(rvar), levels=rvar.all.unique.levels)
}else{
  rvar.all.unique.levels<- unlist(strsplit(rvar.all.unique.levels.transmit,split=","))
  rvar<-factor(rvar, levels=rvar.all.unique.levels)
}
#cvar
if(!is.null(cvar.transmit))
{
cvar<-eval(parse(text=cvar.transmit), envir = parent.frame())

#coerce to factor if required
if(!is.factor(cvar))
{
  cvar.all.unique.levels<- unlist(strsplit(cvar.all.unique.levels.transmit,split=","))
  cvar<-factor(as.factor(cvar), levels=cvar.all.unique.levels)
}else{
  cvar.all.unique.levels<- unlist(strsplit(cvar.all.unique.levels.transmit,split=","))
  cvar<-factor(cvar, levels=cvar.all.unique.levels)
}
}
else
{
cvar<-NULL
}

#stvar
if(!is.null(stvar.transmit))
{
stvar<-eval(parse(text=stvar.transmit), envir = parent.frame())

#coerce to factor if required
if(!is.factor(stvar))
{
  stvar.all.unique.levels<- unlist(strsplit(stvar.all.unique.levels.transmit,split=","))
  stvar<-factor(as.factor(stvar), levels=stvar.all.unique.levels)
}else{
  stvar.all.unique.levels<- unlist(strsplit(stvar.all.unique.levels.transmit,split=","))
  stvar<-factor(stvar, levels=stvar.all.unique.levels)
}
}
else
{
stvar<-NULL
}

#exclude
if(!is.null(exclude.transmit))
{
exclude.text<-strsplit(exclude.transmit, split=",")
exclude<-eval(parse(text=exclude.text), envir = parent.frame())
}
else
{
exclude<-NULL
}

#prepare output object
if(!is.null(rvar)&&!is.null(cvar)&&!is.null(stvar))
{
	outobj<-table(rvar,cvar,stvar,exclude=exclude,useNA=useNA.transmit)
}


if(!is.null(rvar)&&!is.null(cvar)&&is.null(stvar))
{
	outobj<-table(rvar,cvar,exclude=exclude,useNA=useNA.transmit)	
}


if(!is.null(rvar)&&is.null(cvar)&&is.null(stvar))
{
	outobj<-table(rvar,exclude=exclude,useNA=useNA.transmit)	
}

out.table<-as.table(outobj)
out.dimnames<-dimnames(outobj)
out.dim<-dim(outobj)
out.counts<-as.vector(outobj)

out.list<-list(table=out.table,counts=out.counts,dim=out.dim,dimnames=out.dimnames)
return(out.list)

}
#ASSIGN FUNCTION
# tableDS.assign
