#' 
#' @title lists all objects on a serverside environment
#' @description creates a list of the names of all of the objects in
#' a specified serverside environment
#' @details Serverside aggregate function {lsDS} called by clientside function
#' {ds.ls}. When running analyses one may want to know the objects already generated. This 
#' request is not disclosive as it only returns the names of the objects and not their contents. 
#' By default, objects in the current 'active analytic environment' (".GlobalEnv")
#' will be displayed. This
#' is the environment that contains all of the objects that serverside DataSHIELD 
#' is using for the main analysis or has written out to the serverside during the process
#' of managing or undertaking the analysis (variables, scalars, matrices, data.frames etc).
#' For further details see help for {ds.ls} function and for native R function {ls}
#' @param search.filter either NULL or a character string (potentially including '*'
#' wildcards) specifying required search criteria. This argument is
#' fully specified by its corresponding argument in the clientside function.
#' @param env.to.search integer (e.g. in a format such as '2' or '5L' format) specifying
#' the position in the search path of the environment to be explored. This argument is
#' fully specified by its corresponding argument in the clientside function.
#' @return a list containing: (1) the name/details of the serverside R environment
#' which {ds.ls} has searched; (2) a vector of character strings giving the names of
#' all objects meeting the naming criteria specified by the argument <search.filter> in this
#' specified R serverside environment; (3) the nature of the search filter string as it was
#' actually applied
#' @author Gaye, A (2015). Updated and extended by Paul Burton (2020).
#' @export
lsDS<-function(search.filter=NULL,env.to.search){
#########################################################################
# DataSHIELD MODULE: CAPTURE THE nfilter SETTINGS                       #
thr <- listDisclosureSettingsDS()                                       #
#nfilter.tab<-as.numeric(thr$nfilter.tab)                               #
#nfilter.glm<-as.numeric(thr$nfilter.glm)                               #
#nfilter.subset<-as.numeric(thr$nfilter.subset)                         #
nfilter.string<-as.numeric(thr$nfilter.string)                          #
#nfilter.stringShort<-as.numeric(thr$nfilter.stringShort)               #
#nfilter.kNN<-as.numeric(thr$nfilter.kNN)                               #
#datashield.privacyLevel<-as.numeric(thr$datashield.privacyLevel)       #
#########################################################################

envir.text<-".GlobalEnv"

if(env.to.search==1L)
{
envir.2.search<-eval(parse(text=envir.text))
}else{
envir.2.search<-as.environment(env.to.search)
}


j<-0

if(is.null(search.filter))
{
outobj<-ls(pos=env.to.search)

outlist<-list(environment.searched=environmentName(envir.2.search),objects.found=outobj)

return(outlist)
  
}else{
  search.filter.split<-unlist(strsplit(search.filter,split=""))
  length.sf<-length(search.filter.split)
  
  search.filter.temp1<-NULL
#  print(length.sf)

  if(length.sf>=5)
  { 
    while(j <= (length.sf-4))
    {
      j<-j+1
 
      print(j)     
      add.to<-search.filter.split[j]
      if(search.filter.split[j]=="_"&&search.filter.split[j+1]==":"&&search.filter.split[j+2]=="A"&&
         search.filter.split[j+3]==":"&&search.filter.split[j+4]=="_")
      {
        add.to<-"*"
        j<-j+4
      }
      search.filter.temp1<-c(search.filter.temp1,add.to)
    }
    
    
    while(j<length.sf)
      {
      j<-j+1
      print(j)
      add.to<-search.filter.split[j]
      search.filter.temp1<-c(search.filter.temp1,add.to)
      }
    
    search.filter.final<-paste(search.filter.temp1,collapse="")
    
    outobj<-ls(pos=env.to.search,pattern=utils::glob2rx(search.filter.final))
  }else{

   outobj<-ls(pos=env.to.search,pattern=utils::glob2rx(search.filter))
   search.filter.final<-search.filter
  }

outlist<-list(environment.searched=environmentName(envir.2.search), objects.found=outobj,search.filter.final=search.filter.final)

return(outlist)
}
}
#AGGREGATE FUNCTION
# lsDS
