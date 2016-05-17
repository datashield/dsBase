isValidNumericDS <- function(obj){

  unique.values.noNA <- unique(obj[complete.cases(obj)])
  toosmall <- 0

  if(length(unique.values.noNA)==2){
    tabvar <- table(obj)[table(obj)>=1]
    min.category <- min(tabvar)
    nf.tab <- getOption("nfilter.tab")
    if(min.category < nf.tab){
      toosmall <- 1
    }
  }
 
return(toosmall)

}

