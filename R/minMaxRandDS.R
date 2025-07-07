#' @title Secure ranking of "V2BR" (vector to be ranked) across all sources
#' @description Creates a minimum value that is more negative, and less positive
#' than any real value in V2BR and a maximum value that is more positive and
#' less negative than any value of V2BR. 
#' @details Severside aggregate function called by ds.ranksSecure. The minimum
#' and maximum values it creates are used to replace missing values
#' (NAs) in V2BR if the argument <NA.manag>e is set to "NA.low" or "NA.hi"
#' respectively. For more details about the cluster of functions that
#' collectively enable secure global ranking and estimation of global quantiles
#' see the associated document entitled "secure.global.ranking.docx". Also
#' see the header file for ds.ranksSecure
#' @param input.var.name a character string specifying the name of V2BR. This
#' argument is set by the argument with the same name in the clientside function
#' ds.ranksSecure
#' @return the data frame objects containing the global ranks and quantiles.
#' For more details see the associated document entitled
#' "secure.global.ranking.docx"
#' @author Paul Burton 9th November, 2021
#' @export
#'
minMaxRandDS <- function(input.var.name){ #START FUNC
  
  # back-up current .Random.seed and revert on.exit
  old_seed <- .Random.seed
  on.exit(.Random.seed <- old_seed, add = TRUE)
  
  input.var <- eval(parse(text=input.var.name), envir = parent.frame())

    #create seed that is unknown and cannot be repeated 
  set.seed(NULL)
  
  mult.created<-0
  while(mult.created==0){
    rand.mult.temp<-stats::rnorm(1,1.5,0.5)
      if(rand.mult.temp>1.2){
      rand.mult<-rand.mult.temp
      mult.created<-1
    }
  }
   rand.max<-max(input.var,na.rm=TRUE)
   rand.min<-min(input.var,na.rm=TRUE)
   
   if(rand.max<0){
      rand.max<-(-rand.max)+1
   }

      if(rand.max==0){
     rand.max<-1
   }

   rand.max<-rand.max*rand.mult   

   if(rand.min>0){
     rand.min<-(-rand.min)-1
   }
   
   if(rand.min==0){
     rand.min<-(-1)
   }
   
   rand.min<-rand.min*rand.mult   
   
rand.min.max<-c(rand.min,rand.max)

return(rand.min.max)

}
#AGGREGATE
# minMaxRandDS

  


