#' @title Secure ranking of "V2BR" (vector to be ranked) across all sources
#' @description Creates a data frame "sR8.df" by cbinding the data frame
#' "blackBox.ranks.df" with the global ranks and global quantiles vectors in
#' "global.ranks.quantiles.df". Performs QA on this matrix and orders the
#' sR8.df data frame according to the argument <ranks.sort.by> in ds.ranksSecure
#' @details Severside assign function called by ds.ranksSecure.
#' Creates a data frame "sR8.df" by cbinding the data frame "blackBox.ranks.df"
#' with the global ranks and global quantiles vectors in
#' "global.ranks.quantiles.df". Checks that all components of sR8.df have the
#' correct dimensions and  are consistent in their ordering. If either the
#' number of rows or the order of the rows are inconsistent with those in
#' "blackBox.ranks.df" an error message is returned and the processing stops.
#' If sR8.df passes all QA tests it is written to the serverside as a data
#' frame with its name identified by the argument <output.ranks.df> in
#' ds.ranksSecure. If that argument is NULL or unspecified the data frame
#' is called "main.ranks.df". The ranksSecureDS4 function also orders the
#' combined data frame (<output.ranks.df>) in one of two ways: if
#' the argument <ranks.sort.by> in ds.ranksSecure is set to "ID.orig" the
#' combined data frame is ordered in the same way as the original V2BR vector;
#' if the argument <ranks.sort.by> is set to vals.orig" the combined data frame
#' is ordered by the magnitude of the values of V2BR (ascending). Having
#' created the data frame (<output.ranks.df>) in this manner it can now be
#' directly cbinded to either the V2BR vector itself or to a data frame, tibble
#' or matrix containing V2BR (assuming they are also in the order corresponding
#' to the argument <ranks.sort.by>) and this combined object can be used as the
#' basis of analysis based on the global ranks or quantiles including a range
#' of types of non-parametric analysis.
#' @param ranks.sort.by a character string taking two possible values. These
#' are "ID.orig" and "vals.orig". These define the order in which the
#' output.ranks.df and summary.output.ranks.df data frames are presented. This
#' argument is set by the argument with the same name in ds.ranksSecure.
#' Default value is "ID.orig".
#' @return Creates the data frame identified by the name given by the argument
#' (<output.ranks.df>) of the ds.ranksSecure function and writes it to the
#' serverside. If the argument <output.ranks.df> is NULL or unspecified the
#' output data frame is called "main.ranks.df". The data frame is ordered
#' according to the argument <ranks.sort.by> in ds.ranksSecure.
#' @author Paul Burton 9th November, 2021
#' @export
ranksSecureDS4 <- function(ranks.sort.by){ #START FUNC

sR8.df<-cbind(blackbox.ranks.df,global.ranks.quantiles.df$real.ranks.global,global.ranks.quantiles.df$real.quantiles.global)
colnames(sR8.df)[ncol(sR8.df)-1]<-"final.ranks.global"
colnames(sR8.df)[ncol(sR8.df)]<-"final.quantiles.global"

ord.by.ID.orig<-order(sR8.df$ID.seq.real.orig)

if(ranks.sort.by!="ID.orig" && ranks.sort.by!="vals.orig"){
    error.message<-
      paste0("FAILED: ranks.sort.by must be specified as either 'ID.orig' or 'vals.orig'")
    stop(error.message, call. = FALSE)
}

if(ranks.sort.by=="ID.orig")
{
sR8.df<-sR8.df[ord.by.ID.orig,]
}

if(ranks.sort.by=="vals.orig")
{
  sR8.df<-sR8.df
}

lenx<-length(sR8.df$encrypted.input.var)

if((length(sR8.df$global.ranks.input.from.sR5)!=lenx)||
   (length(sR8.df$global.ranks.after.blackbox)!=lenx)||
   (length(sR8.df$final.ranks.global)!=lenx)||
   (length(sR8.df$final.quantiles.global)!=lenx))
{
  error.message<-
    paste0("FAILED: ranking components of different lengths. This could reflect
            modification of the clientside code which is not recommended. It can
            also occur if the R session on one or more of the opal data servers
            runs out of memory")
  stop(error.message, call. = FALSE)
}  

ranks.consistent<-1

if(sum(round(rank(sR8.df$global.ranks.input.from.sR5)-rank(sR8.df$global.ranks.after.blackbox),2)==0)!=lenx){
  ranks.consistent<-0  
}

if(sum(round(rank(sR8.df$global.ranks.input.from.sR5)-rank(sR8.df$final.ranks.global),2)==0)!=lenx){
ranks.consistent<-0  
}

if(sum(round(rank(sR8.df$global.ranks.input.from.sR5)-rank(sR8.df$final.quantiles.global),2)==0)!=lenx){
  ranks.consistent<-0  
}

if(ranks.consistent==0)
{
  error.message<-
    paste0("FAILED: inconsistent ranking across different components of ranking matrix,
            try a different seed. Altenatively this could reflect modification of the
            clientside code which is not recommended. Finally, it can also occur
            if the R session on one or more of the opal data servers runs out
            of memory")
  stop(error.message, call. = FALSE)
}

return(sR8.df)
}

#ASSIGN
# ranksSecureDS4
