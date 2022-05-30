#' @title Secure ranking of "V2BR" (vector to be ranked) across all sources
#' @description Checks that the data frame produced in creating the initial
#' global ranks (ranks based on real and pseudo-data after the running of
#' blackBoxDS)has the correct dimensions and order as the serverside data frames
#' to which it will now be appended. If either the number of rows or the order
#' of the rows are inconsistent with the pre-existing data frames on the
#' serverside an error message is returned and the processing stops. Then
#' strips out the pseudo-data leaving solely the global ranks based just on
#' the real data
#' @details Severside assign function called by ds.ranksSecure. 
#' It works on the on the output created by serverside function ranksSecureDS1
#' and saved on the serverside in data frame sR4.df by ds.dmtC2S. Having
#' checked QA it strips out all rows corresponding to pseudo-data. The resultant
#' data frame contains the following vectors: (1) the fully encrypted V2BR
#' (after application of blackBoxDS);(2) "ID.by.val" the sequential
#' ID associated with the "combined real+pseudo data vector" sorted by
#' value (ascending); (3) "studyid", a vector consisting solely of value n in
#' the nth study; (4) "global.rank" the vector containing global ranks created by
#' the clientside code in ds.ranksSecure after ranksSecureDS1 is called and up
#' to the point where ds.dmtC2S sends sR4.df to the serverside. For more
#' details about the cluster of functions that collectively enable secure global
#' ranking and estimation of global quantiles see the associated document
#' entitled "secure.global.ranking.docx". Also see the header file
#' for ds.ranksSecure
#' @return creates a new data frame sR5.df on the serverside containing
#' solely the real data and including key elements needed for next stage of the
#' ranking process. Most crucially these include "global.rank" and "ID.by.val"
#' sorted in ascending order of the magnitude of V2BR
#' @author Paul Burton 9th November, 2021
#' @export
ranksSecureDS2 <- function(){ #START FUNC

########################################################################## 
##########################################################################
##LOCAL CODE SET UP TO CHECK ERROR TRAPS DURING DEVELOPMENT
##THIS CODE TESTED AND PASSED 11/10/21
#
##FIRST RUN QA.ranksSecure
#
##THEN RUN THIS FUNCTION WITH NEITHER TRAP TRIGGERED (ie FROM #generic ..)
##SHOULD BE NO ERROR MESSAGES
#
##THEN RUN WITH JUST TRAP 2 TRIGGERED (i.e. FROM #trap 2)
##SHOULD JUST BE TRAP 2 ERROR MESSAGE
#
##THEN RUN QA.ranksSecure AGAIN
#
##THEN RUN WITH JUST TRAP 1 TRIGGERED (i.e. FROM #trap 1 BUT NOT #trap 2)
##SHOULD JUST BE TRAP 2 ERROR MESSAGE
#  
##trap 1
## bodf.1<-bodf.1[2:nrow(bodf.1),]
#
##trap 2    
##  ord10<-order(bodf.1[,5])
##  bodf.1<-bodf.1[ord10,]
#
##generic set up no traps triggered
#  
#sR4.df<-R.sR4.df.2
#blackbox.output.df<-bodf.2
########################################################################## 
########################################################################## 
  
#check key data frames have the same number of rows and are in the same order
df.equivalent<-1

if(nrow(blackbox.output.df)!=nrow(sR4.df)){
  df.equivalent<-0
  }
if(df.equivalent==0){
error.message<-
  paste0("FAILED: in at least one study, data frame before global ranking has
  a different number of rows to data.frame after global ranking. Please explore
  and correct whatever may have caused this")
stop(error.message, call. = FALSE)
}

if(sum(round(abs(blackbox.output.df$encrypted.var-sR4.df$encrypted.var),2))>0){
  df.equivalent<-0
}

if(df.equivalent==0){
  error.message<-
    paste0("FAILED: in at least one study, data frame before global ranking is 
  in a different order to the data.frame after global ranking. Please explore
  and correct whatever may have caused this")
  stop(error.message, call. = FALSE)
}

  
sR5.df.temp<-data.frame(cbind(blackbox.output.df,sR4.df[,c(1,4,3)]))

sR5.df.real<-sR5.df.temp[blackbox.output.df$is.real==1,]

colnames(sR5.df.real)<-colnames(sR5.df.temp)

colnames(sR5.df.real)[1]<-"input.var.real.orig"
colnames(sR5.df.real)[3]<-"ranks.input.var.real.orig"
colnames(sR5.df.real)[5]<-"ID.seq.real.orig"


return(sR5.df.real)
}

#ASSIGN
# ranksSecureDS2

  


