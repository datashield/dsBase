#' @title Secure ranking of "V2BR" (vector to be ranked) across all sources
#' @description takes key non-disclosive components of the serverside data frame
#' blackbox.ranks.df over to the clientside to enable global re-ranking of the
#' global ranks just applying to the real data (not the pseudo-data).
#' @details Severside aggregate function called by ds.ranksSecure. The
#' non-disclosive components of blackbox.ranks.df that are transmitted to the
#' clientside are: (1) final values of the encrypted global ranks vector
#' after all seven rounds of encryption have been completed; (2) a set of
#' sequential IDs allocated to the global ranks vector in each study in their
#' current order based on increasing value of V2BR. This allows later re-linkage
#' of values back on the serverside and confirmation that that linkage is
#' correct. (3) a studyid vector with all values n in the nth study. This
#' facilitates data management on the serverside during the global ranking
#' of global ranks. For more details about the cluster of functions that
#' collectively enable secure global ranking and estimation of global quantiles
#' see the associated document entitled "secure.global.ranking.docx". Also see
#' the header file for ds.ranksSecure
#' @return the non-disclosive elements of blackbox.output.df (see details)
#' on the serverside as a data frame object (called sR6.df)
#' on the clientside. After processing within ds.ranksSecure to create the
#' global ranks and global quantiles (of real data only) across all studies,
#' this is returned to the serverside as data frame "global.ranks.quantiles.df"
#' using the clientside function ds.dmtC2S. To illustrate the difference between
#' ranks and quantiles, if there are a total of 1000 original real observations
#' across all studies and one particular observation has the rank 250, it will
#' have quantile value 0.25 (i.e. 25%th value in the overall data ordered by
#' increasing value). Both ranks and quantiles can have ties. For more details
#' about the cluster of functions that collectively enable secure global ranking
#' and estimation of global quantiles see the associated document entitled
#' "secure.global.ranking.docx". Also see the header file for ds.ranksSecure
#' @author Paul Burton 9th November, 2021
#' @export
ranksSecureDS3 <- function(){ #START FUNC

sR6.df<-data.frame(cbind(blackbox.ranks.df[,9],1:nrow(blackbox.ranks.df),blackbox.ranks.df[,7]))
colnames(sR6.df)<-c("encrypted.ranks","ID.real.by.val","studyid")

return(sR6.df)
}

#AGGREGATE
# ranksSecureDS3

  


