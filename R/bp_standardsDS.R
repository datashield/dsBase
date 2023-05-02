#' 
#' @title Calculates Blood pressure z-scores
#' @description The function calculates blood pressure z-scores in two steps:
#' Step 1. Calculates z-score of height according to CDC growth chart (Not the 
#' WHO growth chart!). Step 2. Calculates z-score of BP according to the fourth
#' report on BP management, USA
#' @param sex the name of the sex variable. The variable should be coded as 1 for males
#' and 2 for females. If it is coded differently (e.g. 0/1), then you can use the 
#' ds.recodeValues function to recode the categories to 1/2 before the use of 
#' ds.bp_standards
#' @param age the name of the age variable in years.
#' @param height the name of the height variable in cm
#' @param bp the name of the blood pressure variable.
#' @param systolic logical. If TRUE (default) the function assumes conversion of 
#' systolic blood pressure. If FALSE the function assumes conversion of diastolic 
#' blood pressure.
#' @return assigns a new object on the server-side. The assigned object is a list 
#' with two elements: the 'Zbp' which is the zscores of the blood pressure and 'perc'
#' which is the percentiles of the BP zscores.
#' @note The z-scores of height based on CDC growth charts are calculated 
#' by the sds function from the childsds R package.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @import childsds
#' @export
#' 
bp_standardsDS <- function(sex=sex, age=age, height=height, bp=bp, systolic=systolic){
  
  if(is.character(sex)){
    sex <- eval(parse(text = sex), envir = parent.frame())
  }
  if(is.character(age)){
    age <- eval(parse(text = age), envir = parent.frame())
  }
  if(is.character(height)){
    height <- eval(parse(text = height), envir = parent.frame())
  }
  if(is.character(bp)){
    bp <- eval(parse(text = bp), envir = parent.frame())
  }
  
  # convert height to a Z-score relative to age and sex based on CDC growth charts
  Zht <- sds(value=height, age=age, sex=sex, male="1", female="2",
             ref = childsds::cdc.ref, item = "height2_20", type = "SDS")
  
  # Compute the expected BP (systolic or diastolic) for males/females of age y years and height
  # h inches using the regression coefficients given in table B-1 in 
  # https://www.nhlbi.nih.gov/sites/default/files/media/docs/hbp_ped.pdf
  males_idx <- which(sex==1)
  females_idx <- which(sex==2)
  mu <- rep(NA, times=length(sex))
  if(systolic==TRUE){
    mu[males_idx] <- 102.19768 + 1.82416 * (age[males_idx]-10) + 0.12776 * (age[males_idx]-10)^2 + 
      0.00249 * (age[males_idx]-10)^3 - 0.00135 * (age[males_idx]-10)^4 + 2.73157 * Zht[males_idx] - 
      0.19618 * Zht[males_idx]^2 - 0.04659 * Zht[males_idx]^3 + 0.00947 * Zht[males_idx]^4 
    mu[females_idx] <- 102.01027 + 1.94397 * (age[females_idx]-10) + 0.00598 * (age[females_idx]-10)^2 - 
      0.00789 * (age[females_idx]-10)^3 - 0.00059 * (age[females_idx]-10)^4 + 2.03526 * Zht[females_idx] + 
      0.02534 * Zht[females_idx]^2 - 0.01884 * Zht[females_idx]^3 + 0.00121 * Zht[females_idx]^4 
  }  
  if(systolic==FALSE){
    mu[males_idx] <- 61.01217 + 0.68314 * (age[males_idx]-10) - 0.09835 * (age[males_idx]-10)^2 + 
      0.01711 * (age[males_idx]-10)^3 + 0.00045 * (age[males_idx]-10)^4 + 1.46993 * Zht[males_idx] - 
      0.07849 * Zht[males_idx]^2 - 0.03144 * Zht[males_idx]^3 + 0.00967 * Zht[males_idx]^4 
    mu[females_idx] <- 60.50510 + 1.01301 * (age[females_idx]-10) + 0.01157 * (age[females_idx]-10)^2 + 
      0.00424 * (age[females_idx]-10)^3 - 0.00137 * (age[females_idx]-10)^4 + 1.16641 * Zht[females_idx] + 
      0.12795 * Zht[females_idx]^2 - 0.03869 * Zht[females_idx]^3 - 0.00079 * Zht[females_idx]^4 
  }    

  # convert the observed BP to a Z-score (Zbp) using sigmas given in table B-1
  Zbp <- rep(NA, times=length(mu))
  if(systolic==TRUE){
    Zbp[males_idx] <- (bp[males_idx]-mu[males_idx])/10.7128
    Zbp[females_idx] <- (bp[females_idx]-mu[females_idx])/10.4855
  } 
  if(systolic==FALSE){
    Zbp[males_idx] <- (bp[males_idx]-mu[males_idx])/11.6032
    Zbp[females_idx] <- (bp[females_idx]-mu[females_idx])/10.9573
  } 
  
  # convert the bp Z-score to a percentile
  perc <- round(stats::pnorm(Zbp)*100, digits=2)

  return(list(Zbp=Zbp, perc=perc))

}
