#'
#' @title Computes the WHO Growth Reference z-scores of anthropometric data 
#' @description Calculate WHO Growth Reference z-score for a given anthropometric measurement
#' This function is similar to R function \code{getWGSR} from the \code{zscorer} package.
#' @details The function computes the WHO Growth Reference z-scores of anthropometric data for
#' weight, height or length, MUAC, head circumference, sub-scapular skinfold and triceps skinfold.
#' Note that the function might fail or return NAs when the variables are outside the ranges
#' given in the WGS (WHO Child Growth Standards) reference (i.e. 45 to 120 cm for height and
#' 0 to 60 months for age). It is up to the user to check the ranges and the units of their 
#' data.
#' @param sex the name of the binary variable that indicates the sex of the subject. This must
#' be coded as 1 = male and 2 = female. If in your project the variable sex has different
#' levels, you should recode the levels to 1 for males and 2 for females using the 
#' \code{ds.recodeValues} DataSHIELD function before the use of the \code{ds.getWGSR}.
#' @param firstPart Name of variable specifying:\cr
#' Weight (kg) for BMI/A, W/A, W/H, or W/L\cr
#' Head circumference (cm) for HC/A\cr
#' Height (cm) for H/A\cr
#' Length (cm) for L/A\cr
#' MUAC (cm) for MUAC/A\cr
#' Sub-scapular skinfold (mm) for SSF/A\cr
#' Triceps skinfold (mm) for TSF/A\cr
#' Give a quoted variable name as in (e.g.) "weight". Be careful with units (weight in kg;
#' height, length, head circumference, and MUAC in cm, skinfolds in mm).
#' @param secondPart Name of variable specifying:\cr
#' Age (days) for H/A, HC/A, L/A, MUAC/A, SSF/A, or TSF/A\cr
#' Height (cm) for BMI/A, or W/H\cr
#' Length (cm) for W/L\cr
#' Give a quoted variable name as in (e.g.) "age". Be careful with units (age in days;
#' height and length in cm).
#' @param index The index to be calculated and added to data. One of:\cr
#' bfa BMI for age\cr
#' hca Head circumference for age\cr
#' hfa Height for age\cr
#' lfa Length for age\cr
#' mfa MUAC for age\cr
#' ssa Sub-scapular skinfold for age\cr
#' tsa Triceps skinfold for age\cr
#' wfa Weight for age\cr
#' wfh Weight for height\cr
#' wfl Weight for length\cr
#' Give a quoted index name as in (e.g.) "wfh".
#' @param standing Variable specifying how stature was measured. If NA (default) then age (for "hfa"
#' or "lfa") or height rules (for "wfh" or "wfl") will be applied. This must be coded as
#' 1 = Standing; 2 = Supine; 3 = Unknown. Missing values will be recoded to 3 = Unknown. 
#' Give a single value (e.g."1"). If no value is specified then height and age rules will be applied.
#' @param thirdPart Name of variable specifying age (in days) for BMI/A. Give a quoted variable
#' name as in (e.g.) "age". Be careful with units (age in days).
#' @return \code{ds.getWGSR} assigns a numeric vector that includes the z-scores for the
#' specified index.
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#'
getWGSRDS <- function(sex, firstPart, secondPart, index, standing=NA, thirdPart=NA){
  
  sex <- eval(parse(text=sex), envir = parent.frame())
  firstPart <- eval(parse(text=firstPart), envir = parent.frame())
  secondPart <- eval(parse(text=secondPart), envir = parent.frame())
  if (!is.na(thirdPart)){
    thirdPart <- eval(parse(text=thirdPart), envir = parent.frame())
  }
  
  # access the internal reference data
  #wgsrData <- dsBase:::wgsrData
  #wgsrData <- zscorer:::wgsrData
  
  ## Avoid missing and impossible values in 'standing' by coding NA and other values to '3'
  if(is.na(standing) | !(standing %in% c(1, 2, 3))) {
    standing = 3
  }
  
  ## Unknown index specified - return NA
  if(!(index %in% c("bfa", "hca", "hfa", "lfa", "mfa", "ssa", "tsa", "wfa", "wfh", "wfl"))){
    stop("Please provide a correct index!", call.=FALSE)
  }
  
  ## Round lengths to nearest 0.1 cm
  if(index %in% c("wfh", "wfl")) {
    secondPart <- round(secondPart, 1)
  }
  
  ## Round ages to the nearest day
  if(index %in% c("hca", "hfa", "lfa", "mfa", "ssa", "tsa", "wfa")) {
    secondPart <- round(secondPart, digits = 0)
  }
  
  if(index == "bfa") {
    thirdPart <- round(thirdPart, 0)
  }
  
  input.index <- index
  
  z <- rep(NA, length(sex))
  
  for (i in 1:length(z)){
    
    index <- input.index
    
    # If 'thirdPart' (age) is missing for BMI-for-age return NA
    if(index == "bfa" & is.na(thirdPart[i])) {
      z[i] <- NA
    }else{
  
    ## 'secondPart' is zero then BMI cannot be calculated
    if(index == "bfa" & (secondPart[i] == 0 | is.na(secondPart[i]))){
      z[i] <- NA
    }else{
      
      ## 'sex' must be male (1) or female (2) - otherwise returns NA
      if(!(sex[i] %in% c('1', '2'))){
        z[i] <- NA
      }else{
      
        ## Missing data for 'sex', 'firstPart', or 'secondPart' - return NA
        if(is.na(sex[i]) | is.na(firstPart[i]) | is.na(secondPart[i])){
          z[i] <- NA
        }else{ 
        
          ## Rules for length-for-age and height-for-age indices
          if(standing == 1 & (index == "lfa" | index == "hfa") & secondPart[i] < 731) {
            index <- "lfa"
            firstPart[i] <- firstPart[i] + 0.7
          }
          if(standing == 2 & (index == "lfa" | index == "hfa") & secondPart[i] < 731) {
            index <- "lfa"
          }
          if(standing == 3 & (index == "lfa" | index == "hfa") & secondPart[i] < 731) {
            index <- "lfa"
          }
          if(standing == 1 & (index == "lfa" | index == "hfa") & secondPart[i] >= 731) {
            index <- "hfa"
          }
          if(standing == 2 & (index == "lfa" | index == "hfa") & secondPart[i] >= 731) {
            index <- "hfa"
            firstPart[i] <- firstPart[i] - 0.7
          }
          if(standing == 3 & (index == "lfa" | index == "hfa") & secondPart[i] >= 731) {
            index <- "hfa"
          }
          
          ## Rules for weight-for-length and weight-for-height indices
          if(standing == 1 & (index == "wfl" | index == "wfh") & secondPart[i] < 65) {
            index = "wfl"
            secondPart[i] <- secondPart[i] + 0.7
          }
          if(standing == 1 & (index == "wfl" | index == "wfh") & secondPart[i] >= 65) {
            index = "wfh"
          }
          if(standing == 2 & (index == "wfl" | index == "wfh") & secondPart[i] <= 110) {
            index = "wfl"
          }
          if(standing == 2 & (index == "wfl" | index == "wfh") & secondPart[i] > 110) {
            index = "wfh"
            secondPart[i] <- secondPart[i] - 0.7
          }
          if(standing == 3 & (index == "wfl" | index == "wfh") & secondPart[i] < 87) {
            index = "wfl"
          }
          if(standing == 3 & (index == "wfl" | index == "wfh") & secondPart[i] >= 87) {
            index = "wfh"
          }
          
          ## Rules for BMI-for-age index
          if(standing == 1 & index == "bfa" & thirdPart[i] < 731) {
            secondPart[i] <- secondPart[i] + 0.7
          }
          if(standing == 2 & index == "bfa" & thirdPart[i] >= 731) {
            secondPart[i] <- secondPart[i] - 0.7
          }
          ## Calculate BMI (as 'firstPart') and place age in 'secondPart'
          if(index == "bfa") {
            firstPart[i] <- firstPart[i] / (secondPart[i] / 100)^2
            secondPart[i] <- thirdPart[i]
          }
          ## If 'secondPart' is out of range for specified 'index' - return NA
          rangeSecondPart <- range(wgsrData$given[wgsrData$index == index])
          if(secondPart[i] < rangeSecondPart[1] | secondPart[i] > rangeSecondPart[2]) {
            z[i] <- NA
          }else{
          
            ## Lookup reference values and calculate z-score
            lkpIndexSex <- wgsrData[wgsrData$index == index & wgsrData$sex == sex[i], ]
            L <- stats::approx(lkpIndexSex$given, lkpIndexSex$l, xout = secondPart[i], ties = "ordered")$y
            M <- stats::approx(lkpIndexSex$given, lkpIndexSex$m, xout = secondPart[i], ties = "ordered")$y
            S <- stats::approx(lkpIndexSex$given, lkpIndexSex$s, xout = secondPart[i], ties = "ordered")$y
            z[i] <- (((firstPart[i] / M) ^ L) - 1) / (L * S)
            SD3pos <- M * (1 + L * S * (+3))^(1 / L)
            SD2pos <- M * (1 + L * S * (+2))^(1 / L)
            SD23pos <- SD3pos - SD2pos
            SD3neg <- M * (1 + L * S * (-3))^(1 / L)
            SD2neg <- M * (1 + L * S * (-2))^(1 / L)
            SD23neg <- SD2neg - SD3neg
            
            if(z[i] >  3) z[i] <-  3 + ((firstPart[i] - SD3pos) / SD23pos)
            if(z[i] < -3) z[i] <- -3 + ((firstPart[i] - SD3neg) / SD23neg)
          }  
        }  
      }    
    }
  }    
  }  
  return(z)
  
}
# ASSIGN FUNCTION
# getWGSRDS

