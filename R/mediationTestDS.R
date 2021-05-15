#'
#' @title The Sobel mediation test
#' @description This function is similar to the R function \code{mediation.test} that is included
#' in the \code{bda} package. The function computes statistics and p-values for three versions of
#' Sobel mediation test: Sobel test, Aroian test and Goodman test.
#' @details To test whether a mediator carries the influence on an independent variable to a 
#' dependent variable. Missing values will be automatically excluded with a warning.
#' @param mv a string character, the name of the mediator variable
#' @param iv a string character, the name of the independent variable
#' @param dv a string character, the name of the dependent variable
#' @return A table showing the values of the test statistics (z-values) and the corresponding
#' p-values for the Sobel, Aroian and Goodman tests. 
#' @author Demetris Avraam for DataSHIELD Development Team
#' @export
#' 
mediationTestDS <- function(mv,iv,dv){
  
  mv <- eval(parse(text=mv), envir = parent.frame())
  iv <- eval(parse(text=iv), envir = parent.frame())
  dv <- eval(parse(text=dv), envir = parent.frame())

  nm = length(mv); ni = length(iv); nd = length(dv);
  if(nm!=ni | nm!=nd | ni!=nd) stop("Variables have different lengths.")
  
  sele1 <- is.na(mv)
  sele2 <- is.na(iv)
  sele3 <- is.na(dv)
  sele <- sele1 | sele2 | sele3
  nmi <- sum(sele)
  if(nmi > 0){
    warning("missing value(s) removed")
  }
  
  if(nm==nmi){
    stop("all records contain missing values")
  }else if(nm - nmi < 5){
    stop("too few valid records to test")
  }else if(nm - nmi < 10){
    warning("the number of records might be too small")
  }
  
  tmp = summary(stats::lm(mv~iv));
  a = tmp$coef[2,1];sa=tmp$coef[2,2];
  tmp = summary(stats::lm(dv~mv+iv));
  b = tmp$coef[2,1];sb=tmp$coef[2,2];
  tmp1 = b^2*sa^2+a^2*sb^2
  tmp2 = sa^2*sb^2
  zsob = a*b/sqrt(tmp1);
  psob = stats::pnorm(-abs(zsob))*2;
  zaro = a*b/sqrt(tmp1+tmp2);
  paro = stats::pnorm(-abs(zaro))*2;
  if(tmp1>tmp2){
    zgm = a*b/sqrt(tmp1-tmp2)
    pgm = stats::pnorm(-abs(zgm))*2;
  }else{
    zgm=NA
    pgm=NA;
  }
  p.value=c(psob,paro,pgm)
  z.value = c(zsob,zaro,zgm)
  out = data.frame(rbind(z.value,p.value));
  names(out)=c("Sobel","Aroian","Goodman")
  out
}
# AGGREGATE FUNCTION
# mediationTestDS
