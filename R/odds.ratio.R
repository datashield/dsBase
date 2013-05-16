#' Calculates odds ratio and its 95 percent confidence interval
#' 
#' @param xvect a factor vector
#' @param yvect a factor vector
#' @param alpha level of statistical significance
#' @export
#' 
odds.ratio <- function (xvect, yvect, alpha=0.05) {
  
  ####
  # Calculates odds ratio and its 95% confidence interval
  # xvect and yvect should be preferrably factor vectors with level names
  # The factors should be organised in such a way that the first level is the reference level.
  # E.g.
  #      y=0  y=1
  # x=0  n00  n01
  # x=1  n10  n11
  ####
  
  # Check whether xvect and yvect are factor vectors
  if (is.factor(xvect)==F & is.factor(yvect)==F)
    warning('vectors supplied are not factor vectors') else
      if (is.factor(xvect)==F)
        warning('xvect supplied is not a factor vector') else
          if (is.factor(yvect)==F)
            warning('yvect supplied is not a factor vector')
  
  freq.tab = table(xvect, yvect)
  num.exp = dim(freq.tab)[1]
  OR = matrix(0, num.exp, 1)
  OR[1] = 1.00
  levelsX = sort(unique(xvect))
  
  if (freq.tab[1,1]==0)
    stop('Negative outcomes at reference exposure level are absent: Odds ratio cannot be calculated.')
  else if (freq.tab[1,2]==0)
    stop('Positive outcomes at reference exposure level are absent: Odds ratio cannot be calculated.')
  else
    for (i in 2:num.exp) {
      if (freq.tab[i,1]==0) {
        txt = paste('Negative outcomes at exposure level ', levelsX[i], ' are absent: Odds ratio cannot be calculated.')
        stop(txt)
      }
      else OR[i] = (freq.tab[i,2]/freq.tab[i,1])/(freq.tab[1,2]/freq.tab[1,1])
    }
  
  OR = round(OR, 3)
  
  # Wald CI's for odds ratio
  
  CIs = matrix(NA, num.exp, 2)
  for (i in 2:num.exp) {
    if ( (OR[i]!=0) & (!is.na(OR[i])) ) {
      se = sqrt((1/freq.tab[1,1])+(1/freq.tab[1,2])+(1/freq.tab[i,1])+(1/freq.tab[i,2]))
      zalph <- qnorm(1 - alpha/2)
      logOR <- log(OR[i])
      log.lo <- logOR - zalph * se
      log.hi <- logOR + zalph * se
      CIs[i,1] = exp(log.lo)
      CIs[i,2] = exp(log.hi)
    }
    else if (OR[i]==0) {
      txt = c('Confidence intervals at exposure level ', i, ' cannot be calculated as some entries are missing (or non-valid entries, i.e. <4 observations)')
      warning(txt)
    }
  }
  CIs = round(CIs, 3)
  
  ORframe = data.frame(OR=OR, LowerCI = CIs[,1], UpperCI = CIs[,2])
  if (is.factor(xvect)==T)
    row.names(ORframe) = levels(xvect) else
      row.names(OR) = as.character(levelsX)
  
  cat('\nOdds ratios and Wald', (1-alpha)*100, '% confidence intervals\n\n')
  print(ORframe)
  
}
