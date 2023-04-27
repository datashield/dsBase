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
#' @param age the name of the age variable in months.
#' @param height the name of the height variable in cm
#' @param bp the name of the blood pressure variable.
#' @param systolic logical. If TRUE (default) the function assumes conversion of 
#' systolic blood pressure. If FALSE the function assumes conversion of diastolic 
#' blood pressure.
#' @return assigns a new object on the server-side. The assigned object is a list 
#' with two elements: the 'Zbp' which is the zscores of the blood pressure and 'perc'
#' which is the percentiles of the BP zscores.
#' @note The z-scores of height are calculated using the code from the CDC-DNPAO/CDCAnthro
#' Github repository: https://github.com/CDC-DNPAO/CDCAnthro
#' @author Demetris Avraam for DataSHIELD Development Team
#' @rawNamespace import(data.table, except = c(first, last, between))
#' @importFrom stats approx pnorm qnorm sigma
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
  
  # internal functions from cdcanthro
  
  .c <- function (...) as.character(substitute(c(...))[-1L])
  
  set_cols_first <- function (DT, cols, intersection = TRUE) # thanks to hutils
  {
    if (intersection) {
      return(setcolorder(DT, c(intersect(cols, names(DT)),
                               setdiff(names(DT), cols))))
    }
    else {
      return(setcolorder(DT, c(cols, setdiff(names(DT), cols))))
    }
  }
  
  cz_score=function(var, l, m, s){ # LMS formula with modified (m) z-scores
    ls=l*s; invl=1/l
    z = (((var/m) ^ l) -1) / (ls) # z-score formula
    sdp2 = (m * (1 + 2*ls) ^ (invl)) - m; # BMI at modified z-score of 2 SDs
    sdm2 = m - (m * (1 - 2*ls) ^ (invl));
    mz=fifelse(var < m, (var - m)/(0.5*sdm2),
               (var - m)/(sdp2*0.5) )
    list(z, mz)
  }
  
  cdcanthro <- function(data, age = age_in_months, wt = weight_kg, ht = height_cm, 
                         bmi = bmi, all = FALSE){
    age_in_months <- weight <- height <- seq_ <- sex <- agey <- bz <- lwt2 <- mwt2 <- swt2 <- lbmi2 <- mbmi2 <- sbmi2 <- lht2 <- mht2 <- sht2 <- lwt1 <- mwt1 <- swt1 <- lbmi1 <- mbmi1 <- sbmi1 <- lht1 <- mht1 <- sht1 <- mbmi <- lbmi <- sbmi <- mref <- sref <- denom <- weight_kg <- height_cm <- bmiz <- l <- m <- s <- waz <- haz <- z1 <- z0 <- p95 <- bmip <- "_AGEMOS1" <- ebp <- ebz <- agemos <- agemos1 <- agemos2 <- sexn <- bmi_l <- bmi_s <- bmi_m <- NULL
    setDT(data)
    data$seq_ <- 1L:nrow(data)
    dorig <- copy(data)
    data$age <- data[[deparse(substitute(age))]]
    data$wt <- data[[deparse(substitute(wt))]]
    data$ht <- data[[deparse(substitute(ht))]]
    data$bmi <- data[[deparse(substitute(bmi))]]
    data[, `:=`(sexn, toupper(substr(sex, 1, 1)))]
    data[, `:=`(sexn, fcase(sexn %in% c(1, "B", "M"), 1L, sexn %in% 
                              c(2, "G", "F"), 2L))]
    data <- data[between(age, 24, 240) & !(is.na(wt) & is.na(ht)), 
                 .(seq_, sexn, age, wt, ht, bmi)]
    dref <- ref_data[`_AGEMOS1` > 23 & denom == "age"]
    names(dref) <- tolower(names(dref))
    names(dref) <- gsub("^_", "", names(dref))
    setnames(dref, "sex", "sexn")
    d20 <- dref[agemos2 == 240, .(sexn, agemos2, lwt2, mwt2, 
                                  swt2, lbmi2, mbmi2, sbmi2, lht2, mht2, sht2)]
    names(d20) <- gsub("2", "", names(d20))
    dref <- dref[, .(sexn, agemos1, lwt1, mwt1, swt1, lbmi1, 
                     mbmi1, sbmi1, lht1, mht1, sht1)]
    names(dref) <- gsub("1", "", names(dref))
    dref = rbindlist(list(dref, d20))
    dref[sexn == 1, `:=`(mref = 23.02029, sref = 0.13454)]
    dref[sexn == 2, `:=`(mref = 21.717, sref = 0.15297)]
    v = c("sexn", "age", "wl", "wm", "ws", "bl", "bm", "bs", 
          "hl", "hm", "hs", "mref", "sref")
    setnames(dref, v)
    if (length(setdiff(data$age, dref$age)) > 0) {
      uages = unique(data$age)
      uages
      db <- dref[sexn == 1]
      fapp <- function(v, ...) approx(db$age, v, xout = uages)$y
      db <- sapply(db[, ..v], fapp)
      dg <- dref[sexn == 2]
      fapp <- function(v, ...) approx(dg$age, v, xout = uages)$y
      dg <- sapply(dg[, ..v], fapp)
      dref <- setDT(data.frame(rbind(db, dg)))
    }
    du <- unique(data[, .(sexn, age)], by = c("sexn", "age"))
    dref <- dref[du, on = c("sexn", "age")]
    setkey(data, sexn, age)
    setkey(dref, sexn, age)
    dt <- dref[data]
    dt[, `:=`(c("waz", "mod_waz"), cz_score(dt$wt, dt$wl, dt$wm, 
                                            dt$ws))]
    dt[, `:=`(c("haz", "mod_haz"), cz_score(dt$ht, dt$hl, dt$hm, 
                                            dt$hs))]
    dt[, `:=`(c("bz", "mod_bmiz"), cz_score(dt$bmi, dt$bl, dt$bm, 
                                            dt$bs))]
    setDT(dt)
    setnames(dt, c("bl", "bm", "bs"), c("bmi_l", "bmi_m", "bmi_s"))
    dt[, `:=`(c("wl", "wm", "ws", "hl", "hm", "hs"), NULL)]
    dt[, `:=`(bmip = 100 * pnorm(bz), p50 = bmi_m * (1 + bmi_l * 
                                                       bmi_s * qnorm(0.5))^(1/bmi_l), p85 = bmi_m * (1 + bmi_l * 
                                                                                                       bmi_s * qnorm(0.85))^(1/bmi_l), p95 = bmi_m * (1 + bmi_l * 
                                                                                                                                                        bmi_s * qnorm(0.95))^(1/bmi_l), p97 = bmi_m * (1 + bmi_l * 
                                                                                                                                                                                                         bmi_s * qnorm(0.97))^(1/bmi_l), wap = 100 * pnorm(waz), 
              hap = 100 * pnorm(haz), z1 = ((bmi/bmi_m) - 1)/bmi_s, 
              z0 = log(bmi/bmi_m)/bmi_s)][, `:=`(dist_median = z1 * 
                                                   bmi_m * bmi_s, adj_dist_median = z1 * sref * mref, perc_median = z1 * 
                                                   100 * bmi_s, adj_perc_median = z1 * 100 * sref, log_perc_median = z0 * 
                                                   100 * bmi_s, adj_log_perc_median = z0 * 100 * sref, bmip95 = 100 * 
                                                   (bmi/p95))]
    dt[, `:=`(ebz = bz, ebp = bmip, agey = age/12)]
    dt[, `:=`(sigma, fifelse(sexn == 1, 0.3728 + 0.5196 * agey - 
                               0.0091 * agey^2, 0.8334 + 0.3712 * agey - 0.0011 * agey^2))]
    dt[bmip >= 95, `:=`(ebp, 90 + 10 * pnorm((bmi - p95)/round(sigma, 
                                                               8)))]
    dt[bmip >= 95, `:=`(ebz, qnorm(ebp/100))]
    dt[bmip > 99 & is.infinite(ebz), `:=`(ebz, 8.21)]
    x <- c("agey", "mref", "sref", "sexn", "wt", "ht", "bmi")
    dt[, `:=`((x), NULL)]
    setnames(dt, c("bz", "bmip", "ebp", "ebz"), c("original_bmiz", 
                                                  "original_bmip", "bmip", "bmiz"))
    v = c("seq_", "bmiz", "bmip", "waz", "wap", "haz", "hap", 
          "p50", "p95", "bmip95", "original_bmip", "original_bmiz", 
          "perc_median", "mod_bmiz", "mod_waz", "mod_haz")
    if (all == TRUE) {
      v = c(v, "bmi_l", "bmi_m", "bmi_s", "sigma", "adj_dist_median", 
            "dist_median", "adj_perc_median", "log_perc_median", 
            "adj_log_perc_median")
    }
    dt <- dt[, ..v]
    setkey(dt, seq_)
    setkey(dorig, seq_)
    dtot <- dt[dorig]
    set_cols_first(dtot, names(dorig))
    dtot[, `:=`(seq_, NULL)]
    return(dtot[])
  }

  
  data <- as.data.frame(cbind(sex=sex, age=age, height=height))
  
  # create fake weight and bmi variables
  data$weight <- rep(60, times=nrow(data))
  data$bmi <- data$weight / (data$height/100)^2
  
  # convert the height of h (inches) to a height Z-score relative to age and sex 
  # based on CDC growth charts
  dout <- cdcanthro(data=data, age=age, ht = height, wt=weight, bmi=bmi)
  Zht <- dout$haz
  
  # convert age in months to age in years
  age_y <- data$age/12
  
  # Compute the expected BP (systolic or diastolic) for males/females of age y years and height
  # h inches using the regression coefficients given in table B-1 in 
  # https://www.nhlbi.nih.gov/sites/default/files/media/docs/hbp_ped.pdf
  males_idx <- which(data$sex==1)
  females_idx <- which(data$sex==2)
  mu <- rep(NA, times=nrow(data))
  if(systolic==TRUE){
    mu[males_idx] <- 102.19768 + 1.82416 * (age_y[males_idx]-10) + 0.12776 * (age_y[males_idx]-10)^2 + 
      0.00249 * (age_y[males_idx]-10)^3 - 0.00135 * (age_y[males_idx]-10)^4 + 2.73157 * Zht[males_idx] - 
      0.19618 * Zht[males_idx]^2 - 0.04659 * Zht[males_idx]^3 + 0.00947 * Zht[males_idx]^4 
    mu[females_idx] <- 102.01027 + 1.94397 * (age_y[females_idx]-10) + 0.00598 * (age_y[females_idx]-10)^2 - 
      0.00789 * (age_y[females_idx]-10)^3 - 0.00059 * (age_y[females_idx]-10)^4 + 2.03526 * Zht[females_idx] + 
      0.02534 * Zht[females_idx]^2 - 0.01884 * Zht[females_idx]^3 + 0.00121 * Zht[females_idx]^4 
  }  
  if(systolic==FALSE){
    mu[males_idx] <- 61.01217 + 0.68314 * (age_y[males_idx]-10) - 0.09835 * (age_y[males_idx]-10)^2 + 
      0.01711 * (age_y[males_idx]-10)^3 + 0.00045 * (age_y[males_idx]-10)^4 + 1.46993 * Zht[males_idx] - 
      0.07849 * Zht[males_idx]^2 - 0.03144 * Zht[males_idx]^3 + 0.00967 * Zht[males_idx]^4 
    mu[females_idx] <- 60.50510 + 1.01301 * (age_y[females_idx]-10) + 0.01157 * (age_y[females_idx]-10)^2 - 
      0.00424 * (age_y[females_idx]-10)^3 - 0.00137 * (age_y[females_idx]-10)^4 + 1.16641 * Zht[females_idx] + 
      0.12795 * Zht[females_idx]^2 - 0.03869 * Zht[females_idx]^3 - 0.00079 * Zht[females_idx]^4 
  }    

  # convert the observed BP to a Z-score (Zbp) using sigmas given in table B-1
  Zbp <- rep(NA, times=nrow(data))
  if(systolic==TRUE){
    Zbp[males_idx] <- (bp[males_idx]-mu[males_idx])/10.7128
    Zbp[females_idx] <- (bp[females_idx]-mu[females_idx])/10.4855
  } 
  if(systolic==FALSE){
    Zbp[males_idx] <- (bp[males_idx]-mu[males_idx])/11.6032
    Zbp[females_idx] <- (bp[females_idx]-mu[females_idx])/10.9573
  } 
  
  # convert the bp Z-score to a percentile
  perc <- round(pnorm(Zbp)*100, digits=2)

  return(list(Zbp=Zbp, perc=perc))

}
