#' 
#' @title Converts birth measurements to intergrowth z-scores/centiles
#' @description Converts birth measurements to INTERGROWTH z-scores/centiles (generic)
#' @param gagebrth the name of the "gestational age at birth in days" variable.
#' @param z z-score(s) to convert (must be between 0 and 1). Default value is 0.
#' This value is used only if \code{fun} is set to "igb_zscore2value".
#' @param p centile(s) to convert (must be between 0 and 100). Default value is p=50. 
#' This value is used only if \code{fun} is set to "igb_centile2value".
#' @param val the name of the anthropometric variable to convert.
#' @param var the name of the measurement to convert ("lencm", "wtkg", "hcircm", "wlr")
#' @param sex the name of the sex factor variable. The variable should be coded as Male/Female.
#' If it is coded differently (e.g. 0/1), then you can use the ds.recodeValues function to 
#' recode the categories to Male/Female before the use of ds.igb_standards
#' @param fun the name of the function to be used. This can be one of: "igb_centile2value",
#' "igb_zscore2value", "igb_value2zscore" (default), "igb_value2centile".
#' @note For gestational ages between 24 and 33 weeks, the INTERGROWTH very early preterm 
#' standard is used.
#' @return assigns the converted measurement as a new object on the server-side
#' @author Demetris Avraam for DataSHIELD Development Team
#' @import dplyr
#' @importFrom gamlss.dist qST3 pST3
#' @export
#' 
igb_standardsDS <- function(gagebrth=gagebrth, z=z, p=p, val=val, var=var, sex=sex, fun=fun){
  
    ## Internal function #1: igb_centile2value
    igb_centile2value <- function(gagebrth, p = 50, var = "lencm", sex = "Female") {
      
      dat <- data.frame(x = gagebrth, p = p, var = var, sex = sex,
                        stringsAsFactors = FALSE)
      
      if (! all(unique(dat$sex[!is.na(dat$sex)]) %in% c("Male", "Female")))
        stop("sex must be 'Male' or 'Female'")
      
      if (! all(var %in% c("lencm", "wtkg", "hcircm", "wlr")))
        stop("'var' must be one of 'lencm', 'wtkg', 'hcircm', 'wlr'")
      
      # since coefficients are available only by pair/sex
      # we need to call this for each unique combination
      ig_centile2value_single_pars <- function(x, y, var, sex) {
        if (var == "wlr") {
          x <- x / 7
          if (sex == "Male") {
            res <- -17.84615 + (-3778.768 * ( x ^ (-1))) + (1291.477 * ( (x ^ (-1)) * log(x))) +
              (stats::qnorm(y / 100) * (1.01047 +  (-0.0080948 * x)))
          } else {
            res <- -5.542927 + (0.0018926 * (x ^ 3)) + (-0.0004614 * ( (x ^ 3) * log(x))) +
              (stats::qnorm(y / 100) * 0.6806229)
          }
        } else {
          coefs <- ig_coefs[[var]][[sex]]
          # the best we can do is daily resolution
          x <- as.integer(round(x))
          idx <- match(x, coefs$ga)
          na_idx <- is.na(idx)
          res <- rep(NA, length(idx))
          res[!na_idx] <- gamlss.dist::qST3(y[!na_idx] / 100,
                                            coefs$mu[idx][!na_idx],
                                            coefs$sigma[idx][!na_idx],
                                            coefs$nu[idx][!na_idx],
                                            coefs$tau[idx][!na_idx])
        }
        
        res
      }
      
      ig_centile2value_single_pars_e <- function(x, y, var, sex) {
        if (var == "wlr") {
          x <- x / 7
          if (sex == "Male") {
            res <- (0.1382809 + 3.400617) + (-0.0103163 * x ^ 2) + (0.0003407 * x ^ 3) +
              (stats::qnorm(y / 100) * sqrt(0.3570057))
          } else {
            res <- 3.400617 + (-0.0103163 * x ^ 2) + (0.0003407 * x ^ 3) +
              (stats::qnorm(y / 100) * sqrt(0.3570057))
          }
        } else {
          coefs <- ig_early_coefs[[var]]
          
          frm <- matrix(c(
            rep(1, length(x)),
            x / 7,
            rep(as.integer(sex == "Male"), length(x))),
            ncol = 3)
          if (var == "wtkg") {
            frm[, 2] <- sqrt(frm[, 2])
          }
          
          mu <- as.vector(frm %*% coefs$coefs)
          res <- stats::qnorm(y / 100, mu, coefs$sigma)
          if (var == "wtkg") {
            res <- exp(res)
          }
        }
        res
      }
      
      dat <- data.frame(dat %>%
                          dplyr::group_by(var, sex) %>%
                          dplyr::mutate(res = ig_centile2value_single_pars(x, p, var[1], sex[1])))
      
      # if born earlier than 33 weeks, use early preterm standard
      idx <- which(dat$x < 33 * 7 & dat$x >= 24 * 7)
      if (length(idx) > 0) {
        edat <- dat[idx, ]
        edat <- edat %>%
          dplyr::group_by(var, sex) %>%
          dplyr::mutate(res = ig_centile2value_single_pars_e(x, p, var[1], sex[1]))
        
        dat$res[idx] <- edat$res
      }
      
      dat$res[dat$x < 24 * 7] <- NA
      dat$res[dat$x > 42 * 7 + 6] <- NA
      
      dat$res
    }
    
  ## Internal function #2: igb_zscore2value
  igb_zscore2value <- function(gagebrth, z = 0, var = "lencm", sex = "Female") {
    igb_centile2value(gagebrth, p = 100 * stats::pnorm(z), var = var, sex = sex)
  }
  
  ## Internal function #3: igb_value2centile
  igb_value2centile <- function(gagebrth, val, var = "lencm", sex = "Female") {
    
    dat <- data.frame(x = gagebrth, y = val, var = var, sex = sex, stringsAsFactors = FALSE)
    
    if (! all(unique(dat$sex) %in% c("Male", "Female")))
      stop("sex must be 'Male' or 'Female'")
    
    if (! all(var %in% c("lencm", "wtkg", "hcircm", "wlr")))
      stop("'var' must be one of 'lencm', 'wtkg', 'hcircm', 'wlr'")
    
    # since coefficients are available only by pair/sex
    # we need to call this for each unique combination
    ig_value2centile_single_pars <- function(x, y, var, sex) {
      if (var == "wlr") {
        x <- x / 7
        if (sex == "Male") {
          nn <- function(x) -17.84615 + (-3778.768 * ( x ^ (-1))) +
            (1291.477 * ( (x ^ (-1)) * log(x)))
          dd <- function(x) (1.01047 +  (-0.0080948 * x))
          z <- (y - nn(x)) / dd(x)
          res <- stats::pnorm(z) * 100
        } else {
          nn <- function(x) -5.542927 + (0.0018926 * (x ^ 3)) +
            (-0.0004614 * ( (x ^ 3) * log(x)))
          dd <- 0.6806229
          z <- (y - nn(x)) / dd
          res <- stats::pnorm(z) * 100
        }
      } else {
        coefs <- ig_coefs[[var]][[sex]]
        # the best we can do is daily resolution
        x <- as.integer(round(x))
        idx <- match(x, coefs$ga)
        na_idx <- is.na(idx)
        res <- rep(NA, length(idx))
        res[!na_idx] <- gamlss.dist::pST3(y[!na_idx],
                                          coefs$mu[idx][!na_idx],
                                          coefs$sigma[idx][!na_idx],
                                          coefs$nu[idx][!na_idx],
                                          coefs$tau[idx][!na_idx]) * 100
      }
      
      res
    }
    
    ig_value2centile_single_pars_e <- function(x, y, var, sex) {
      if (var == "wlr") {
        x <- x / 7
        if (sex == "Male") {
          nn <- function(x) (0.1382809 + 3.400617) + (-0.0103163 * x ^ 2) + (0.0003407 * x ^ 3)
          dd <- sqrt(0.3570057)
          z <- (y - nn(x)) / dd
          res <- stats::pnorm(z) * 100
        } else {
          nn <- function(x) 3.400617 + (-0.0103163 * x ^ 2) + (0.0003407 * x ^ 3)
          dd <- sqrt(0.3570057)
          z <- (y - nn(x)) / dd
          res <- stats::pnorm(z) * 100
        }
      } else {
        coefs <- ig_early_coefs[[var]]
        
        frm <- matrix(c(
          rep(1, length(x)),
          x / 7,
          rep(as.integer(sex == "Male"), length(x))),
          ncol = 3)
        if (var == "wtkg") {
          frm[, 2] <- sqrt(frm[, 2])
          y <- log(y)
        }
        
        mu <- as.vector(frm %*% coefs$coefs)
        res <- stats::pnorm(y, mu, coefs$sigma) * 100
      }
      res
    }
    
    dat <- dat %>%
      dplyr::group_by(var, sex) %>%
      dplyr::mutate(res = ig_value2centile_single_pars(x, y, var[1], sex[1]))
    
    # if born earlier than 33 weeks, use early preterm standard
    idx <- which(dat$x < 33 * 7 & dat$x >= 24 * 7)
    if (length(idx) > 0) {
      edat <- dat[idx, ]
      edat <- edat %>%
        dplyr::group_by(var, sex) %>%
        dplyr::mutate(res = ig_value2centile_single_pars_e(x, y, var[1], sex[1]))
      
      dat$res[idx] <- edat$res
    }
    
    dat$res[dat$x < 24 * 7] <- NA
    dat$res[dat$x > 42 * 7 + 6] <- NA
    
    dat$res
  }
  
  ## Internal function #4: igb_value2zscore
  igb_value2zscore <- function(gagebrth, val, var = "lencm", sex = "Female") {
    stats::qnorm(igb_value2centile(gagebrth, val, var = var, sex = sex) / 100)
  }
  
  
  # Start the analysis
  if(is.character(gagebrth)){
    gagebrth <- eval(parse(text = gagebrth), envir = parent.frame())
  }
  if(is.character(val)){
    val <- eval(parse(text = val), envir = parent.frame())
  }
  if(is.character(sex)){
    sex <- eval(parse(text = sex), envir = parent.frame())
  }
  
  x.out <- rep(NA, t=length(sex))
  idx.males <- which(sex=="Male")
  idx.females <- which(sex=="Female")
  
  if(fun=='igb_centile2value'){
    res.males <- igb_centile2value(gagebrth=gagebrth[idx.males], p=p, var = var, sex = "Male")
    res.females <- igb_centile2value(gagebrth=gagebrth[idx.females], p=p, var = var, sex = "Female")
    x.out[idx.males] <- res.males
    x.out[idx.females] <- res.females
  }  
  if(fun=='igb_zscore2value'){
    res.males <- igb_zscore2value(gagebrth=gagebrth[idx.males], z=z, var = var, sex = "Male")
    res.females <- igb_zscore2value(gagebrth=gagebrth[idx.females], z=z, var = var, sex = "Female")
    x.out[idx.males] <- res.males
    x.out[idx.females] <- res.females
  }
  if(fun=='igb_value2zscore'){
    res.males <- igb_value2zscore(gagebrth = gagebrth[idx.males], val = val[idx.males], var = var, sex = "Male")
    res.females <- igb_value2zscore(gagebrth = gagebrth[idx.females], val = val[idx.females], var = var, sex = "Female")
    x.out[idx.males] <- res.males
    x.out[idx.females] <- res.females
  }  
  if(fun=='igb_value2centile'){
    res.males <- igb_value2centile(gagebrth = gagebrth[idx.males], val = val[idx.males], var = var, sex = "Male")
    res.females <- igb_value2centile(gagebrth = gagebrth[idx.females], val = val[idx.females], var = var, sex = "Female")
    x.out[idx.males] <- res.males
    x.out[idx.females] <- res.females
  }
   
  # assign the outcome to the data servers
  return(x.out)
  
}  
