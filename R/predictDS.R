#'
#' @title predictDS
#' @description Generates server-side predictions using the client-side output from \code{ds.glm}.
#' 
#' @details 
#' This function uses the components supplied by the client-side function (coefficients, family, formula, 
#' and any categorical variables) to generate predictions on the server. To use the base R \code{predict()} function, 
#' a "dummy" glm object is created using the same model formula, family, and link function as the original model. 
#' The dummy model's coefficients are then replaced with the client-side coefficient estimates.
#'
#' To avoid mismatches between the factors used in the original glm and those in the dummy glm, the categorical 
#' variables saved by the client-side function are applied to the newdata.
#'
#' For intercept-only models, the function simply returns a vector of predicted values equal to the model intercept, with the appropriate length 
#' based on the row length of \code{newdataname}.
#' 
#' 
#' @param newdataname A character string specifying the name of the new dataset to be used for predictions.
#' @param traindataname A character string specifying the name of the dataset used for model training.
#' @param type A character string specifying the type of prediction. Options are \code{"response"} or \code{"link"}.
#' @param na.action A character string to specify the action to take if missing values are present. Default is \code{"na.pass"}.
#' 
#' 
#' @author Zulal Bekerecioglu
#' @return a numeric vector containing the predicted values
#' @export
#'
#'
predictDS <- function(newdataname, traindataname, type = c("response", "link"), 
                      na.action = "na.pass") {
  
  # Get the objects saved by the client function ds.predict
  coefficients<-get("predictDS_coefficients")
  model_formula <-get("predictDS_formula")
  family <- get("predictDS_family")
  categorical_variables <- get("predictDS_categorical_variables")
  
  
  
  if(!is.null(traindataname))
  {
    traindata<-get(traindataname)
    
  }else{
    stop("'traindataname' couldn't be found, please provide a valid object name.", call.=FALSE)
  }
  
  
  if(!is.null(newdataname))
  {
    newdf<-get(newdataname)
    
  }else{
    stop("'newdataname' couldn't be found, please provide a valid object name.", call.=FALSE)
  }
  
  if (!is.character(type) || !(type %in% c("link","response"))) {
    stop("Invalid argument. Must be one of: 'link','response'.", call. = FALSE)
  }
  
  # Convert the family object to it's corresponding function, i.e. poisson.link.log -> poisson(link= "log")
  family_dist <- strsplit(family, "\\.")[[1]]
  
  family_name <- family_dist[1]   # "binomial"
  link_name   <- family_dist[3]   # "logit"
  
  family_func <- get(family_name)  # gets the function binomial()
  family_obj  <- family_func(link = link_name)
  
  
  
  # SPECIAL CASE HANDLING: y ~ 1, intercept only ######################
  # A numeric vector will be created wit the mean, with the same length as the row number in newdataname 
  special_case <- length(attr(terms(formula(model_formula)), "term.labels")) == 0
  
  if(special_case){
    intercept <- coefficients
    
    # If the input is just a numeric vector, get the length
    if(class(newdf)=="numeric"){
      predictions.f <- rep(intercept, length(newdf))
    } else if(class(newdf)=="data.frame"){
      predictions.f <- rep(intercept, nrow(newdf)) # Otherwise use the number of rows
    } else {
      stop("Invalid input: The object called 'newdataname' must be either a numeric vector or a data.frame.",, call. = FALSE)
    }
    
    if (type == "link") {
      predictions.f <- predictions.f
    } else if (family_name == "gaussian") { # if type is 'response'
      # identity
      predictions.f <- predictions.f  
    } else if (family_name == "poisson") {
      # log
      predictions.f <- exp(predictions.f)     
    } else if (family_name == "binomial") {
      # logit
      predictions.f <- plogis(predictions.f)   
    } else {
      stop("Unsupported family for intercept-only prediction: Family must be either Gaussian, Poisson, or Binomial.", call. = FALSE)
    }
    
    return(predictions.f)
  }
  # END OF SPECIAL CASE ######################
  
  
  
  # Fix factor levels if any exists
  for (var in categorical_variables) {
    
    # First get all the factor levels from the train data
    traindata[[var]] <- factor(traindata[[var]])
    
    # Ensure new data variable is a factor with the SAME levels (this is needed if the newdf is missing some categories)
    newdf[[var]] <- factor(newdf[[var]],
                           levels = levels(traindata[[var]]))
  }
  
  # Get the na.action argument
  na.action.fun <- match.fun(na.action)
  
  # Use a dummy glm object with the correct formula and family
  dummy_fit <- glm(model_formula,
                   data = traindata,
                   family = family_obj,
                   control = glm.control(maxit = 1))
  
  # Change its coefficients with the correct ones
  names(coefficients) <- names(dummy_fit$coefficients)
  dummy_fit$coefficients <- coefficients
  
  # New predictions
  prediction <- predict(dummy_fit, newdata = newdf, type = type, na.action = na.action.fun)
  
  return(prediction)
}
 