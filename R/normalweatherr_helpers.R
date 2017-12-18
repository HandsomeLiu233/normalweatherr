#' Function to get weekday number from a date where \code{1} is Monday and 
#' \code{7} is Sunday. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Date vector.
#' 
#' @param as.factor Should the return be a factor? 
#' 
#' @return Numeric vector.
#' 
wday_monday <- function(x, as.factor = FALSE) {
  
  x <- lubridate::wday(x)
  x <- x - 1
  x <- ifelse(x == 0, 7, x)
  if (as.factor) x <- factor(x, levels = 1:7, ordered = TRUE)
  return(x)
  
}


#' Function to get a vector of random rows for data frame sampling, usually for
#' creating training and testing data frames for models. 
#' 
#' Use \code{set.seed} before the function to make the random number generation
#' reproducible. 
#' 
#' @param df Data frame to get random rows from. 
#' 
#' @param fraction Fraction of \code{df} to sample. 
#' 
#' @return Integer vector. 
#' 
#' @author Stuart K. Grange
#' 
random_rows <- function(df, fraction = 0.8) {
  
  # Get n
  n <- nrow(df)
  
  # Sample
  rows <- sample(n, round(n * fraction))
  
  return(rows)
  
}



#' @importFrom openair timeAverage
#' @export
openair::timeAverage


mode_average <- function (x, na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Function to predict using a model object and return a vector. 
#' 
#' @param model Model object which can be used with \code{\link{predict}}. 
#' 
#' @param df Data frame containing input data which can be used by \code{model},
#' not always needed. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Numeric vector. 
#' 
#' @examples
#' \dontrun{
#' 
#' # Predict using a random forest model
#' value_predict <- make_prediction(list_random_forest, data_testing_set)
#' 
#' }
#' 
#' @export
make_prediction <- function(model, df) {
  
  # Get class of model object
  model_class <- class(model)[1]
  
  # Different prediction logic dependent on model type
  if (model_class == "gam") {
    
    # Seems to be generic
    x <- unname(predict(model, df))
    
    # A one dimensional matrix? A little odd, drop
    attr(x, "dim") <- NULL
    
  } else if (model_class == "randomForest.formula") {
    
    # Can be generic, but use name space for when the package is not loaded
    x <- unname(randomForest:::predict.randomForest(model, df))
    
  } else if (model_class == "ksvm") {
    
    # Not generic and returns a matrix
    x <- unname(kernlab::predict(model, df))[, 1]
    
  } else if (model_class == "gbm") {
    
    # Use a vector but needs an extra argument, comes from model object
    x <- gbm::predict.gbm(
      model, 
      df, 
      n.trees = length(model$trees)
    )
    
  } else if (model_class == "lm") {
    
    x <- unname(predict(model, df))
    
  } else {
    
    stop("Model not recognised.", call. = FALSE)
    
  }
  
  return(x)
  
}

# from Stuart's enlightenr package
#' @export
extract_rf_mse <- function(list_model, na.rm = FALSE)
  min(list_model$mse, na.rm = na.rm)


#' @export
extract_rf_r_squared <- function(list_model, na.rm = FALSE)
  max(list_model$rsq, na.rm = na.rm) * 100