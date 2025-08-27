#' function for predicting a GAM model using raster input
#' 
#' @param object GAM model
#' @param newdata stars raster data used to make prediction
#' @param ... extra arguments for predict.gam
#' @return prediction raster
predict_raster = function(object, newdata, ...) {
  # Convert newdata to a tibble
  newdata_df = dplyr::as_tibble(newdata)
  
  # Get predictions as a matrix
  preds = predict(object, newdata_df, ...) |> 
    as.matrix()
  
  # Create a list of predictions for each column
  r = lapply(seq_len(ncol(preds)), function(i) {
    # Create a tibble with the first column from newdata and the corresponding prediction
    r = dplyr::select(newdata, 1) |> 
      rlang::set_names("pred") |> 
      dplyr::mutate(pred = as.vector(preds[, i]))
    return(r)
  })
  
  # Combine the results into a single output if there are multiple predictions
  if (length(r) > 1) {
    r = do.call(dplyr::bind_rows, r)
  } else {
    r = r[[1]]
  }
  
  # Name the predictions if there are multiple columns
  if (ncol(preds) > 1) {
    names(r) = colnames(preds)
  }
  
  return(r)
}

#' function for predicting a random forest model using raster input
#' 
#' @param object random forest model
#' @param newdata stars raster data used to make prediction
#' @param ... extra arguments for predict.rf
#' @return prediction raster
predict_rf_raster <- function(rf_object, newdata, ...) {
  # Convert stars object to a tibble for prediction
  newdata_df <- dplyr::as_tibble(newdata)
  
  # Get only predictor columns (not geometry)
  predictor_cols <- setdiff(names(newdata_df), attr(newdata, "dimensions")$geometry$names)
  predictors_only <- newdata_df[, predictor_cols]
  
  # Predict class probabilities
  preds <- predict(object, predictors_only, type = "prob")$.pred_1
  
  # Add predictions back to stars object
  result <- newdata[1]  # to get dimensions + geometry
  result$predicted_suitability <- preds
  
  return(result)
}  
