#' function for predicting a GAM model using raster input
#' 
#' @param object GAM model
#' @param newdata stars raster data used to make prediction
#' @param ... extra arguments for predict.gam
#' @return prediction raster
predict_raster = function(object, newdata, ...){
  newdata_df = dplyr::as_tibble(newdata)
  preds = predict(object, newdata_df, ...)
  r = dplyr::select(newdata, 1) |> 
    rlang::set_names("pred") |> 
    dplyr::mutate(pred = as.vector(preds))
  return(r)
}
  
