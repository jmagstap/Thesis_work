split_species_data <- function(species_data, species_name, train_prop = 0.7, seed = 123) {
  set.seed(seed)  # Ensure reproducibility
  
  # Randomly split the dataset
  split_index <- sample(seq_len(nrow(species_data)), size = floor(train_prop * nrow(species_data)))
  train_data <- species_data[split_index, ]
  test_data <- species_data[-split_index, ]
  
  # Create dynamic variable names
  train_var <- paste0(species_name, "_train_data")
  test_var <- paste0(species_name, "_test_data")
  
  # Assign to global environment
  assign(train_var, train_data, envir = .GlobalEnv)
  assign(test_var, test_data, envir = .GlobalEnv)
  
  # Optionally return a message
  message("Created variables: ", train_var, " and ", test_var)
}



####### fit gam model
#' use this function to fit your gam model with specified data
fit_gam_sdm <- function(train_data,
                        species_name,
                        formula = pa ~ s(bot_temp, k = 4) +
                          s(bot_salin, k = 4) +
                          s(topographic_position_index, k = 4) +
                          s(aspect, k = 4) +
                          s(bot_ph, k = 4),
                        family = binomial(link = "logit")) {
  
  # Fit the GAM model
  gam_model <- mgcv::gam(
    formula = formula,
    family = family,
    data = train_data
  )
  
  # Construct the dynamic variable name
  model_name <- paste0(species_name, "_gam_model")
  
  # Assign it to the global environment
  assign(model_name, gam_model, envir = .GlobalEnv)
  
  # Optionally return the model invisibly
  invisible(gam_model)
}

###### fit glm model
fit_glm_sdm <- function(train_data,
                        species_name,
                        formula = pa ~ bot_temp + bot_salin + topographic_position_index + aspect + bot_ph,
                        family = binomial(link = "logit")) {
  
  # Fit the logistic regression (GLM) model
  glm_model <- glm(
    formula = formula,
    family = family,
    data = train_data
  )
  
  # Construct a dynamic variable name
  model_name <- paste0(species_name, "_glm_model")
  
  # Assign it to the global environment
  assign(model_name, glm_model, envir = .GlobalEnv)
  
  # Optionally return the model invisibly
  invisible(glm_model)
}


evaluate_sdm_with_plots <- function(model, test_data, threshold = 0.5, model_type = "gam") {
  # Predict probabilities from model (GAM or GLM)
  preds <- predict(model, newdata = test_data, type = "response")
  
  # Add predictions and true class
  test_data <- test_data %>%
    mutate(
      predicted_prob = preds,
      predicted_class = ifelse(predicted_prob >= threshold, 1, 0),
      truth = factor(pa, levels = c(0, 1)),
      prediction = factor(predicted_class, levels = c(0, 1))
    )
  
  # Metrics
  acc <- accuracy(test_data, truth = truth, estimate = prediction)
  sensitivity <- sens(test_data, truth = truth, estimate = prediction)
  specificity <- spec(test_data, truth = truth, estimate = prediction)
  
  # ROC/AUC
  test_data$pa <- factor(test_data$pa, levels = c(0, 1))
  test_data$predicted_prob_flipped <- 1 - test_data$predicted_prob
  roc_curve_vis <- roc_curve(test_data, truth = pa, predicted_prob_flipped)
  auc_value <- roc_auc(test_data, truth = pa, predicted_prob_flipped)$.estimate
  
  # ROC plot
  roc_curve_plot <- ggplot(roc_curve_vis, aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(color = "black") +
    geom_abline(lty = 3, color = "gray") +
    coord_equal() +
    theme_bw() +
    labs(
      title = paste("ROC Curve with AUC -", toupper(model_type)),
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)"
    ) +
    annotate(
      "text",
      x = 0.65,
      y = 0.1,
      label = paste0("AUC = ", round(auc_value, 3)),
      size = 5,
      fontface = "bold"
    )
  
  # Probability histogram
  hist(test_data$predicted_prob[test_data$pa == 1],
       breaks = 20, col = rgb(0, 0, 1, 0.5), xlim = c(0, 1),
       main = paste(toupper(model_type), "Predicted Probabilities"), xlab = "Probability")
  
  hist(test_data$predicted_prob[test_data$pa == 0],
       breaks = 20, col = rgb(1, 0, 0, 0.5), add = TRUE)
  
  legend("topright", legend = c("Presence", "Absence"),
         fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)))
  
  return(list(
    test_data = test_data,
    accuracy = acc,
    sensitivity = sensitivity,
    specificity = specificity,
    roc_curve = roc_curve_plot
  ))
}

evaluate_species_models <- function(species_data, species_name) {
  
  # 1. Split training and testing data
  split_species_data(species_data, species_name)
  
  # Create dynamic variable names
  train_data <- get(paste0(species_name, "_train_data"))
  test_data  <- get(paste0(species_name, "_test_data"))
  
  # 2. Fit GAM and GLM
  fit_gam_sdm(train_data, species_name)
  fit_glm_sdm(train_data, species_name)
  
  gam_model <- get(paste0(species_name, "_gam_model"))
  glm_model <- get(paste0(species_name, "_glm_model"))
  
  # 3. Evaluate and plot
  evaluate_sdm_with_plots(gam_model, test_data)
  
  # 4. Predict on test data
  gam_pred <- predict(gam_model, newdata = test_data, type = "response")
  glm_pred <- predict(glm_model, newdata = test_data, type = "response")
  obs <- test_data$pa
  
  # 5. ROC curves
  roc_gam <- roc(obs, gam_pred)
  roc_glm <- roc(obs, glm_pred)
  
  roc_gam_df <- data.frame(
    specificity = roc_gam$specificities,
    sensitivity = roc_gam$sensitivities,
    model = "GAM"
  )
  
  roc_glm_df <- data.frame(
    specificity = roc_glm$specificities,
    sensitivity = roc_glm$sensitivities,
    model = "GLM"
  )
  
  roc_curve_vis <- bind_rows(roc_gam_df, roc_glm_df)
  
  # 6. AUC values
  auc_values <- data.frame(
    model = c("GAM", "GLM"),
    auc = c(auc(roc_gam), auc(roc_glm))
  )
  
  # 7. ROC Plot
  roc_curve_plot <- ggplot(roc_curve_vis, aes(x = 1 - specificity, y = sensitivity, color = model)) +
    geom_path(size = 1) +
    geom_abline(linetype = "dashed", color = "gray") +
    coord_equal() +
    theme_bw() +
    labs(
      title = paste("ROC Curves for", tools::toTitleCase(species_name), "Models"),
      x = "1 - Specificity (False Positive Rate)",
      y = "Sensitivity (True Positive Rate)",
      color = "Model"
    ) +
    scale_color_manual(
      values = c("GAM" = "blue", "GLM" = "red"),
      labels = paste0(auc_values$model, " (AUC = ", round(auc_values$auc, 3), ")")
    )
  
  print(roc_curve_plot)
}