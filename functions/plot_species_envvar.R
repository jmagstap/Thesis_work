#' this function takes species data and creates freq_data and presence_data to use to create various plots and maps to see relationships with env variables 
#' @param species_data identify "species_sdm_data" that you have loaded into your environment
#' @param env_var Specify the environmental variable youn want to show relationship with (ex. bot_salin)
plot_species_envvar <- function(species_data, env_var) {
  # Use tidy eval to select the environmental variable column
  freq_data <- species_data %>%
    group_by({{ env_var }}, pa, geom) %>%
    summarise(freq = n(), .groups = 'drop') %>%
    st_as_sf()
  
  presence_data <- species_data %>%
    filter(pa == 1) %>%
    group_by({{ env_var }}) %>%
    summarise(freq = n(), .groups = 'drop')
  
  # Convert the env_var to a string for labels and aes
  env_var_label <- rlang::as_name(rlang::enquo(env_var))
  
  # Plot 1: Presence frequency points by env variable
  p1 <- ggplot(presence_data, aes(x = !!rlang::sym(env_var_label), y = freq)) +
    geom_point(color = "red", size = 3) +
    labs(x = env_var_label, y = "Frequency of Presence") +
    theme_minimal()
  
  # Plot 2: Histogram of env variable for presences
  p2 <- ggplot(presence_data, aes(x = !!rlang::sym(env_var_label))) +
    geom_histogram(binwidth = 0.1, fill = "red", color = "black") +
    labs(x = "", y = "Count", title = paste("Histogram of", env_var_label, "for Presences")) +
    theme_minimal()
  
  # Plot 3: Frequency of presence/absence by env variable
  p3 <- ggplot(freq_data, aes(x = !!rlang::sym(env_var_label), y = freq, color = factor(pa))) +
    geom_point(size = 1) +
    labs(x = env_var_label, y = "Frequency", color = "Presence") +
    scale_color_manual(values = c("0" = "blue", "1" = "red"),
                       labels = c("Absence", "Presence")) +
    theme_minimal()
  
  # Plot 4: Map of presence and absence points
  p4 <- ggplot(freq_data) +
    geom_sf(aes(color = factor(pa)), size = 0.5, alpha = 0.3) +
    scale_color_manual(
      name = "Presence",
      values = c("0" = "gray60", "1" = "blue"),
      labels = c("Absence", "Presence")
    ) +
    theme_minimal() +
    labs(title = "Species Presence and Absence Locations")
  
  list(
    freq_data = freq_data,
    presence_data = presence_data,
    plot_presence_points = p1,
    plot_presence_histogram = p2,
    plot_freq_points = p3,
    plot_map = p4
  )
}
