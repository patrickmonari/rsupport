
# Function for proportional stacked bar plots for full dataset, not averaged by pair

# Inputs:
# data: dataframe
# separate_by_treatment: boolean, include treatment split from "treatment" column in dataframe
# time_values: which observation periods from "time" column in dataframe
# major_type_values: which major USV types from "major_type" column in dataframe
# fill_by: filling bars by "label" or "major_type" columns in dataframe
# dataset_name: title, custom input


create_stacked_bar_plot_total <- function(data, separate_by_treatment = FALSE, time_values = NULL, major_type_values = NULL, dataset_name = "Dataset", fill_by = "label") {
  # Validate fill_by input
  if (!fill_by %in% c("label", "major_type")) {
    stop("Invalid fill_by value. Please use 'label' or 'major_type'.")
  }
  
  # Filter data for time_values if provided
  if (!is.null(time_values)) {
    data <- data %>%
      filter(time %in% time_values)
  }
  
  # Filter data for major_type_values if provided
  if (!is.null(major_type_values)) {
    data <- data %>%
      filter(major_type %in% major_type_values)
  }
  
  # Create the plot based on separate_by_treatment
  if (separate_by_treatment) {
    p <- data %>%
      ggplot(aes_string(x = "treatment", fill = fill_by)) + # Use aes_string for dynamic column names
      geom_bar(position = "fill") +
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE) +
      theme_cowplot() +
      theme(
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("Treatment") +
      ylab("Relative Proportion") +
      facet_wrap(~time) +
      ggtitle(paste("Data:", dataset_name)) # Add title to the plot
  } else {
    p <- data %>%
      ggplot(aes_string(x = "time", fill = fill_by)) + # Use aes_string for dynamic column names
      geom_bar(position = "fill") +
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE) +
      theme_cowplot() +
      theme(
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("Time") +
      ylab("Relative Proportion") +
      ggtitle(paste("Data:", dataset_name)) # Add title to the plot
  }
  
  return(p)
}





# Function for proportional stacked bar plots for full dataset, averaged by pair

# Inputs:
# data: dataframe
# separate_by_treatment: boolean, include treatment split from "treatment" column in dataframe
# time_values: which observation periods from "time" column in dataframe
# major_type_values: which major USV types from "major_type" column in dataframe
# fill_by: filling bars by "label" or "major_type" columns in dataframe
# dataset_name: title, custom input

# Make plot using data averaged across pairs
create_stacked_bar_plot_averaged <- function(data, separate_by_treatment = FALSE, time_values = NULL, major_type_values = NULL, dataset_name = "Dataset", fill_by = "label") {
  # Validate fill_by input
  if (!fill_by %in% c("label", "major_type")) {
    stop("Invalid fill_by value. Please use 'label' or 'major_type'.")
  }
  
  # Filter data for time_values if provided
  if (!is.null(time_values)) {
    data <- data %>%
      filter(time %in% time_values)
  }
  
  # Filter data for major_type_values if provided
  if (!is.null(major_type_values)) {
    data <- data %>%
      filter(major_type %in% major_type_values)
  }
  
  # Calculate the count of rows for each level of "label" within each "pair_ID"
  counts <- data %>%
    dplyr::group_by(pair_ID, label) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    ungroup()
  
  # Calculate the average count for each level of "label" across all "pair_ID"
  avg_counts <- counts %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(avg_count = mean(count)) %>%
    ungroup()
  
  # Merge the average counts back into the original data
  data <- data %>%
    left_join(avg_counts, by = "label")
  
  # Create the plot based on separate_by_treatment
  if (separate_by_treatment) {
    p <- ggplot(data, aes_string(x = "treatment", y = "avg_count", fill = fill_by)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE) +
      theme_cowplot() +
      theme(
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("Treatment") +
      ylab("Average Relative Proportion") +
      facet_wrap(~time) +
      ggtitle(paste("Data:", dataset_name)) # Add title to the plot
  } else {
    p <- ggplot(data, aes_string(x = "time", y = "avg_count", fill = fill_by)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE) +
      theme_cowplot() +
      theme(
        panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 8)
      ) +
      xlab("Time") +
      ylab("Average Relative Proportion") +
      ggtitle(paste("Data:", dataset_name)) # Add title to the plot
  }
  
  return(p)
}


