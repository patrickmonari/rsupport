# Ensure required packages are loaded
library(lme4)
library(car)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

bootstrap_analysis <- function(data, formula_str, Number_of_boots = 1000, output_file = "bootstrapped_effect_sizes.jpg") {
  
  # Silly workaround to make sure formula_str updates in the global environment
  formula_str <<- formula_str
  
  m1 <<- lmer(formula_str, data = data)
  
  # Extract the fixed effect coefficients.
  FE_df <- fixef(m1) %>% 
    t() %>%
    as.data.frame()
  
  # Extract the random effects variance and residual variance
  RE_df <- VarCorr(m1) %>%
    as.data.frame() %>%
    unite("Level", -c(vcov, sdcor)) %>%
    select(-vcov) %>%
    t() %>%
    as.data.frame()
  
  BS_params <- data.frame(matrix(nrow = Number_of_boots, ncol = ncol(FE_df)))
  colnames(BS_params) <- colnames(FE_df)
  
  BS_var <- data.frame(matrix(nrow = Number_of_boots, ncol = ncol(RE_df)))
  colnames(BS_var) <- RE_df["Level",]
  
  BS_pred <- expand.grid(hr_C = quantile(data$hr_C, probs = seq(0, 1, length.out = 10)), treatment_C = data$treatment_C,
                         iterration = 1:Number_of_boots,
                         pred = NA)
  
  for(i in 1:Number_of_boots) {
    BS_X <- slice_sample(data, prop = 1, replace = TRUE)
    BS_lmer <- lmer(formula = m1@call$formula,
                    data = BS_X)
    
    BS_params[i,] <- BS_lmer %>%
      fixef() %>%
      t() %>%
      as.data.frame()
    
    BS_var[i,] <- BS_lmer %>%
      VarCorr() %>%
      as.data.frame() %>%
      .$sdcor
    
    BS_pred[which(BS_pred$iterration == i),]$pred <- predict(BS_lmer,
                                                             newdata = BS_pred[which(BS_pred$iterration == i),],
                                                             re.form = ~0)
  }
  
  # Prompt the user to specify a binwidth or use the default value of 30
  cat("Enter the bin number for the histograms (or press enter to use default value of 30): ")
  user_bins <- readline()
  if (user_bins == "") {
    bins <- 30
  } else {
    bins <- as.numeric(user_bins)
  }
  
  # Only prompt for non-intercept coefficients
  coef_titles <- sapply(colnames(BS_params)[-which(colnames(BS_params) == "(Intercept)")], function(coef_name) {
    cat(sprintf("Enter the title for the coefficient '%s' (or press enter to use default): ", coef_name))
    title <- readline()
    if (title == "") {
      return(coef_name)
    } else {
      return(title)
    }
  })
  
  # Prompt the user for the CI plot title
  cat("Enter the title for the Confidence Interval (CI) plot (or press enter to use default 'CI Plot'): ")
  ci_title <- readline()
  if (ci_title == "") {
    ci_title <- "CI Plot"
  }
  
  # Generate the CI plot
  ci_plot <- plot_model(m1, type = "std") + ylim(-1, 1) + theme_cowplot() + labs(title = ci_title) + theme(plot.title = element_text(size = 18))
  
  # Ensure the order of the levels matches the order in which they appear in the plot
  ordered_names <- c("(Intercept)", names(coef_titles))
  ordered_coef_titles <- c("(Intercept)", coef_titles)
  
  # Update y-axis labels based on the correct order
  ci_plot$data$term <- factor(ci_plot$data$term, levels = rev(ordered_names), labels = rev(ordered_coef_titles))
  
  # Generate plots for each coefficient, excluding the intercept
  plots_list <- lapply(1:length(coef_titles), function(i) {
    coef_name <- names(coef_titles)[i]
    data_to_plot <- BS_params %>% rename_with(~ "CoefValue", all_of(coef_name))
    title <- coef_titles[i]
    p <- ggplot(data_to_plot, aes(x = CoefValue)) + 
      geom_histogram(bins = bins) + 
      theme_cowplot() +
      labs(title = title, x = "Coefficient Value") +
      theme(plot.title = element_text(size = 18))
    return(p)
  })
  
  # Combine CI plot and the list of coefficient plots
  all_plots <- list(ci_plot)
  all_plots <- append(all_plots, plots_list)
  
  # Start plotting
  jpeg(output_file, units="in", width=10, height=15, res=1000)
  print(cowplot::plot_grid(plotlist = all_plots, labels = "AUTO", nrow = length(all_plots), label_size = 24, label_x = 0, label_y = 1 ))
  dev.off()
  
  cat("Image generated successfully!\n")
}

# Usage
# bootstrap_analysis(data = d, formula_str = "y ~ x*z + (1|id)", output_file = "my_custom_name.jpg")