generate_zinb_table <- function(data, formula, output_label = "ZINB_table", caption_label = "Zero-inflated negative binomial model") {
  
  # Silly workaround to make sure formula_str updates in the global environment
  formula_str <<- formula
  
  # Parse formula from string
  formula_obj <<- as.formula(formula_str)
  
  # Fit a GLMM
  fit <- glmmTMB(formula = formula_obj,
                 data = data,
                 ziformula = ~1, 
                 family = nbinom2)
  
  # Obtain summary for the model
  glmmTMB_summary <- summary(fit)
  
  # Extract model parameters for all components
  cond_fixef <- fixef(fit)$cond
  cond_se <- sqrt(diag(vcov(fit)$cond))
  cond_z <- glmmTMB_summary$coefficients$cond[, "z value"]
  cond_pval <- glmmTMB_summary$coefficients$cond[, "Pr(>|z|)"]
  
  # Check if zero-inflation and dispersion terms exist
  zi_fixef <- if (!is.null(fixef(fit)$zi)) fixef(fit)$zi else numeric(0)
  zi_se <- if (!is.null(vcov(fit)$zi)) sqrt(diag(vcov(fit)$zi)) else numeric(0)
  zi_z <- if (!is.null(glmmTMB_summary$coefficients$zi)) glmmTMB_summary$coefficients$zi[, "z value"] else numeric(0)
  zi_pval <- if (!is.null(glmmTMB_summary$coefficients$zi)) glmmTMB_summary$coefficients$zi[, "Pr(>|z|)"] else numeric(0)
  
  disp_fixef <- if (!is.null(fixef(fit)$disp)) fixef(fit)$disp else numeric(0)
  disp_se <- if (!is.null(vcov(fit)$disp)) sqrt(diag(vcov(fit)$disp)) else NA
  disp_z <- if (!is.null(glmmTMB_summary$coefficients$disp)) glmmTMB_summary$coefficients$disp[, "z value"] else NA
  disp_pval <- if (!is.null(glmmTMB_summary$coefficients$disp)) glmmTMB_summary$coefficients$disp[, "Pr(>|z|)"] else NA
  
  # Combine all components into a single table
  all_terms <- c(
    rownames(glmmTMB_summary$coefficients$cond),
    if (!is.null(zi_fixef)) "Zero inflation" else NULL,
    if (!is.null(disp_fixef)) "Dispersion factor" else NULL
  )
  
  fixef_table <- c(cond_fixef, zi_fixef, disp_fixef)
  se_table <- c(cond_se, zi_se, disp_se)
  z_table <- c(cond_z, zi_z, disp_z)
  pval_table <- c(cond_pval, zi_pval, disp_pval)
  
  # Prompt user for custom term names
  term_names <- character(length(all_terms))
  for (i in seq_along(all_terms)) {
    cat(sprintf("Current term: %s\n", all_terms[i]))
    term_names[i] <- readline(prompt = "Enter custom label or press enter to keep current name: ")
    if (term_names[i] == "") {
      term_names[i] <- all_terms[i]
    }
  }
  
  # Create a significance notation based on p-value thresholds
  p_value_signif <- ifelse(pval_table < 0.001, "***",
                           ifelse(pval_table < 0.01, "**",
                                  ifelse(pval_table < 0.05, "*",
                                         ifelse(pval_table < 0.06, ".", ""))))
  
  # Create a summary table without effect size information
  glmmTMB_table <- data.frame(
    Term = term_names,
    Coefficients = format(fixef_table, scientific = TRUE, digits = 3),
    SE = sprintf("%.3f", se_table),
    ZValue = format(z_table, digits = 3),
    pValue = ifelse(pval_table < 0.0001, "<0.0001", sprintf("%.3f", pval_table)),
    Significance = p_value_signif
  )
  
  # Set "SE", "Z Value", "P Value", and "Significance" to empty strings where values are NA
  na_indices <- is.na(glmmTMB_table$SE) | is.na(glmmTMB_table$ZValue) | is.na(glmmTMB_table$pValue)
  
  glmmTMB_table$SE[na_indices] <- ""
  glmmTMB_table$ZValue[na_indices] <- ""
  glmmTMB_table$pValue[na_indices] <- ""
  glmmTMB_table$Significance[na_indices] <- ""
  
  
  # Export the table for a scientific publication
  html_file_name <- paste0(output_label, ".html")
  png_file_name <- paste0(output_label, ".png")
  
  # Create kable output and add a horizontal line above the last two rows
  kable_output <- kable(glmmTMB_table, align = c("l", "c", "c", "c", "c", "c"), 
                        caption = caption_label, col.names = c("", "beta", "se", "z-value", "p-value", "")) %>%
    kable_classic(full_width = FALSE) %>%
    column_spec(1, bold = TRUE) %>%
    column_spec(2:3, width = "2cm") %>%
    column_spec(4, width = "2cm") %>%
    column_spec(5, width = "2cm") %>%
    column_spec(6, width = "1cm", bold = TRUE) %>%
    row_spec(0, bold = TRUE) %>%
    add_header_above(c(" " = 5, " " = 1), line = TRUE) %>%  
    row_spec(nrow(glmmTMB_table) - 2, extra_css = "border-bottom: 1px dashed #000")
  
  save_kable(kable_output, file = html_file_name)
  
  # Use webshot2 to print the table to PNG
  webshot2::webshot(
    url = html_file_name,
    file = png_file_name,
    selector = "body"
  )
  
  cat("PNG generated successfully!\n")
  
  return(png_file_name)
}
