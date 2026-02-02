# Psycholinguistics Data Analysis - Mixed Effects Models for Reading Time
# This script analyzes experimental data to examine VP repetition effects across conditions
# The code has been optimized for public sharing and reproducibility

# Load required packages
required_packages <- c("readxl", "dplyr", "tidyr", "lme4", "lmerTest", "emmeans", 
                       "ggplot2", "ggeffects", "showtext", "showtextdb", "sysfonts", "car")

# Install missing packages if necessary
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop("Package ", pkg, " failed to install or load!")
    }
  }
}

# ==================== Data Preprocessing Functions ====================

# Load Experiment 2 data from Excel (replace with actual file path)
data <-  Experiment2_Data_Summary  # placeholder, replace with actual import

# Function 1: Clean reading times (remove out-of-bounds RTs)
clean_reading_times <- function(data) {
  long_columns <- c("RT2", "RT3", "RT4", "RT5", "RT6", "RT7", "RT8", "RT9",
                    "RT12", "RT13", "RT14", "RT15", "RT16", "RT17", "RT18", "RT19")
  short_columns <- c("RT2", "RT3", "RT6", "RT7")
  
  data <- data %>%
    mutate(
      across(
        all_of(long_columns),
        ~ if_else(type == "long" & (.x > 3 | .x < 0.09), NA_real_, .x)
      ),
      across(
        all_of(short_columns),
        ~ if_else(type == "short" & (.x > 3 | .x < 0.09), NA_real_, .x)
      )
    )
  
  return(data)
}

# Function 2: Remove rows with missing data
remove_missing_trials <- function(data) {
  long_columns <- c("RT2", "RT3", "RT4", "RT5", "RT6", "RT7", "RT8", "RT9",
                    "RT12", "RT13", "RT14", "RT15", "RT16", "RT17", "RT18", "RT19")
  short_columns <- c("RT2", "RT3", "RT6", "RT7")
  
  data_clean <- data %>%
    filter(
      if_else(
        type == "long",
        rowSums(is.na(across(all_of(long_columns)))) == 0,
        if_else(
          type == "short",
          rowSums(is.na(across(all_of(short_columns)))) == 0,
          TRUE
        )
      )
    )
  
  return(data_clean)
}

# Function 3: Remove low-accuracy subjects
remove_low_accuracy_subjects <- function(data, threshold = 0.85) {
  subject_accuracy <- data %>%
    group_by(subject) %>%
    summarise(
      correct_rate = mean(is_correct, na.rm = TRUE),
      n_trials = n()
    )
  
  subjects_to_keep <- subject_accuracy %>%
    filter(correct_rate >= threshold) %>%
    pull(subject)
  
  data_clean <- data %>%
    filter(subject %in% subjects_to_keep)
  
  cat("Original number of subjects:", length(unique(data$subject)), "\n")
  cat("Number of subjects after filtering:", length(unique(data_clean$subject)), "\n")
  cat("Number of subjects removed:", length(unique(data$subject)) - length(unique(data_clean$subject)), "\n")
  
  return(data_clean)
}

# Function 4: Residualize data - remove length effects
residualize_data <- function(data) {
  data <- data %>%
    rename(
      VP1_RT = `VP1_RT(ms)`,
      VP2_RT = `VP2_RT(ms)`
    )
  
  df_long <- data %>%
    pivot_longer(
      cols = c(VP1_RT, VP2_RT),
      names_to = "Condition",
      values_to = "RT"
    ) %>%
    mutate(
      Condition = factor(Condition, levels = c("VP1_RT", "VP2_RT")),
      subject = factor(subject)
    )
  
  cat("Number of rows after pivot_longer:", nrow(df_long), "\n")
  cat("Missing values in RT:", sum(is.na(df_long$RT)), "\n")
  cat("Missing values in VP_LEN:", sum(is.na(df_long$VP_LEN)), "\n")
  
  df_long_complete <- df_long %>%
    filter(!is.na(RT), !is.na(VP_LEN))
  
  cat("Rows available for modeling after removing missing:", nrow(df_long_complete), "\n")
  
  model <- lmer(
    RT ~ VP_LEN + (1 | subject),
    data = df_long_complete
  )
  
  df_long_complete$RCRT <- resid(model)
  df_long_complete <- df_long_complete %>%
    mutate(RCRT_z = as.numeric(scale(RCRT)))
  
  return(df_long_complete)
}

# Function 5: Remove outliers based on residuals
remove_outliers <- function(data) {
  resid_sd <- sd(abs(data$RCRT))
  threshold <- 3 * resid_sd
  data <- data %>%
    mutate(is_extreme = ifelse(abs(RCRT) > threshold, 1, 0))
  
  cat("Number of extreme trials:", sum(data$is_extreme), "\n")
  
  data_clean <- data[data$is_extreme == 0, ]
  cat("Original observations:", nrow(data), "\n")
  cat("Observations after removing extremes:", nrow(data_clean), "\n")
  
  return(data_clean)
}

# Function 6: Fit final mixed-effects model
fit_final_model <- function(data) {
  data <- data %>%
    mutate(
      z_lextale = as.numeric(scale(lextale)),
      z_wm = as.numeric(scale(working_memory))
    )
  
  data$type <- as.factor(data$type)
  data$type <- relevel(data$type, ref = "long")
  
  model_new_1 <- lmer(
    RCRT_z ~ Condition * type * z_wm + Condition * type * z_lextale
    + (1 | subject) + (1 | trial_id), 
    data = data
  )
  
  return(model_new_1)
}

# Function 7: Generate visualizations
create_visualizations <- function(data, model) {
  # Interaction plot: Condition × VP length
  emm_condition_type <- emmeans(model, specs = ~ Condition | type)
  emm_data <- as.data.frame(emm_condition_type)
  
  p1 <- ggplot(emm_data, aes(x = Condition, y = emmean, group = type, color = type)) +
    geom_line(linewidth = 1.2, alpha = 0.8) +
    geom_point(size = 4, alpha = 0.9) +
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                  width = 0.15, linewidth = 0.8, alpha = 0.7) +
    scale_color_manual(values = c("long" = "#3574b2", "short" = "#d04d4d"),
                       labels = c("Long", "Short"), name = "VP Length") +
    labs(
      x = "Condition (VP Region)",
      y = "Predicted Standardized Reading Time (RCRT_z)",
      title = "Interaction between Condition and VP Length",
      subtitle = "Long VP phrases show greater facilitation effect in VP2 region"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      text = element_text(family = "Arial", size = 16),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40"),
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14, color = "black"),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  # Additional plots (p2, p3, p4) follow the same English style; omitted here for brevity
  
  return(list(p1 = p1))  # return list of plots; extend with p2, p3, p4 as needed
}

# ==================== Main Analysis Pipeline ====================
analyze_data <- function(input_file_path = NULL, data = NULL) {
  if (!is.null(input_file_path)) {
    data <- read_excel(input_file_path)
  } else if (is.null(data)) {
    stop("Must provide either a data file path or a data frame")
  }
  
  cat("Starting data analysis pipeline...\n")
  
  data_cleaned <- clean_reading_times(data)
  data_cleaned <- remove_missing_trials(data_cleaned)
  data_filtered_subjects <- remove_low_accuracy_subjects(data_cleaned)
  data_residualized <- residualize_data(data_filtered_subjects)
  data_no_outliers <- remove_outliers(data_residualized)
  
  cat("Fitting final mixed-effects model...\n")
  final_model <- fit_final_model(data_no_outliers)
  summary(final_model)
  
  cat("\nModel summary:\n")
  print(summary(final_model))
  cat("\nANOVA table:\n")
  print(Anova(final_model, type = "III"))
  
  cat("Generating visualizations...\n")
  visualizations <- create_visualizations(data_no_outliers, final_model)
  
  cat("Analysis complete. Plots generated.\n")
  return(list(model = final_model, data = data_no_outliers, visualizations = visualizations))
}

# Install ragg for high-resolution output
install.packages("ragg")
library(ragg)
