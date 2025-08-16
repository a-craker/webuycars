library(ranger)
library(cowplot)
library(tidyverse)
# library(randomForest)

# ranger is a faster, more modern random forest implementation - it can handle high cardinality in categorical variables (because we have many makes and models)

# NOTES -------------------------------------------------------------------

### RANDOM FOREST ###
# ensemble learning method, combining multiple individual models to produce a better result than any single one

# random forests allow us to measure how important each variable is in predicting the target:
    # when a variable is important. splitting on it leads to large reductions in prediction error
    # we track this across all trees and calculate importance score


### DECISION TREE ###
# splits the data set into branches bases on feature values
  # at each node, it chooses the best split to reduce error in predicting the target (in this case, car price)
  # continues splitting until it reaches a stopping condition (e.g. minimum number of observations)
# problem - trees tend to over-fit (memorize the data rather than generalize it)

### RANDOM FOREST = MANY DECISION TREES + RANDOMNESS
# (1) BOOTSTRAP AGGREGATION (BAGGING)
    # each tree is trained on a random subset (with replacement) of the data
    # introduces variation and reduces over-fitting 

# (2) FEATURE RANDOMNESS
    # at each split, the tree only considers a random subset of features, not all of them
    # this de-correlates the trees

# (3) ENSEMBLE VOTING 
    # for regression: the forest average predictions from all trees
    # for classification: it votes and picks the most common class


# DATA -------------------------------------------------------------------

# data prep
cars <- read_csv("output/webuycars.csv")

cars_rf <- read_csv("output/webuycars.csv") %>% 
  select(
    price, model, mileage, year, fuel_tank_size, fuel_consumption, make, gvm,
    service_history, no_gears, has_any_roadworthy_relevant_issues
  ) %>% 
  drop_na() %>% 
  mutate(
    model = as.factor(model),
    make = as.factor(make),
    service_history = as.factor(service_history),
    has_any_roadworthy_relevant_issues = as.factor(has_any_roadworthy_relevant_issues)
  )


variable_importance <- function(df, threshold) {

  threshold <- threshold
  
  cars_rf <- df %>% 
    mutate(price_above = if_else(price > threshold, 1, 0) %>% as.factor())  
    
  
  # train rf
  rf_model <- ranger(
    formula = price_above ~ . - price,  # Exclude actual price
    data = cars_rf,
    importance = "impurity",  # or "permutation" for better accuracy (slower)
    probability = TRUE,
    num.trees = 500,
    seed = 123
  )
  
  importance <- rf_model$variable.importance %>%
    as.data.frame() %>%
    rownames_to_column("feature") %>%
    rename(importance = ".") %>%  
    arrange(desc(importance))
  
  return(importance)
  
}

thresholds <- c(60000, 125000, 250000, 500000, 750000, 1000000)

importance_list <- lapply(thresholds, function(t) {
  variable_importance(cars_rf, t) %>%
    mutate(threshold = paste0("Above R", format(t, big.mark = ",", scientific = FALSE)))
})

all_importance <- bind_rows(importance_list)


# PLOTTING ----------------------------------------------------------------

feature_labels <- c(
  mileage = "Mileage (km)",
  year = "Year",
  fuel_tank_size = "Fuel Tank Size (L)",
  fuel_consumption = "Fuel Consumption (L/100km)",
  model = "Vehicle Model",
  make = "Vehicle Make",
  gvm = "Gross Vehicle Mass (kg)",
  service_history = "Has Service History",
  no_gears = "Number of Gears",
  has_any_roadworthy_relevant_issues = "Roadworthy Issues"
)


plot_importance <- function(df, threshold_label) {
  df %>%
    filter(threshold == threshold_label) %>%
    mutate(
      feature_label = feature_labels[feature],
      feature_label = ifelse(is.na(feature_label), feature, feature_label)
    ) %>% 
    ggplot(aes(x = reorder(feature_label, importance), y = importance)) +
    geom_col(fill = "#8b0e3a", width = 0.9) +
    coord_flip() +
    labs(
      title = threshold_label,
      x = "Feature",
      y = " Variable Importance"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.title.y = element_text(margin = margin(r = 10))
      )
}

threshold_labels <- unique(all_importance$threshold)

plots <- lapply(threshold_labels, function(label) {
  plot_importance(all_importance, label)
})

combined_plot <- plot_grid(plotlist = plots, ncol = 2)

ggsave("plots/variable_importance_plots.png", combined_plot, width = 16, height = 10)

