library(tidyverse)
library(tidymodels)
library(lubridate)
library(slider)

source("R/clean_data.R")

# Log Regression Model for Predicting precincts with meaningful increased incidents in future month

# Create monthly precinct shooting counts

precinct_monthly_model <-
  nypd_shooting_clean %>%
  filter(
    !is.na(precinct),
    !is.na(boro),
    !is.na(occur_date)
  ) %>%
  count(
    precinct,
    boro,
    year,
    month_number,
    name = "shootings"
  ) %>%
  mutate(
    month_date = make_date(
      year,
      month_number,
      1
    )
  ) %>%
  arrange(
    precinct,
    month_date
  )

# Identify monthly range


earliest_month <-
  min(
    precinct_monthly_model$month_date,
    na.rm = TRUE
  )

latest_month <-
  max(
    precinct_monthly_model$month_date,
    na.rm = TRUE
  )

# Add Precinct-Months with zero shootings since there's gaps in data

precinct_monthly_model <-
  precinct_monthly_model %>%
  select(
    precinct,
    boro,
    month_date,
    shootings
  ) %>%
  complete(
    nesting(
      precinct,
      boro
    ),
    month_date = seq.Date(
      from = earliest_month,
      to = latest_month,
      by = "month"
    ),
    fill = list(
      shootings = 0
    )
  ) %>%
  arrange(
    precinct,
    month_date
  )

# Recreate calendar variables

precinct_monthly_model <-
  precinct_monthly_model %>%
  mutate(
    year = year(month_date),
    
    month_number = factor(
      month(month_date),
      levels = 1:12
    ),
    
    precinct = factor(precinct),
    
    boro = factor(boro)
  )

# Create lagged shooting variables

precinct_monthly_model <-
  precinct_monthly_model %>%
  group_by(precinct) %>%
  arrange(
    month_date,
    .by_group = TRUE
  ) %>%
  mutate(
    previous_month_shootings =
      lag(
        shootings,
        n = 1
      ),
    
    previous_month_change =
      lag(
        shootings,
        n = 1
      ) -
      lag(
        shootings,
        n = 2
      ),
    
    monthly_change =
      shootings -
      previous_month_shootings
  ) %>%
  ungroup()

# Create rolling time frame shooting predictors

precinct_monthly_model <-
  precinct_monthly_model %>%
  group_by(precinct) %>%
  arrange(
    month_date,
    .by_group = TRUE
  ) %>%
  mutate(
    rolling_3_month_average =
      slide_dbl(
        lag(
          shootings,
          n = 1
        ),
        mean,
        .before = 2,
        .complete = TRUE
      ),
    
    rolling_6_month_average =
      slide_dbl(
        lag(
          shootings,
          n = 1
        ),
        mean,
        .before = 5,
        .complete = TRUE
      ),
    
    rolling_3_month_sd =
      slide_dbl(
        lag(
          shootings,
          n = 1
        ),
        sd,
        .before = 2,
        .complete = TRUE
      )
  ) %>%
  ungroup()

# Remove any incomplete rolling indicators

precinct_monthly_model <-
  precinct_monthly_model %>%
  filter(
    !is.na(previous_month_shootings),
    !is.na(previous_month_change),
    !is.na(rolling_3_month_average),
    !is.na(rolling_6_month_average),
    !is.na(rolling_3_month_sd),
    !is.na(monthly_change)
  )

# Split Testing and Training sets

number_of_test_months <- 24

test_start_month <-
  latest_month %m-%
  months(
    number_of_test_months - 1
  )

precinct_training <-
  precinct_monthly_model %>%
  filter(
    month_date < test_start_month
  )

precinct_testing <-
  precinct_monthly_model %>%
  filter(
    month_date >= test_start_month
  )

# Calculate meaningful increase threshold as we did in exploration

meaningful_increase_threshold <-
  quantile(
    precinct_training$monthly_change,
    probs = 0.8,
    na.rm = TRUE
  ) %>%
  unname()

meaningful_increase_threshold

# Create binary outcome: Yes or No on when increase is meaningful

precinct_training <-
  precinct_training %>%
  mutate(
    meaningful_increase = factor(
      if_else(
        monthly_change >
          meaningful_increase_threshold,
        "yes",
        "no"
      ),
      levels = c(
        "no",
        "yes"
      )
    )
  )

precinct_testing <-
  precinct_testing %>%
  mutate(
    meaningful_increase = factor(
      if_else(
        monthly_change >
          meaningful_increase_threshold,
        "yes",
        "no"
      ),
      levels = c(
        "no",
        "yes"
      )
    )
  )

# Check training distribution

training_outcome_distribution <-
  precinct_training %>%
  count(
    meaningful_increase
  ) %>%
  mutate(
    percent =
      n /
      sum(n) *
      100
  )

training_outcome_distribution

# Feature selection for modeling

precinct_training_model <-
  precinct_training %>%
  select(
    precinct,
    boro,
    month_date,
    year,
    month_number,
    previous_month_shootings,
    previous_month_change,
    rolling_3_month_average,
    rolling_6_month_average,
    rolling_3_month_sd,
    meaningful_increase
  )

precinct_testing_model <-
  precinct_testing %>%
  select(
    precinct,
    boro,
    month_date,
    year,
    month_number,
    previous_month_shootings,
    previous_month_change,
    rolling_3_month_average,
    rolling_6_month_average,
    rolling_3_month_sd,
    meaningful_increase
  )

# Create model recipe

hotspot_recipe <-
  recipe(
    meaningful_increase ~ .,
    data = precinct_training_model
  ) %>%
  update_role(
    month_date,
    new_role = "identifier"
  ) %>%
  step_unknown(
    all_nominal_predictors()
  ) %>%
  step_dummy(
    all_nominal_predictors()
  ) %>%
  step_zv(
    all_predictors()
  ) %>%
  step_normalize(
    all_numeric_predictors()
  )

# Initilize log regression model

hotspot_model <-
  logistic_reg(
    mode = "classification"
  ) %>%
  set_engine("glm")

# Create workflow


hotspot_workflow <-
  workflow() %>%
  add_recipe(
    hotspot_recipe
  ) %>%
  add_model(
    hotspot_model
  )

# Fit model to training data

hotspot_fit <-
  hotspot_workflow %>%
  fit(
    data = precinct_training_model
  )

# Prediction using model and generate probabilities

hotspot_classes <-
  predict(
    hotspot_fit,
    new_data = precinct_testing_model,
    type = "class"
  )

hotspot_probabilities <-
  predict(
    hotspot_fit,
    new_data = precinct_testing_model,
    type = "prob"
  )

# Combine test set outcomes with predicted outcomes

hotspot_predictions <-
  precinct_testing_model %>%
  select(
    precinct,
    boro,
    month_date,
    meaningful_increase
  ) %>%
  bind_cols(
    hotspot_classes,
    hotspot_probabilities
  )

hotspot_predictions

# Evaluate through confusion matrix

hotspot_confusion_matrix <-
  hotspot_predictions %>%
  conf_mat(
    truth = meaningful_increase,
    estimate = .pred_class
  )

hotspot_confusion_matrix

# Model Metrics


hotspot_accuracy <-
  hotspot_predictions %>%
  accuracy(
    truth = meaningful_increase,
    estimate = .pred_class
  )

hotspot_precision <-
  hotspot_predictions %>%
  precision(
    truth = meaningful_increase,
    estimate = .pred_class,
    event_level = "second"
  )

hotspot_recall <-
  hotspot_predictions %>%
  recall(
    truth = meaningful_increase,
    estimate = .pred_class,
    event_level = "second"
  )

hotspot_roc_auc <-
  hotspot_predictions %>%
  roc_auc(
    truth = meaningful_increase,
    .pred_yes,
    event_level = "second"
  )

hotspot_f1 <-
  hotspot_predictions %>%
  f_meas(
    truth = meaningful_increase,
    estimate = .pred_class,
    event_level = "second"
  )


hotspot_metrics <-
  bind_rows(
    hotspot_accuracy,
    hotspot_precision,
    hotspot_recall,
    hotspot_f1,
    hotspot_roc_auc
  )

hotspot_metrics

# ROC Curve

hotspot_roc_curve <-
  hotspot_predictions %>%
  roc_curve(
    truth = meaningful_increase,
    .pred_yes,
    event_level = "second"
  )

autoplot(
  hotspot_roc_curve
)
