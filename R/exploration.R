library(tidyverse)

# Emerging Hotspot Detection -------------------------------------

# Create historical shooting averages by precinct and year for prior 3 years

historical_period <-
  nypd_shooting_clean %>%
  filter(
    year >= 2019,
    year <= 2021,
    !is.na(precinct)
  ) %>%
  count(
    precinct,
    year,
    name = "shootings"
  ) %>%
  group_by(precinct) %>%
  summarize(
    historical_avg = mean(shootings),
    .groups = "drop"
  )

# Create shooting averages by precinct and year for recent 3 years

recent_period <-
  nypd_shooting_clean %>%
  filter(
    year >= 2022,
    year <= 2024,
    !is.na(precinct)
  ) %>%
  count(
    precinct,
    year,
    name = "shootings"
  ) %>%
  group_by(precinct) %>%
  summarize(
    recent_avg = mean(shootings),
    .groups = "drop"
  )

# Compare recent activity with the historical baseline

hotspots <-
  historical_period %>%
  inner_join(
    recent_period,
    by = "precinct"
  ) %>%
  filter(
    historical_avg >= 10
  ) %>%
  mutate(
    shooting_change = recent_avg - historical_avg,
    percent_change =
      100 *
      shooting_change /
      historical_avg
  ) %>%
  arrange(desc(percent_change))

# Select the 15 precincts with the largest percentage increases

top_hotspots <-
  hotspots %>%
  slice_max(
    order_by = percent_change,
    n = 15,
    with_ties = FALSE
  )

# Order precincts so the largest increase appears at the top

top_hotspots <-
  top_hotspots %>%
  mutate(
    precinct = factor(
      precinct,
      levels = precinct[
        order(percent_change)
      ]
    )
  )

# Plot emerging shooting hotspots

ggplot(
  top_hotspots,
  aes(
    x = precinct,
    y = percent_change
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = paste0(
        round(percent_change),
        "%"
      )
    ),
    hjust = -0.15
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0,
        0.15
      )
    )
  ) +
  labs(
    title = "Top 15 emerging shooting hotspots",
    subtitle = paste(
      "Change in average annual shootings:",
      "2022–2024 compared with 2019–2021"
    ),
    x = "Precinct",
    y = "Increase in average annual shootings (%)",
    caption = paste(
      "Precincts with fewer than 10 average annual",
      "shootings during 2019–2021 were excluded."
    )
  )

# Precinct risk classification -------------------------------------

# Create complete precinct-month shooting counts for 2024

precinct_monthly_risk <-
  nypd_shooting_clean %>%
  filter(
    year == 2024,
    !is.na(precinct)
  ) %>%
  count(
    precinct,
    month_number,
    name = "shootings"
  ) %>%
  complete(
    precinct,
    month_number = 1:12,
    fill = list(
      shootings = 0
    )
  )

# Calculate 2024 shooting-risk features

precinct_risk <-
  precinct_monthly_risk %>%
  group_by(precinct) %>%
  summarize(
    avg_monthly_shootings = mean(shootings),
    total_shootings = sum(shootings),
    max_monthly_shootings = max(shootings),
    monthly_variability = sd(shootings),
    .groups = "drop"
  )

# Calculate quartile thresholds

risk_thresholds <-
  quantile(
    precinct_risk$avg_monthly_shootings,
    probs = c(
      0.25,
      0.50,
      0.75
    ),
    na.rm = TRUE
  )

# Assign risk classifications

precinct_risk <-
  precinct_risk %>%
  mutate(
    risk_level = case_when(
      avg_monthly_shootings >= risk_thresholds[3] ~
        "Very High",
      avg_monthly_shootings >= risk_thresholds[2] ~
        "High",
      avg_monthly_shootings >= risk_thresholds[1] ~
        "Moderate",
      TRUE ~
        "Low"
    ),
    risk_level = factor(
      risk_level,
      levels = c(
        "Low",
        "Moderate",
        "High",
        "Very High"
      )
    )
  ) %>%
  arrange(desc(avg_monthly_shootings))

# Validate risk classifications

precinct_risk %>%
  count(
    risk_level
  )

precinct_risk %>%
  filter(
    risk_level == "Very High"
  )

# Select highest-risk precincts for plotting

top_risk_precincts <-
  precinct_risk %>%
  slice_max(
    order_by = avg_monthly_shootings,
    n = 25,
    with_ties = FALSE
  ) %>%
  mutate(
    precinct = factor(
      precinct,
      levels = precinct[
        order(avg_monthly_shootings)
      ]
    )
  )

# Plot 2024 precinct risk classifications

ggplot(
  top_risk_precincts,
  aes(
    x = precinct,
    y = avg_monthly_shootings,
    fill = risk_level
  )
) +
  geom_col() +
  geom_text(
    aes(
      label = round(
        avg_monthly_shootings,
        1
      )
    ),
    hjust = -0.15
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +
  labs(
    title = "Precinct shooting-risk classification for 2024",
    subtitle = paste(
      "Risk levels based on average monthly",
      "shooting activity"
    ),
    x = "Precinct",
    y = "Average monthly shootings",
    fill = "Risk level",
    caption = paste(
      "Risk levels are based on quartiles of",
      "2024 average monthly shooting activity."
    )
  )

# Periods of increase in monthly shooting activity -----------------------


# Month to month changes

precinct_monthly <-
  nypd_shooting_clean %>%
  count(
    precinct,
    year,
    month_number,
    name = "shootings"
  ) %>%
  complete(
    precinct,
    year = min(year):max(year),
    month_number = 1:12,
    fill = list(
      shootings = 0
    )
  ) %>%
  mutate(
    month = make_date(
      year,
      month_number,
      1
    )
  ) %>%
  arrange(
    precinct,
    month
  )

precinct_monthly <-
  precinct_monthly %>%
  group_by(precinct) %>%
  mutate(
    monthly_change =
      shootings -
      lag(shootings)
  ) %>%
  ungroup()

# Distribution of changes

summary(
  precinct_monthly$monthly_change
)

quantile(
  precinct_monthly$monthly_change,
  probs = c(
    .50,
    .75,
    .90,
    .95,
    .99
  ),
  na.rm = TRUE
)

# Plot 

ggplot(
  precinct_monthly,
  aes(
    x = monthly_change
  )
) +
  geom_histogram(
    binwidth = 1
  ) +
  labs(
    title =
      "Distribution of month-to-month changes in shootings",
    x = "Change from previous month",
    y = "Number of precinct-months"
  )

# Define meaningful increase threshold

meaningful_increase_threshold <-
  as.numeric(
    quantile(
      precinct_monthly$monthly_change,
      probs = .95,
      na.rm = TRUE
    )
  )

meaningful_increase_threshold

# Classify precinct-month

precinct_monthly <-
  precinct_monthly %>%
  mutate(
    meaningful_increase =
      monthly_change >= meaningful_increase_threshold
  )

# Summarize meaningful-increase classification

meaningful_increase_summary <-
  precinct_monthly %>%
  filter(
    !is.na(meaningful_increase)
  ) %>%
  count(
    meaningful_increase,
    name = "precinct_months"
  ) %>%
  mutate(
    percent =
      precinct_months /
      sum(precinct_months) *
      100,
    classification =
      if_else(
        meaningful_increase,
        "Meaningful increase",
        "No meaningful increase"
      )
  )

meaningful_increase_summary

# Plot meaningful vs non-meaningful increases


ggplot(
  meaningful_increase_summary,
  aes(
    x = classification,
    y = precinct_months,
    fill = classification
  )
) +
  geom_col() +
  geom_text(
    aes(
      label =
        paste0(
          round(percent, 1),
          "%"
        )
    ),
    vjust = -0.4
  ) +
  guides(
    fill = "none"
  ) +
  labs(
    title =
      "Classification of precinct-month changes",
    subtitle =
      paste0(
        "Meaningful increase defined as an increase of ",
        meaningful_increase_threshold,
        " or more shootings"
      ),
    x = NULL,
    y =
      "Number of precinct-months"
  )
