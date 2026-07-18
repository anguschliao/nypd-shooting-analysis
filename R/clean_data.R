library (tidyverse)

# Load raw shooting incident data ------------------------------------

source("R/download_data.R")

# Inspect raw data ------------------------------------------

names(nypd_shooting_raw)

dim(nypd_shooting_raw)

head(nypd_shooting_raw)

# Check for missing values

nypd_shooting_raw %>%
  summarize(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  ) %>%
  arrange(desc(missing_values))

# Standardize column names ------------------------------------

nypd_shooting_clean <- nypd_shooting_raw %>%
  rename_with(str_to_lower)

names(nypd_shooting_clean)

# Select relevant variables ------------------------------------

nypd_shooting_clean <- nypd_shooting_clean %>%
  select(
    incident_key,
    occur_date,
    occur_time,
    boro,
    precinct,
    jurisdiction_code,
    loc_of_occur_desc,
    loc_classfctn_desc,
    location_desc,
    statistical_murder_flag,
    perp_age_group,
    perp_sex,
    perp_race,
    vic_age_group,
    vic_sex,
    vic_race,
    x_coord_cd,
    y_coord_cd,
    latitude,
    longitude
  )

# Convert date and time variables ------------------------

nypd_shooting_clean <- nypd_shooting_clean %>%
  mutate(
    occur_date = mdy(occur_date),
    occur_time = hms(occur_time)
  )

class(nypd_shooting_clean$occur_date)

class(nypd_shooting_clean$occur_time)

# Standardize categorical variables ------------------------

nypd_shooting_clean <- nypd_shooting_clean %>%
  mutate(
    across(
      c(
        boro,
        loc_of_occur_desc,
        loc_classfctn_desc,
        location_desc,
        perp_age_group,
        perp_sex,
        perp_race,
        vic_age_group,
        vic_sex,
        vic_race
      ),
      str_to_lower
    )
  )

# Convert murder indicator ------------------------------------------

nypd_shooting_clean <- nypd_shooting_clean %>%
  mutate(
    statistical_murder_flag =
      as.logical(statistical_murder_flag)
  )

# Check incident identifiers for duplications ---------------------------

nypd_shooting_clean %>%
  count(incident_key) %>%
  filter(n > 1) %>%
  arrange(desc(n))

nypd_shooting_clean %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

# Check for fully duplicated rows

sum(duplicated(nypd_shooting_clean))

# Check missing numeric variables ------------------------------

nypd_shooting_clean %>%
  summarize(
    missing_date = sum(is.na(occur_date)),
    missing_time = sum(is.na(occur_time)),
    missing_borough = sum(is.na(boro)),
    missing_precinct = sum(is.na(precinct)),
    missing_latitude = sum(is.na(latitude)),
    missing_longitude = sum(is.na(longitude))
  )

# Check latitude and longitude ranges

nypd_shooting_clean %>%
  summarize(
    minimum_latitude = min(latitude, na.rm = TRUE),
    maximum_latitude = max(latitude, na.rm = TRUE),
    minimum_longitude = min(longitude, na.rm = TRUE),
    maximum_longitude = max(longitude, na.rm = TRUE)
  )

# Create calendar variables -----------------------------------------

nypd_shooting_clean <- nypd_shooting_clean %>%
  mutate(
    year = year(occur_date),
    month = month(
      occur_date,
      label = TRUE,
      abbr = FALSE
    ),
    month_number = month(occur_date),
    week = isoweek(occur_date),
    day_of_week = wday(
      occur_date,
      label = TRUE,
      abbr = FALSE,
      week_start = 1
    ),
    hour = hour(occur_time),
    weekend = day_of_week %in% c(
      "Saturday",
      "Sunday"
    )
  )

# Arrange cleaned data -----------------------------------------------

nypd_shooting_clean <- nypd_shooting_clean %>%
  arrange(
    occur_date,
    occur_time,
    incident_key
  )

# Validate cleaned data ----------------------------------------------

glimpse(nypd_shooting_clean)

dim(nypd_shooting_clean)

summary(nypd_shooting_clean$occur_date)

nypd_shooting_clean %>%
  count(boro, sort = TRUE)

nypd_shooting_clean %>%
  count(year, sort = FALSE)

nypd_shooting_clean %>%
  summarize(
    total_records = n(),
    unique_incident_keys = n_distinct(incident_key),
    earliest_date = min(occur_date, na.rm = TRUE),
    latest_date = max(occur_date, na.rm = TRUE),
    missing_precinct = sum(is.na(precinct)),
    missing_coordinates = sum(
      is.na(latitude) |
        is.na(longitude)
    ),
    fatal_shootings = sum(
      statistical_murder_flag,
      na.rm = TRUE
    )
  )

# Check precinct values

nypd_shooting_clean %>%
  count(precinct, sort = TRUE)
