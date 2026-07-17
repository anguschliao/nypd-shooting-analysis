library (tidyverse)

# Load raw shooting incident data ------------------------------------

source("R/download_data.R")

# Inspect raw data ---------------------------------------------------

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

# Standardize column names -------------------------------------------

nypd_shooting_clean <- nypd_shooting_raw %>%
  rename_with(str_to_lower)

names(nypd_shooting_clean)

# Select relevant variables -----------------------------------------

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

# Convert date and time variables -----------------------------------

nypd_shooting_clean <- nypd_shooting_clean %>%
  mutate(
    occur_date = mdy(occur_date),
    occur_time = hms(occur_time)
  )

class(nypd_shooting_clean$occur_date)

class(nypd_shooting_clean$occur_time)

# Standardize categorical variables ---------------------------------

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

nypd_shooting_clean <- nypd_shooting_clean %>%
  mutate(
    boro = case_when(
      boro == "bronx" ~ "Bronx",
      boro == "brooklyn" ~ "Brooklyn",
      boro == "manhattan" ~ "Manhattan",
      boro == "queens" ~ "Queens",
      boro == "staten island" ~ "Staten Island",
      TRUE ~ str_to_title(boro)
    )
  )

# Convert murder indicator ------------------------------------------

nypd_shooting_clean <- nypd_shooting_clean %>%
  mutate(
    statistical_murder_flag =
      as.logical(statistical_murder_flag)
  )

