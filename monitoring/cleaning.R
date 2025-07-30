data_cleaning <- function(df_raw){
  df_clean <- df_raw |>
    # Clean column names to snake_case using janitor
    janitor$clean_names() |>

    # Filter for rows where the event contains 'cholera' but exclude other similar diseases
    dplyr$filter(
      stringr$str_detect(tolower(event), "cholera") &
        !stringr$str_detect(tolower(event), "intestinal|bacterial|shigellosis|salmonellosis")
    ) |>

    # Select and rename key columns
    dplyr$transmute(
      iso3 = location_codes$names_to_iso3(country),  # Convert country names to ISO3 codes
      event,

      # Consolidate multiple possible start date fields
      start_date_raw = dplyr$case_when(
        !is.na(start_of_reporting_period) ~ start_of_reporting_period,
        !is.na(start_of_reporting_period_2) ~ start_of_reporting_period_2,
        !is.na(start_of_reporting_period_3) ~ start_of_reporting_period_3
      ),

      date = as.Date(week_date),  # Parse the week_date column to Date format
      #week_date_start = date - lubridate$wday(date, week_start = 1) + 1,  # Calculate the start of the week

      # Parse start_date using appropriate format based on date patterns or time
      start_date = dplyr$case_when(
        stringr$str_detect(start_date_raw, "[0-9]{1,2}[-|//][A-Za-z]{3}") ~ lubridate$dmy(start_date_raw, quiet = TRUE),
        stringr$str_detect(start_date_raw, "^[A-Za-z]{3}") ~ lubridate$mdy(start_date_raw, quiet = TRUE),
        date >= "2023-09-25" ~ lubridate$mdy(start_date_raw, quiet = TRUE),
        date < "2023-09-25" ~ lubridate$dmy(start_date_raw, quiet = TRUE)
      ),

      cholera_cases = readr$parse_number(as.character(total_cases)),  # Extract numeric case count
      cholera_cfr = readr$parse_number(as.character(cfr)),
      deaths = readr$parse_number(as.character(deaths))

    ) |>

    # Drop the raw start_date column
    dplyr$select(-start_date_raw) |>

    # Group by ISO3 and date for aggregation
    dplyr$group_by(iso3, date) |>

    # Summarize by collapsing multiple event descriptions and summing cases
    dplyr$summarize(
      event = paste(unique(event), collapse = "; "),
      start_date = min(start_date),
      cholera_cases = sum(cholera_cases),
      cholera_cfr = sum(cholera_cfr),
      deaths = sum(deaths),
      .groups = "drop"
    ) |>

    # Re-group by country
    dplyr$group_by(iso3) |>

    # Sort by date within each group
    dplyr$arrange(date, .by_group = TRUE) |>

    # Interpolate missing case values using linear approximation
    dplyr$mutate(
      cholera_cases = zoo$na.approx(cholera_cases, na.rm = FALSE)
    ) |>
    # Fill in missing dates with NA values for cholera cases
    # Cleaning up values shown as decimals

    # Remove grouping
    dplyr$ungroup()

  decimal_values <- df_clean |>
    dplyr$filter(cholera_cases %% 1 != 0)
  small_values <- df_clean |>
    dplyr$filter(cholera_cases < 200)
  df_prepped <- df_clean |>
    dplyr$group_by(iso3) |>
    dplyr$mutate(
      original_value = cholera_cases,
      # if previous value is more than 3 weeks ago, do not change small values
      prev_date_diff = as.numeric(date - dplyr$lag(date, 1, default = NA)),
      # Create the previous and next value columns
      # Get the previous values
      prev_val = pmax(
        dplyr$lag(cholera_cases, 1, default = NA),
        dplyr$lag(cholera_cases, 2, default = NA),
        dplyr$lag(cholera_cases, 3, default = NA),
        dplyr$lag(cholera_cases, 4, default = NA),
        dplyr$lag(cholera_cases, 5, default = NA),
        na.rm = TRUE
      ),
      # Get the next values
      next_val = pmax(
        dplyr$lead(cholera_cases, 1, default = NA),
        dplyr$lead(cholera_cases, 2, default = NA),
        dplyr$lead(cholera_cases, 3, default = NA),
        dplyr$lead(cholera_cases, 4, default = NA),
        dplyr$lead(cholera_cases, 5, default = NA),
        na.rm = TRUE
      ),
      # Calculate the reference value as the larger of the previous and next values
      # Calculate the scaled cholera_cases (multiplying by 10, 100, and 1000)
      scaled_1 = cholera_cases * 1,
      scaled_10 = cholera_cases * 10,
      scaled_100 = cholera_cases * 100,
      scaled_1000 = cholera_cases * 1000,
      scaled_10000 = cholera_cases * 10000,
      # Calculate the absolute differences between scaled values and the reference
      diffs_prev_1 = abs(scaled_1 - prev_val),
      diffs_prev_10 = abs(scaled_10 - prev_val),
      diffs_prev_100 = abs(scaled_100 - prev_val),
      diffs_prev_1000 = abs(scaled_1000 - prev_val),
      diffs_prev_10000 = abs(scaled_10000 - prev_val),
      diffs_next_1 = abs(scaled_1 - next_val),
      diffs_next_10 = abs(scaled_10 - next_val),
      diffs_next_100 = abs(scaled_100 - next_val),
      diffs_next_1000 = abs(scaled_1000 - next_val),
      diffs_next_10000 = abs(scaled_10000 - next_val),
      # Find the minimum difference for each row
      min_diff = pmin(diffs_prev_1, diffs_prev_10, diffs_prev_100, diffs_prev_1000,
                      diffs_prev_10000, diffs_next_1, diffs_next_10, diffs_next_100,
                      diffs_next_1000, diffs_next_10000, na.rm = TRUE),
      # Find the best match by selecting the scaled value with the smallest difference
      best_match = floor(dplyr$case_when(
        min_diff == diffs_prev_1 ~ scaled_1,
        min_diff == diffs_prev_10 ~ scaled_10,
        min_diff == diffs_prev_100 ~ scaled_100,
        min_diff == diffs_prev_1000 ~ scaled_1000,
        min_diff == diffs_prev_10000 ~ scaled_10000,
        min_diff == diffs_next_1 ~ scaled_1,
        min_diff == diffs_next_10 ~ scaled_10,
        min_diff == diffs_next_100 ~ scaled_100,
        min_diff == diffs_next_1000 ~ scaled_1000,
        min_diff == diffs_next_10000 ~ scaled_10000,
        TRUE ~ cholera_cases
      )),
      best_match_class = ceiling(best_match / cholera_cases),
      ref = dplyr$case_when(
        is.na(prev_val) & is.na(next_val) ~ NA_real_,
        is.na(prev_val) ~ next_val,
        is.na(next_val) ~ prev_val,
        abs(prev_val - best_match) <= abs(next_val - best_match) ~ prev_val,
        TRUE ~ next_val
      ),
      # Apply the scaling logic: update cholera_cases based on the best match
      cholera_cases = dplyr$case_when(
        # prev_value == next value == current value, then keep current value
        (dplyr$lag(cholera_cases, 1) == dplyr$lead(cholera_cases, 1) &
           dplyr$lag(cholera_cases, 1) == cholera_cases) ~ cholera_cases,
        # If the value is non-integer and ref exists, check min_diff against best_match_class
        ((cholera_cases %% 1 != 0 & !is.na(ref))) ~ {
          dplyr$if_else((min_diff < 1 | min_diff < best_match_class |
                           best_match_class >= 100), best_match, floor(cholera_cases))
        },
        # If cholera_cases is small (< 200), and strong match to prev/next
        cholera_cases < 200 & !is.na(ref) ~ {
          dplyr$if_else((prev_val == next_val | min_diff < best_match_class) &
                          (!is.na(prev_val) & !is.na(next_val)) &
                          (prev_date_diff <= 28), ref, cholera_cases)
        },
        # If min_diff is smaller than best_match_class, update
        min_diff < best_match_class & !is.na(ref) ~ {
          best_match
        },
        TRUE ~ floor(cholera_cases)
      )
    ) |>
    dplyr$select(iso3, date, event, start_date, cholera_cases, cholera_cfr, deaths, prev_date_diff)

}
