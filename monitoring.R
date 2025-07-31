# Script to send out monitoring emails

# -- Setup --
options(scipen = 999)
min_cfr <- 0
min_cases <- 500

# -- Load Libraries --
box::use(
  readr,
  dplyr,
  purrr,
  lubridate,
  ggplot2,
  ggtext,
  janitor,
  stringr,
  zoo,
  gghdx,
  whisker,
  emayili,
  countrycode,
  AzureStor,
  glue,
  scales
)
box::use(src/location_codes)

# -- Load and Clean Raw Data --
df_raw_link <- "https://raw.githubusercontent.com/CBPFGMS/pfbi-data/refs/heads/main/final_data_for_powerbi_with_kpi.csv"
df_raw <- readr$read_csv(df_raw_link, show_col_types = FALSE)

source("monitoring/cleaning.R")
df_prepped <- data_cleaning(df_raw)

# -- Method 1: Warning Alerts (p99) --
df_1_all <- df_prepped |>
  dplyr$group_by(iso3) |>
  dplyr$mutate(
    weekly_increase = dplyr$if_else(prev_date_diff <= 28,
                                    cholera_cases - dplyr$lag(cholera_cases, 1),
                                    NA_real_),
    p95 = quantile(weekly_increase[weekly_increase > 0], probs = 0.95, na.rm = TRUE),
    p975 = quantile(weekly_increase[weekly_increase > 0], probs = 0.975, na.rm = TRUE),
    p99 = quantile(weekly_increase[weekly_increase > 0], probs = 0.99, na.rm = TRUE),
    year = lubridate$year(date),
    alert_level = dplyr$case_when(
      weekly_increase >= min_cases & weekly_increase >= p99 & cholera_cfr >= min_cfr ~ "p99",
      weekly_increase >= min_cases & weekly_increase >= p975 ~ "p975",
      weekly_increase >= min_cases & weekly_increase >= p95 ~ "p95",
      TRUE ~ "none"
    )
  )
df_1 <- df_1_all |>
  dplyr$ungroup() |>
  dplyr$mutate(fold_increase = weekly_increase / dplyr$lag(weekly_increase, 1, default = NA))

# -- Method 3: Watch Alerts (highest value in 12 months) --
df_3_all <- df_prepped |>
  dplyr$group_by(iso3) |>
  dplyr$mutate(
    weekly_increase = dplyr$if_else(prev_date_diff <= 28, cholera_cases - dplyr$lag(cholera_cases, 1), 0),
    alert_initial = purrr$map_lgl(date, ~{
      window_start <- .x - months(12)
      window_vals <- weekly_increase[date > window_start & date <= .x]

      if (all(is.na(window_vals))) {
        FALSE
      } else {
        current_val <- weekly_increase[date == .x]
        cholera_cfr <- cholera_cfr[date == .x]
        max_val <- max(window_vals, na.rm = TRUE)
        (current_val == max_val) && (current_val >= min_cases) && (cholera_cfr >= min_cfr)
      }
    }),
    alert = purrr$map2_lgl(date, seq_along(date), ~{
      if (!alert_initial[.y] || weekly_increase[.y] < min_cases) return(FALSE)

      last_alert_date <- max(date[alert_initial & date < .x], na.rm = TRUE)

      if (!is.finite(last_alert_date)) {
        TRUE
      } else {
        difftime(.x, last_alert_date, units = "days") > 90
      }
    })
  )
df_3 <- df_3_all |>
  dplyr$filter(alert)

# -- Load Latest Alerts Log --
latest_alerts <- readr$read_csv("monitoring/last_alerts.csv", show_col_types = FALSE)

# -- Compute New Alerts --
last_watch <- df_3 |>
  dplyr$filter(alert == TRUE) |>
  dplyr$group_by(iso3) |>
  dplyr$summarise(last_watch_alert = max(date, na.rm = TRUE), .groups = "drop")

last_warning <- df_1 |>
  dplyr$filter(alert_level == "p99") |>
  dplyr$group_by(iso3) |>
  dplyr$summarise(last_warning_alert = max(date, na.rm = TRUE), .groups = "drop")

new_alerts <- dplyr$full_join(last_watch, last_warning, by = "iso3")
merged <- dplyr$left_join(new_alerts, latest_alerts, by = "iso3", suffix = c("_new", "_old"))

# -- Print Watch Alerts --
source("monitoring/alert_check.R")

# -- Save Updated Alerts Log --
readr$write_csv(new_alerts, file = "monitoring/last_alerts.csv")

# -- Email Notification --
if (watch_alerts_raised || warning_alerts_raised) {
  message("Sending email notification for alerts...")
  source("monitoring/email.R")
  send_email()

} else {
  message("No new alerts.")
}

