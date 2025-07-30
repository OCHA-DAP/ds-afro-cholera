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
  janitor,
  stringr,
  zoo,
  gghdx,
  whisker,
  emayili,
  countrycode
)
box::use(src/location_codes)

# -- Load and Clean Raw Data --
df_raw_link <- "https://raw.githubusercontent.com/CBPFGMS/pfbi-data/refs/heads/main/final_data_for_powerbi_with_kpi.csv"
df_raw <- readr$read_csv(df_raw_link)

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
        (current_val == max_val) && (current_val >= min_cases) && (cholera_cfr > min_cfr)
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
latest_alerts <- readr$read_csv("monitoring/last_alerts.csv")

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
iso3_watch_alerts <- c()
watch_alerts_raised <- FALSE
purrr$walk2(merged$iso3, seq_len(nrow(merged)), function(country, i) {
  row <- merged[i, ]
  if (!is.na(row$last_watch_alert_new) &&
      (is.na(row$last_watch_alert_old) || row$last_watch_alert_new > row$last_watch_alert_old)) {
    message("🟠 New WATCH alert for ", country, ": ", row$last_watch_alert_new)
    iso3_watch_alerts <<- c(iso3_watch_alerts, country)
    df_country <- df_3_all[df_3_all$iso3 == country, ]

    p <- ggplot2$ggplot(df_country, ggplot2$aes(x = date, y = cholera_cases)) +
      ggplot2$geom_line(color = "grey40", size = 0.3) +
      ggplot2$geom_point(data = subset(df_country, alert == TRUE),
                          ggplot2$aes(x = date, y = cholera_cases),
                          color = "#FFDB58", size = 1) +
      ggplot2$geom_point(data = subset(df_country, alert == TRUE & date == row$last_watch_alert_new),
                         ggplot2$aes(x = date, y = cholera_cases),
                         color = "darkorange", size = 1) +
      ggplot2$labs(
        title = paste("Cholera Cases for", countrycode$countrycode(country, origin = "iso3c", destination = "country.name")),
        x = "Date", y = "Number of Cases",
        caption ="● Dark Orange Dot: New Watch alert\n● Mustard Dot: Previous Watch Alert"
      ) +
      ggplot2$scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
      gghdx$gghdx() +
      ggplot2$theme(
        text = ggplot2$element_text(size = 16.5),
        plot.title = ggplot2$element_text(size = 22, face = "bold"),
        axis.title = ggplot2$element_text(size = 18),
        axis.text = ggplot2$element_text(size = 15),
        plot.caption = ggplot2$element_text(size = 12, hjust = 1, face = "italic", margin = ggplot2$margin(t = 0.7))
      )

    # Save plot
    plot_path <- file.path("plots", paste0("watch_alert_", country, ".png"))
    ggplot2$ggsave(plot_path, plot = p, width = 5, height = 2.5, dpi = 300)
  }
})
if(length(iso3_watch_alerts) > 0){
  watch_alerts_raised <- TRUE
  message("Countries with WATCH alerts: ", paste(iso3_watch_alerts, collapse = ", "))
}

# -- Print Warning Alerts --
iso3_warning_alerts <- c()
warning_alerts_raised <- FALSE

purrr$walk2(merged$iso3, seq_len(nrow(merged)), function(country, i) {
  row <- merged[i, ]
  if (!is.na(row$last_warning_alert_new) &&
      (is.na(row$last_warning_alert_old) || row$last_warning_alert_new > row$last_warning_alert_old)) {

    message("🔴 New WARNING alert for ", country, ": ", row$last_warning_alert_new)
    iso3_warning_alerts <<- c(iso3_warning_alerts, country)

    df_country <- df_1[df_1$iso3 == country, ]

    p <- ggplot2$ggplot(df_country, ggplot2$aes(x = date, y = cholera_cases)) +
      ggplot2$geom_line(color = "grey40", size = 0.3) +
      ggplot2$geom_point(data = subset(df_country, alert_level == "p99"),
                          ggplot2$aes(x = date, y = cholera_cases),
                          color = "#FF6961", size = 1) +
      ggplot2$geom_point(data = subset(df_country, alert_level == "p99" & date == row$last_warning_alert_new),
                          ggplot2$aes(x = date, y = cholera_cases),
                          color = "darkred", size = 1) +
      ggplot2$labs(
        title = paste("Cholera Cases for", countrycode$countrycode(country, origin = "iso3c", destination = "country.name")),
        x = "Date", y = "Number of Cases",
        caption ="● Dark Red Dot: New Warning Alert\n● Red Dot: Previous Warning Alert"
      ) +
      ggplot2$scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") +
      gghdx$gghdx() +
      ggplot2$theme(
        text = ggplot2$element_text(size = 16.5),
        plot.title = ggplot2$element_text(size = 22, face = "bold"),
        axis.title = ggplot2$element_text(size = 18),
        axis.text = ggplot2$element_text(size = 15),
        plot.caption = ggplot2$element_text(size = 12, hjust = 1, face = "italic", margin = ggplot2$margin(t = 0.7))
      )

    # Save plot
    plot_path <- file.path("plots", paste0("warning_alert_", country, ".png"))
    ggplot2$ggsave(plot_path, plot = p, width = 5, height = 2.5, dpi = 300)
  }
})

if (length(iso3_warning_alerts) > 0) {
  warning_alerts_raised <- TRUE
  message("Countries with WARNING alerts: ", paste(iso3_warning_alerts, collapse = ", "))
}


# -- Save Updated Alerts Log --
#readr$write_csv(new_alerts, file = "monitoring/last_alerts.csv")

# -- Email Notification --
if (watch_alerts_raised || warning_alerts_raised) {
  message("Sending email notification for alerts...")

  all_alerts <- c(
    paste0("watch_alert_", iso3_watch_alerts),
    paste0("warning_alert_", iso3_warning_alerts)
  )
  plot_cids <- paste0("cid_plot_", seq_along(all_alerts))
  plot_paths <- file.path("plots", paste0(all_alerts, ".png"))
  plot_countries <- gsub("^(watch_alert_|warning_alert_)", "", all_alerts)
  alert_types <- ifelse(grepl("^watch_alert_", all_alerts), "watch", "warning")

  # Watch section
  watch_blocks <- purrr::map2_chr(
    countrycode$countrycode(plot_countries[alert_types == "watch"], origin = "iso3c", destination = "country.name"),
    plot_cids[alert_types == "watch"],
    ~ glue::glue("<h3>{.x}</h3>\n<img src=\"cid:{.y}\" width=\"100%\"/>")
  )

  # Warning section
  warning_blocks <- purrr::map2_chr(
    countrycode$countrycode(plot_countries[alert_types == "warning"], origin = "iso3c", destination = "country.name"),
    plot_cids[alert_types == "warning"],
    ~ glue::glue("<h3>{.x}</h3>\n<img src=\"cid:{.y}\" width=\"100%\"/>")
  )

  # Combine only non-empty sections
  section_parts <- c()
  if (length(watch_blocks) > 0) {
    section_parts <- c(section_parts, "<h3>Watch Alerts</h3>", "<p>Watch Alerts signal the highest weekly increase in the past 12 months, with at least 500 reported cases.</p>", paste(watch_blocks, collapse = "\n<hr>\n"))
  }
  if (length(warning_blocks) > 0) {
    section_parts <- c(section_parts, "<h3>Warning Alerts</h3>", "<p>Warning Alerts indicate an exceptional increase in cases, surpassing the historical 99th percentile and exceeding 500 cases.</p>", paste(warning_blocks, collapse = "\n<hr>\n"))
  }

  plot_section_html <- paste(section_parts, collapse = "\n<hr>\n")

  # --- Data to render ---
  watch_alerts_html <- if (length(iso3_watch_alerts) > 0) {
    sprintf("<li>Watch Alerts for: %s</li>", paste(countrycode$countrycode(iso3_watch_alerts, origin = "iso3c", destination = "country.name"), collapse = ", "))
  } else {
    ""
  }

  warning_alerts_html <- if (length(iso3_warning_alerts) > 0) {
    sprintf("<li>Warning Alerts for: %s</li>", paste(countrycode$countrycode(iso3_warning_alerts, origin = "iso3c", destination = "country.name"), collapse = ", "))
  } else {
    ""
  }
  template_vars <- list(
    watch_alerts_raised = watch_alerts_raised,
    warning_alerts_raised = warning_alerts_raised,
    watch_alerts_html = watch_alerts_html,
    warning_alerts_html = warning_alerts_html,
    iso3_watch_alerts = paste(iso3_watch_alerts, collapse = ", "),
    iso3_warning_alerts = paste(iso3_warning_alerts, collapse = ", "),
    plot_section = plot_section_html,
    ocha_logo_cid = "ocha_logo_cid"
  )

  # --- Render HTML template ---
  info_path <- "monitoring/templates/informational.html"
  info_template <- paste(readLines(info_path), collapse = "\n")
  info_rendered <- whisker$whisker.render(info_template, template_vars)
  base_path <- "monitoring/templates/base.html"
  base_template <- paste(readLines(base_path), collapse = "\n")
  final_html <- whisker$whisker.render(base_template, list(content = info_rendered))

  # --- Compose email ---
  email <- emayili$envelope() |>
    emayili$from(Sys.getenv("DSCI_AWS_EMAIL_ADDRESS")) |>
    emayili$to("pauline.ndirangu@un.org") |>
    emayili$subject("🚨 New Cholera Alert") |>
    emayili$html(final_html)
  for (i in seq_along(plot_paths)) {
    email <- email |>
      emayili$attachment(path = plot_paths[i], cid = plot_cids[i])
  }
  email <- email |>
    emayili$attachment(path = "monitoring/static/ocha_logo_wide.png", cid = "ocha_logo_cid")

  # --- Send email ---
  smtp <- emayili$server(
    host = Sys.getenv("DSCI_AWS_EMAIL_HOST"),
    port = Sys.getenv("DSCI_AWS_EMAIL_PORT"),
    username = Sys.getenv("DSCI_AWS_EMAIL_USERNAME"),
    password = Sys.getenv("DSCI_AWS_EMAIL_PASSWORD")
  )
  smtp(email)
} else {
  message("No new alerts to notify.")
}

