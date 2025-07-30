send_email <- function(){
  all_alerts <- c(
    if (length(iso3_watch_alerts) > 0) paste0("watch_alert_", iso3_watch_alerts) else character(0),
    if (length(iso3_warning_alerts) > 0) paste0("warning_alert_", iso3_warning_alerts) else character(0)
  )
  plot_cids <- paste0("cid_plot_", seq_along(all_alerts))
  plot_paths <- file.path("plots", paste0(all_alerts, ".png"))
  plot_countries <- gsub("^(watch_alert_|warning_alert_)", "", all_alerts)
  alert_types <- ifelse(grepl("^watch_alert_", all_alerts), "watch", "warning")

  # Watch section
  watch_blocks <- purrr$map2_chr(
    # countrycode$countrycode(plot_countries[alert_types == "watch"], origin = "iso3c", destination = "country.name"),
    "",
    plot_cids[alert_types == "watch"],
    ~ glue$glue("<h3>{.x}</h3>\n<img src=\"cid:{.y}\" width=\"100%\"/>")
  )

  # Warning section
  warning_blocks <- purrr$map2_chr(
    # countrycode$countrycode(plot_countries[alert_types == "warning"], origin = "iso3c", destination = "country.name"),
    "",
    plot_cids[alert_types == "warning"],
    ~ glue$glue("<h3>{.x}</h3>\n<img src=\"cid:{.y}\" width=\"100%\"/>")
  )

  # Combine only non-empty sections
  section_parts <- c()
  if (length(watch_blocks) > 0) {
    section_parts <- c(section_parts, "<h3><strong>Watch Alerts</strong></h3> <p>Watch Alerts signal the highest weekly increase in the past 12 months, with at least 500 reported cases.</p>", paste(watch_blocks, collapse = "\n"))
  }
  if (length(warning_blocks) > 0) {
    section_parts <- c(section_parts, "<h3><strong>Warning Alerts</strong></h3> <p>Warning Alerts indicate an exceptional increase in cases, surpassing the historical 99th percentile and exceeding 500 cases.</p>", paste(warning_blocks, collapse = "\n"))
  }

  plot_section_html <- paste(section_parts, collapse = "\n<hr>\n")

  # --- Data to render ---
  watch_alerts_html <- if (length(iso3_watch_alerts) > 0) {
    alert_texts <- purrr$map_chr(iso3_watch_alerts, function(iso3) {
      date_val <- alert_dates_df$new_watch_alert[alert_dates_df$iso3 == iso3]
      country <- countrycode$countrycode(iso3, origin = "iso3c", destination = "country.name")
      sprintf("%s (%s)", country, format(date_val, "%d %b %Y"))
    })
    sprintf("<li>Watch Alerts for: %s</li>", paste(alert_texts, collapse = ", "))
  } else {
    ""
  }

  warning_alerts_html <- if (length(iso3_warning_alerts) > 0) {
    alert_texts <- purrr$map_chr(iso3_warning_alerts, function(iso3) {
      date_val <- alert_dates_df$new_warning_alert[alert_dates_df$iso3 == iso3]
      country <- countrycode$countrycode(iso3, origin = "iso3c", destination = "country.name")
      sprintf("%s (%s)", country, format(date_val, "%d %b %Y"))
    })
    sprintf("<li>Warning Alerts for: %s</li>", paste(alert_texts, collapse = ", "))
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

  # --- Getting the Distribution List ---
  source("monitoring/distribution_list.R")
  distribution_list <- get_distribution_list(test_list = TRUE)
  to_list <- distribution_list |> dplyr$filter(info == "to") |> dplyr$pull(email)
  cc_list <- distribution_list |> dplyr$filter(info == "cc") |> dplyr$pull(email)

  # --- Compose email ---
  email <- emayili$envelope() |>
    emayili$from(sprintf('OCHA Centre for Humanitarian Data <%s>', Sys.getenv("DSCI_AWS_EMAIL_ADDRESS"))) |>
    #emayili$to("pauline.ndirangu@un.org") |>
    emayili$to(to_list) |>
    emayili$cc(cc_list) |>
    emayili$subject(paste0("🚨Global Cholera Monitoring: New Alert (", format(Sys.Date(), "%d %B %Y"), ")")) |>
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
}
