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
      ggplot2$geom_line(color = "grey40", linewidth = 0.3) +
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
        text = ggplot2$element_text(size = 22),
        plot.title = ggplot2$element_text(size = 36, face = "bold"),
        axis.title = ggplot2$element_text(size = 28),
        axis.text = ggplot2$element_text(size = 24),
        plot.caption = ggplot2$element_text(size = 20, hjust = 1, face = "italic", margin = ggplot2$margin(t = 0.7))
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
      ggplot2$geom_line(color = "grey40", linewidth = 0.3) +
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
        text = ggplot2$element_text(size = 22),
        plot.title = ggplot2$element_text(size = 36, face = "bold"),
        axis.title = ggplot2$element_text(size = 28),
        axis.text = ggplot2$element_text(size = 24),
        plot.caption = ggplot2$element_text(size = 20, hjust = 1, face = "italic", margin = ggplot2$margin(t = 0.7))
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
