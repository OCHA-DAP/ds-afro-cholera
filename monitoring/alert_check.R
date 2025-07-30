alert_dates_df <- data.frame(
  iso3 = character(),
  new_watch_alert = as.Date(character()),
  new_warning_alert = as.Date(character()),
  stringsAsFactors = FALSE
)
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
                         color = "#939393", size = 1) +
      ggplot2$geom_point(data = subset(df_country, alert == TRUE & date == row$last_watch_alert_new),
                         ggplot2$aes(x = date, y = cholera_cases),
                         color = "darkorange", size = 1) +
      ggplot2$labs(
        title = paste("Cholera Cases for", countrycode$countrycode(country, origin = "iso3c", destination = "un.name.en")),
        subtitle = "Watch Alerts based on Increases in Cases",
        x = "Date", y = "Number of Cases",
        caption = "<span style='color:darkorange;'>● Orange Dot:</span> New Watch Alert<br>● Grey Dot: Previous Watch Alert<br>Labels show the number of cholera cases with the increase in parentheses"

      ) +
      ggplot2$scale_x_date(breaks = breaks_6mo <- seq(from = lubridate$floor_date(min(df_country$date, na.rm = TRUE), "year"),
                                        to = lubridate$ceiling_date(max(df_country$date, na.rm = TRUE), "month"),
                                        by = "6 months"),
                           labels = ifelse(lubridate$month(breaks_6mo) == 1,
                                           format(breaks_6mo, "%b\n%Y"),
                                           format(breaks_6mo, "%b\n")),
                           expand = c(0.01, 0)) +
      ggplot2$scale_y_continuous(labels = scales$comma) +
      ggplot2$expand_limits(y = max(df_country$cholera_cases, na.rm = TRUE) * 1.1) +
      ggplot2$geom_text(
        data = subset(df_country, alert == TRUE & date == row$last_watch_alert_new),
        ggplot2$aes(
          x = date,
          y = cholera_cases,
          label = paste0(scales$comma(cholera_cases), " (+", scales$comma(weekly_increase), ")")
        ),
        color = "darkorange",
        size = 6.5,
        vjust = -1
      ) +
      gghdx$gghdx() +
      ggplot2$theme(
        text = ggplot2$element_text(size = 20),
        plot.title = ggplot2$element_text(size = 32, face = "bold"),
        axis.title = ggplot2$element_text(size = 24),
        axis.text = ggplot2$element_text(size = 22),
        plot.caption = ggtext$element_markdown(size = 18, hjust = 1, lineheight = 0.4,
                                            margin = ggplot2$margin(t = 0.7)),
        axis.text.x = ggplot2$element_text(lineheight = 0.35)
      )

    # Save plot
    plot_path <- file.path("plots", paste0("watch_alert_", country, ".png"))
    ggplot2$ggsave(plot_path, plot = p, width = 6, height = 3, dpi = 300)

    # Add to alert_dates_df
    alert_dates_df <<- dplyr$bind_rows(alert_dates_df, data.frame(
      iso3 = country,
      new_watch_alert = row$last_watch_alert_new,
      new_warning_alert = NA,
      stringsAsFactors = FALSE
    ))
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
                         color = "#939393", size = 1) +
      ggplot2$geom_point(data = subset(df_country, alert_level == "p99" & date == row$last_warning_alert_new),
                         ggplot2$aes(x = date, y = cholera_cases),
                         color = "tomato", size = 1) +
      ggplot2$labs(
        title = paste("Cholera Cases for", countrycode$countrycode(country, origin = "iso3c", destination = "un.name.en")),
        subtitle = "Warning Alerts based on Increases in Cases",
        x = "Date", y = "Number of Cases",
        caption = "<span style='color:tomato;'>● Red Dot:</span> New Warning Alert<br>● Grey Dot: Previous Warning Alert<br>Labels show the number of cholera cases with the increase in parentheses"
      ) +
      ggplot2$scale_x_date(breaks = breaks_6mo <- seq(from = lubridate$floor_date(min(df_country$date, na.rm = TRUE), "year"),
                                                      to = lubridate$ceiling_date(max(df_country$date, na.rm = TRUE), "month"),
                                                      by = "6 months"),
                           labels = ifelse(lubridate$month(breaks_6mo) == 1,
                                           format(breaks_6mo, "%b\n%Y"),
                                           format(breaks_6mo, "%b\n")),
                           expand = c(0.01, 0)) +
      ggplot2$scale_y_continuous(labels = scales$comma) +
      ggplot2$expand_limits(y = max(df_country$cholera_cases, na.rm = TRUE) * 1.1) +
      ggplot2$geom_text(
        data = subset(df_country, alert_level == "p99" & date == row$last_warning_alert_new),
        ggplot2$aes(
          x = date,
          y = cholera_cases,
          label = paste0(scales$comma(cholera_cases), " (+", scales$comma(weekly_increase), ")")
        ),
        color = "tomato",
        size = 6.5,
        vjust = -1
      ) +
      gghdx$gghdx() +
      ggplot2$theme(
        text = ggplot2$element_text(size = 20),
        plot.title = ggplot2$element_text(size = 32, face = "bold"),
        axis.title = ggplot2$element_text(size = 24),
        axis.text = ggplot2$element_text(size = 22),
        plot.caption = ggtext$element_markdown(size = 18, hjust = 1, lineheight = 0.4,
                                            margin = ggplot2$margin(t = 0.7)),
        axis.text.x = ggplot2$element_text(lineheight = 0.35)
      )

    # Save plot
    plot_path <- file.path("plots", paste0("warning_alert_", country, ".png"))
    ggplot2$ggsave(plot_path, plot = p, width = 6, height = 3, dpi = 300)

    # Add to alert_dates_df
    # Check if country already in df then update, else add
    if (country %in% alert_dates_df$iso3) {
      idx <- which(alert_dates_df$iso3 == country)
      if (length(idx) == 1) {
        alert_dates_df$new_warning_alert[idx] <<- row$last_warning_alert_new
      }
      message("Updated row: ", idx, " with date: ", row$last_warning_alert_new)
    } else {
      message("New Warning Entry")
      alert_dates_df <<- dplyr$bind_rows(
        alert_dates_df,
        data.frame(
          iso3 = country,
          new_watch_alert = as.Date(NA),
          new_warning_alert = row$last_warning_alert_new
        )
      )
    }

  }
})

if (length(iso3_warning_alerts) > 0) {
  warning_alerts_raised <- TRUE
  message("Countries with WARNING alerts: ", paste(iso3_warning_alerts, collapse = ", "))
}
