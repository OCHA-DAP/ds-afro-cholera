# Global Cholera Monitoring System

This repository contains an automated cholera monitoring system developed by OCHA's Centre for Humanitarian Data, in support of the Global Cholera Strategy. It uses WHO outbreak data and generates alerts based on thresholds, producing email notifications.

### 🔍 What It Does

 - Parses and processes WHO AFRO cholera bulletin data
 - Compares current data to historical baselines and triggers
 - Generates daily alert plots for countries crossing thresholds
 - Sends email alerts with embedded graphics and summary commentary

### 📦 Repository Structure

 - `monitoring/`: Email templates
 - `exploration.Rmd`: Code to produce exploratory analysis and visualizations
 - `exploration.html`: Rendered HTML report from the exploratory analysis
 - `README.md`: Documentation

### 🚀 Running It
#### Automated via GitHub Actions

This project includes a workflow (.github/workflows/daily_alerts.yml) that runs the alert system every day at 06:00 UTC. See the GitHub Actions section for setup.

### 🧰 Dependencies
This project uses the {box} module system and depends on the following packages:
 - box, dplyr, purrr, ggplot2, lubridate, stringr, janitor, readr, zoo, gghdx, countrycode, whisker, emayili, AzureStor, glue

### 📬 Contact

Developed by the Centre for Humanitarian Data. Questions or suggestions? Contact: pauline.ndirangu@un.org

### 📝 License

This project is open-source under the MIT License.
