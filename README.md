# COMP3020 Social Web Analytics — EA SPORTS FC 27

Group members:
- Amansingh Bhatia
- Suwaid Begg
- Avinay Nand

This repository contains the data and R code for our COMP3020 Social Web Analytics group project examining early EA SPORTS FC 27 launch discourse on YouTube.

## Project Structure

- `social_web_assignment3_PARAPHRASED_FINAL_30PAGE.Rmd` — complete report analysis
- `R/01_collect_data.R` — YouTube API data collection script
- `data_raw/` — collected YouTube comment and reply data
- `data_clean/` — balanced analysis dataset and collection summary

## Data

The project analyses nine YouTube videos across three categories:

- Review / First Impressions
- Ultimate Team
- Career Mode

Comments and replies were restricted to the first 72 hours after each video was uploaded.

The final collection contains:

- 1,388 unique top-level comments
- 480 unique replies
- 720 balanced comments used for hypothesis testing, text analysis and clustering

## API Key

The YouTube API key is not included in this repository.

A valid YouTube Data API key must be stored locally in `api_key.R` to rerun the data collection script.
EOF
