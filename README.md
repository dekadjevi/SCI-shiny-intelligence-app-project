# Competitive Intelligence Shiny (dashboard) App

## Objective
This project develops a Shiny application (dashboard)

## Tech Stack
- R
- Shiny
- tidyverse

## Project Structure
- metro_shiny/: Shiny application
- doc/ pdf of the scope statement and KIT WITH its corresponding KIQS
- intelligence/: strategic insights
- data/raw: datasets
- data/script :R.scripts
- data/.... all utilities for each KIQ

## How to Run

- install.packages(c("shiny","ggplot2","dplyr","tidyr","readr","stringr","base64enc","plotly","DT"))
- setwd("path/to/SCI-shiny-intelligence-app-project")
- shiny::runApp("metro_shiny")
