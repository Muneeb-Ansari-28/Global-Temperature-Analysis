# Global Temperature Analysis Dashboard (1990–2024)

Interactive R Shiny application for exploring, visualizing, modeling and predicting global temperature trends across countries and continents.

## 🎯 Project Objective

Analyze historical annual mean temperature data (1990–2024) to:

- Visualize global and regional warming trends
- Compute descriptive statistics and confidence intervals
- Fit linear regression models per country
- Build machine learning models to predict temperature
- Provide interactive exploration & forecasting

## ✨ Features

- **Interactive Dashboard** with key metrics (global avg, hottest/coldest country)
- Global & continent-level temperature trend lines
- World map showing latest year temperatures (Leaflet)
- Country-specific time series (line + bar charts)
- Year-wise temperature distribution + summary statistics
- 95% confidence intervals for yearly means
- Linear regression + forecast for any country (up to 2100)
- Machine learning models:
  - Random Forest
  - Decision Tree
  - Linear Regression
- Variable importance & actual vs predicted visualization

## 🗂 Repository Structure

global-temperature-analysis/
├── proj.R                  # Main Shiny app file
├── world_temp.csv          # Original temperature dataset (1990–2024)
├── Prob_temperature_world.Rproj
├── README.md


## 📊 Dataset

- **Source**: GlobalDataLab[](https://globaldatalab.org/)
- **Years**: 1990–2024
- **Granularity**: National level (one row per country)
- **Variables included**: Country, Continent, ISO_Code, Year, Temperature (°C)

## 🛠 Technologies Used

| Category            | Tools / Packages                                 |
|---------------------|--------------------------------------------------|
| Language            | R                                                |
| Web App Framework   | shiny, shinydashboard                            |
| Visualization       | ggplot2, plotly, leaflet                         |
| Data Manipulation   | dplyr, tidyr, reshape2                           |
| Modeling            | caret, randomForest, rpart, lm                   |
| Tables & UI         | DT, scales, shinythemes                          |
| Others              | lubridate, rlang                                 |

## 🚀 How to Run Locally

### Prerequisites

- R ≥ 4.2
- RStudio recommended

### Installation

```bash
# 1. Clone the repository
git clone https://github.com/yourusername/global-temperature-analysis.git
cd global-temperature-analysis

# 2. Install required packages (run once)
Rscript -e "install.packages(c('shiny', 'shinydashboard', 'ggplot2', 'plotly', 'dplyr', 'tidyr', 'caret', 'randomForest', 'DT', 'rpart', 'rpart.plot', 'lubridate', 'leaflet', 'reshape2', 'scales', 'shinythemes'))"

# Option A – from R console / RStudio
setwd("path/to/project/folder")
shiny::runApp()

# Option B – one-liner from terminal
R -e "shiny::runApp('path/to/project/folder')"
