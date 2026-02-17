#===============================================================================
#                 DHS ANALYSIS - Master script


# Date: November 7th, 2025

#===============================================================================


# ----------------------- SET UP --------------------

## Clear environment and console
rm(list = ls())       # remove all objects
cat("/014")           # clear console
if(!is.null(dev.list())) dev.off()  # close plots

###--------- Packages

required_packages <- c(
  # Import & cleaning
  "haven", "dplyr", "tidyr", "readr", "tibble", "janitor", "forcats", "stringr", "tidyverse", "labelled",
  
  # Statistics & survey Design
  "summarytools", "naniar", "survey", "srvyr", "knitr", "kableExtra",
  
  # Visualization
  "ggplot2", "RColorBrewer", "scales", "ggcorrplot", "viridis", "plotly",
  
  # Mapping & spatial data
  "sf", "sp", "tmap", "ggmap", "ggrepel", "leaflet", "leaflet.extras", "htmltools", "terra", "exactextractr",
  
  # Reporting
  "flexdashboard", "rmarkdown"
)

# Identify packages that are not yet installed on the computer
uninstalled_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(uninstalled_packages)) {
  message(paste("Installing missing packages:", paste(uninstalled_packages, collapse = ", ")))
  install.packages(uninstalled_packages, dependencies = TRUE)
}

# Load all packages
lapply(required_packages, library, character.only = TRUE)

###--------- Country, year of focus and data source

country  <- "Cote_d_Ivoire" 
survey_year   <- 2021
data_source   <- "dhs"

# Defining the path
path_dhs <- getwd()


###--------- Set global aesthetics

# to avoid scientific notation to make data reports and tables more human-readable
options(scipen = 999) 

# setting a ggplot theme for all charts to ensure consistency


theme_educ <- function() {
  theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      axis.title.x = element_text(size = 9, face = "bold", margin = margin(t = 7)),
      axis.title.y = element_text(size = 9, face = "bold", margin = margin(r = 7)),
      axis.text = element_text(size = 8),
      legend.position = c(0.95, 0.15), # bottom-right but inside the plot area
      legend.justification = c("right", "bottom"),
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 8)
    )
}

theme_set(theme_educ())

###--------- Run scripts sequentially

source(file.path("00_r_script", "01_import.R"))
source(file.path("00_r_script", "02_dqa.R"))
source(file.path("00_r_script", "03_data_cleaning.R"))
source(file.path("00_r_script", "04_data_analysis.R"))
source(file.path("00_r_script", "05_data_visualization.R"))

#to generate the report doc
rmarkdown::render(file.path(path_dhs, "02_Presentations", "Reports", "Rmd visualisation study sample.Rmd"))


message("All scripts executed")

