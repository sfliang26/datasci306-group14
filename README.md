# datasci306-group14
Repository for DATASCI 306 Group 14 Final Project

This Shiny app that we've created for our final project estimates a user's biological age based on health biomarkers lifestyle behaviors collected in the NHANES study. It also helps users understand which attributes are driving their biological age and how they compare to other people in their age group.

The app has two main components:

1. **Biological Age Calculator** – interactive inputs for the user’s health markers and predicted biological age and a personalized breakdown of how each biomarker increases or decreases biological age.
2. **Population Explorer** – visualizations comparing the user to NHANES participants within +/-5 years of their age.

---

## Repository contents

- `app.R`  
  Main Shiny application containing the UI and server logic.

- `bio_age_objects.RData`  
  R data file containing the cleaned NHANES subset and the fitted linear regression model.  
  When loaded, it provides:
  - `nhanes` – cleaned adult NHANES dataset with derived variables (BioAge, DeltaAge, etc.)
  - `model` – multiple linear regression model predicting Age from standardized biomarkers.

- `README.md`  
  This file. Describes the app and how to run it locally.

---

## How to run the app locally

### 1. Prerequisites

- R (version ≥ 4.x recommended)
- RStudio (optional but convenient)
- R packages:
  - `shiny`
  - `dplyr`
  - `ggplot2`

You can install the required packages with:

```r
install.packages(c("shiny", "dplyr", "ggplot2"))

