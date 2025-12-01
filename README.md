# Leishmaniasis Risk - Global

This repository contains the code and data used to explore the risk of **Leishmaniasis**, globally, focusing on a combined outcome of the disease (**cutaneous** and **visceral**). 

The analysis incorporates data for environmental and socio-economic drivers, including **climate, land cover, elevation, and socio-economic indices**, to provide an annual/national metric of risk.

The main methedological approach is using boosted regression trees, using the R package: *XGBoost*. 

---

## Project Structure

The repository is organized as follows:

- `01_data_processing/`       # Scripts for cleaning, formatting, and preparing raw data
- `02_data_harmonisation/`    # Scripts for harmonizing datasets across sources and scales
- `03_model_fitting/`         # Code for fitting statistical and predictive models
- `04_model_predictions/`     # Scripts for generating predictions and risk maps
- `05_post_processing/`       # Scripts for summarizing model outputs and preparing visualizations

---

## Key Features

- Assessment of **Leishmaniasis risk** by country
- Inclusion of **climate, land cover, elevation, and socio-economic indices** as predictors
- Modular pipeline for:
  - **Data processing and harmonization**
  - **Model fitting and validation**
  - **Prediction and mapping**
  - **Post-processing and visualization**

---

## Getting Started

1. Clone this repository:

```bash
git clone git@github.com:GinaCharnley/Leishmaniasis-Global.git
```

2.	Open Leishmaniasis-Global.Rproj in RStudio.

3.	Install required packages (example):

```r
install.packages(c("tidyverse", "sf", "raster", "caret"))
```
4.	Run the scripts in order:
	1.	01_data_processing → prepare and clean datasets
	2.	02_data_harmonisation → harmonize and align data
	3.	03_model_fitting → fit models
	4.	04_model_predictions → generate predictions
	5.	05_post_processing → summarize outputs and create visualizations

## Notes:
	•	Ensure all dependencies are installed before running scripts.

## Contact

Dr Gina Charnley: g.charnley19@imperial.ac.uk






