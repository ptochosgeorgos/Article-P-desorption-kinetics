# Phosphorus Desorption Kinetics in Swiss Cropping Systems 🌾🧪

[![Quarto Publish](https://github.com/YOUR-USERNAME/YOUR-REPO-NAME/actions/workflows/quarto-publish.yml/badge.svg)](https://github.com/YOUR-USERNAME/YOUR-REPO-NAME/actions/workflows/quarto-publish.yml)

## Overview
This repository contains the data, statistical models, and manuscript source files for the journal publication derived from the ETH Zurich Master's thesis: *"P-release kinetic as a predictor for P-availability in Swiss cropping systems."*

This research challenges traditional static soil tests (STPs) by utilizing a non-linear mixed-effects modeling approach to evaluate Phosphorus (P) desorption kinetics. By characterizing the maximum equilibrium P intensity ($P_{desorb}$) and the kinetic rate constant ($k$) in a closed water-extraction system, this project establishes a highly sensitive, functional proxy for soil P-quantity and buffering capacity across different Swiss agricultural soils.

The data originates from the long-term Swiss agricultural experiment (STYCS).

## Authors & Affiliations
* **Marc Jerónimo Pérez y Ropero** (Lead Author / Researcher)
* **Prof. Dr. Emmanuel Frossard** (Supervisor) – *Institute of Agricultural Sciences, ETH Zurich*
* **Dr. Frank Liebisch** (Advisor) – *Water Protection and Substance Flows, Agroscope Reckenholz*

## Repository Structure
This project is built using **R** and **Quarto** for fully reproducible academic writing. 

* `data/` - Contains the raw and processed datasets (including `.rds` and `.csv` files) from the STYCS extractions and field trials.
* `notebooks/` - R scripts and Quarto notebooks containing the data cleaning, exploratory data analysis (EDA), and the `nlme` non-linear mixed-effects models.
* `writing/` - The Quarto (`.qmd`) source files for the manuscript, including the abstract, introduction, methods, results, and discussion. 
* `.github/workflows/` - Contains the Continuous Integration/Continuous Deployment (CI/CD) YAML files that automate the manuscript rendering.

## Reproducibility & CI/CD Pipeline
This repository utilizes a modern continuous integration workflow. Every time a commit is pushed to the `main` branch, a GitHub Actions runner automatically executes the R code, compiles the Quarto document via LuaLaTeX, and publishes the most up-to-date version of the manuscript.

**[Link to the Live Rendered Manuscript will go here once GitHub Pages is activated]**

### Local Setup
To run this project locally:
1. Clone this repository.
2. Open the `.Rproj` file in RStudio.
3. Ensure you have the necessary R packages installed (e.g., `nlme`, `dplyr`, `ggplot2`, `rmarkdown`).
4. Render the manuscript via the Quarto CLI or the RStudio "Render" button.

## Status
🚧 **Active Development:** This manuscript is currently being drafted for submission to a peer-reviewed journal.
