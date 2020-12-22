# How the 2019 Canadian Federal Election would have been different if 'everyone' had voted

## Legend

**Bold**: Variable names to change.

*Italic*: Details required

## Overview

This repository contains code and data for predict if 'everyone' had voted for 2019 Canadian Federal Election. It was created by Boyu Cao. The purpose of this project is to summarize the results based on the statistical models we built.

Note that data sets are not included, since the public distribution of those data sets are prohibited; nonetheless, we provided the details below on how to get those data on your own.

The sections of this repo are: inputs, outputs, scripts.

Inputs contain data that are unchanged from their original. We use two datasets: 

- *[Survey data - detail how to get the CES data.]*
## Installation

  You can install the current version of this package using:

  ``` r
  devtools::install_github("hodgettsp/cesR")
  ```
  Then `clean_data.R` will do the rest things.
  
  
- *[GSS data - detail how to get the GSS data.]*
  1. Go to: http://www.chass.utoronto.ca/
  2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
  3. Click SDA @ CHASS, should redirect to sign in. Sign in.
  4. Continue in English (you're welcome to use the French, but we probably can't help you too much).
  5. Crtl F GSS, click
  6. Click "Data" on the one you want. We used 2017.
  7. Click download
  8. Select CSV data file, data definitions for STATA (gross, but stick with it for now).
  9. Can select all variables by clicking button next to green colored "All". Then continue.
  10. Create the files, download and save

Outputs contain data that are modified from the input data, the report and supporting material.

- *model*
  - `brms_model_cons.rds`
  - `brms_model_green.rds`
  - `brms_model_libral.rds`
  - `brms_model_ndp.rds`
  - `brms_model_peo.rds`
  - `brms_model_quebec.rds`
- *results*
  - `results_C.csv`
  - `results_G.csv`
  - `results_L.csv`
  - `results_N.csv`
  - `results_P.csv`
  - `results_Q.csv`

Scripts contain R scripts that take inputs and outputs to reproduce the results:

- `build_model.R`
- `clean_data.R`
- `debug.R`
- `compute.R`
- `gss_cleaning-1.R`

The report file is placed on the root:
- `report.pdf`
- `report.rmd`