# Emerging questions in the ecology and evolution of emerging viruses

Data and code for figures 1 and 2 in Lavelle et al.

## Set up and installing packages

Everything is written in R version 4.5.2, and package versions are managed with [renv](https://rstudio.github.io/renv/), with all versions used in `renv.lock`. You'll need to have R installed on your machine.

First time here? Open R in the project root. The `.Rprofile` activates renv automatically, then run:

```r
install.packages("renv") # if you don't have it yet
renv::restore()          # installs the exact package versions in renv.lock
```

Packages are always called with `pkg::`, so there's no `library()` setup to worry about.

## What's in here

```
renv.lock                     # package versions
data/
  raw/
    anon_experts.csv          # anonymized survey responses (project team only)
    original_qs.csv           # full pool of submitted questions + original themes
  processed/
    50qs_network.csv          # final 50 q's, aligned to stages, themes, + keywords
    country_counts.csv        # respondents per country
    participants_recoded.csv  # role / career stage / expertise (standardized)
R/
  aggregate_data.R            # participant characteristics, aggregated
  fig1_respondents.R          # Figure 1: respondent characteristics
  fig2_network.R              # Figure 2: theme-topic network
output/
  fig1_characteristics.png
  fig2_network.png
```

## Running it

Run everything from the project root, since the scripts use relative paths, and create `output/` before running the figure scripts.

**A note on the data:** `aggregate_data.R` can't be run without the participant-level survey data. That file is held only by the project team because of privacy and data restrictions, so it isn't included here. The script is shared so you can see exactly how the cleaning was done (see the manuscript and consent sheet for more details). You don't need it to make the figures since both are produced from the aggregated files already in `data/processed/`.

```r
source("R/aggregate_data.R")   # project team only (needs the raw survey data)
source("R/fig1_respondents.R")
source("R/fig2_network.R")
```

## Notes

- `original_qs.csv` isn't used by any script - it's just the submitted questions and is here for reference
