# Emerging questions in the ecology and evolution of emerging viruses

Data and code for figures 2 and 4 in Lavelle et al.

## Set up and installing packages

Everything is written in R version 4.5.2, and package versions are managed with [renv](https://rstudio.github.io/renv/), with all versions used in `renv.lock`. You'll need to have R installed on your machine.

To run everything, open R in the project root. The `.Rprofile` activates renv automatically, then run:

```r
install.packages("renv") # if you don't have it yet
renv::restore()          # installs the exact package versions in renv.lock
renv::status()           # will tell you if it worked
```

Packages are always called with `pkg::`, so there's no `library()` setup to worry about. We do this because we care for your sanity!!!!

## Repo structure

```
renv.lock                       # package versions
data/
  raw/
    anon_experts.csv            # anonymized survey responses (project team only)
    50qs_network.csv            # final 50 q's, aligned to stages
  processed/
    50qs_network_themes.csv     # like above but with themes from keywords
    country_counts_survey.csv   # respondents per country
    participants_recoded.csv    # role / career stage / expertise (standardized)
R/
  aggregate_data.R              # participant characteristics, aggregated
  fig2_respondents.R            # Figure 2: respondent characteristics
  fig4_network.R                # Figure 4: theme-topic network
output/
  fig2_characteristics.png
  fig4_network.png
```

## Reproducing figures

The scripts use relative paths, as anyone pure of heart would do. They also create `output/` before running the figure scripts.

Note that `aggregate_data.R` can't be run without the participant-level survey data. That file is held only by the project team (TEL, HR, CJC, CAS) because of privacy and data restrictions per our IRB, so it isn't included here. The script is shared so you can see how the cleaning was done (refer to the manuscript and consent sheet for more details). You don't need it to make the figures since both are produced from the aggregated files already in `data/processed/`.

```r
source("R/fig2_respondents.R")
source("R/fig4_network.R")
```
