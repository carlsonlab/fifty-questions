# used to shuffle row order
set.seed(42)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

# Read raw data and keep ONLY the standardized, recoded fields
resp <- readr::read_csv("data/anon_experts.csv", show_col_types = FALSE) |>
  dplyr::rename(
    role = `Professional role`,
    title = Title,
    country = Country,
    expertise = `Recoded areas`
  ) |>
  dplyr::select(role, title, country, expertise) |>
  dplyr::mutate(
    dplyr::across(c(role, title, expertise), \(x) dplyr::na_if(stringr::str_squish(x), "")),
    id = dplyr::row_number()
  )

# Country counts
country_long <- resp |>
  dplyr::filter(!is.na(country)) |>
  tidyr::separate_longer_delim(country, delim = ",") |>
  dplyr::mutate(
    country = stringr::str_trim(country),
    iso3 = countrycode::countrycode(country, origin = "country.name", destination = "iso3c")
  ) |>
  dplyr::filter(country != "")

# unmatched <- country_long |>
#   dplyr::filter(is.na(iso3)) |>
#   dplyr::distinct(country)

n_resp <- nrow(resp)

country_counts <- country_long |>
  tidyr::drop_na(iso3) |>
  dplyr::distinct(id, iso3) |>
  dplyr::count(iso3, name = "n") |>
  dplyr::mutate(
    country = countrycode::countrycode(iso3, origin = "iso3c", destination = "country.name"),
    pct_mentions = 100 * n / sum(n), # share of all country mentions
    pct_respondents = 100 * n / n_resp # share of all respondents
  ) |>
  dplyr::select(country, iso3, n, pct_mentions, pct_respondents) |>
  dplyr::arrange(dplyr::desc(n), country)

readr::write_csv(country_counts, "data/processed/country_counts.csv")

# Participants by recoded/standardized role, career stage, expertise
# No country, no id, rows shuffled so row order can't map back to the raw file
participants <- resp |>
  dplyr::select(role, title, expertise) |>
  dplyr::slice_sample(prop = 1)

readr::write_csv(participants, "data/processed/participants_recoded.csv", na = "")
