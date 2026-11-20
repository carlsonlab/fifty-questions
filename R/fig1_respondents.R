country_counts <- readr::read_csv("data/processed/country_counts_survey.csv", show_col_types = FALSE)

resp <- readr::read_csv("data/processed/participants_recoded.csv", show_col_types = FALSE) |>
  dplyr::mutate(
    id = dplyr::row_number(),
    dplyr::across(c(role, title, expertise), \(x) dplyr::na_if(stringr::str_squish(x), ""))
  )

n_resp <- nrow(resp)

cont_list <- c("Africa", "Asia", "Europe", "North America", "South America", "Oceania")
colors <- stats::setNames(
  as.character(MoMAColors::moma.colors("Sidhu", n = length(cont_list), type = "discrete")),
  cont_list
)

# A) map
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(iso_a3 != "ATA") |>
  dplyr::mutate(
    iso3 = iso_a3_eh,
    continent = dplyr::if_else(continent %in% cont_list, continent, NA_character_)
  )

poly_world <- suppressWarnings(sf::st_cast(world, "POLYGON"))
cen <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(poly_world))))

# fix the annoying GUF/FRA issue
world <- poly_world |>
  dplyr::mutate(
    iso3 = dplyr::if_else(iso3 == "FRA" & cen[, 1] < -40 & cen[, 2] < 20, "GUF", iso3),
    continent = dplyr::if_else(iso3 == "GUF", "South America", continent)
  ) |>
  dplyr::left_join(country_counts, by = "iso3") |>
  dplyr::mutate(
    fill_group = factor(
      dplyr::if_else(!is.na(n) & n > 0, continent, "No respondents"),
      levels = c(cont_list, "No respondents")
    )
  ) |>
  sf::st_transform("+proj=eqearth")

legend_items <- c(cont_list, "No respondents")

map_a <- ggplot2::ggplot(world) +
  ggplot2::geom_sf(ggplot2::aes(fill = fill_group), color = "white", linewidth = 0.15) +
  ggplot2::scale_fill_manual(
    values = c(colors, "No respondents" = "grey85"),
    breaks = legend_items,
    na.value = "grey85",
    name = "Continent\n(\u2265 1 respondent)",
    drop = FALSE
  ) +
  ggplot2::labs(title = "A) Geographic distribution of respondents") +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 20),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.3),
    legend.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.title = ggplot2::element_text(size = 14, face = "bold", color = "#1A1A1A"),
    legend.text = ggplot2::element_text(size = 14, color = "#333333"),
    legend.key.size = grid::unit(0.5, "cm")
  )

# Bar charts
role_counts <- resp |>
  dplyr::mutate(role = tidyr::replace_na(role, "Not reported")) |>
  tidyr::separate_longer_delim(role, ",") |>
  dplyr::mutate(category = stringr::str_trim(role)) |>
  dplyr::distinct(id, category) |>
  dplyr::count(category)

# in order dont change
stage_levels <- c(
  "Graduate Student", "Postdoctoral Researcher", "Assistant Professor",
  "Associate Professor", "Professor", "Emeritus Professor",
  "Research Scientist and Staff", "Director", "Not reported"
)

stage_counts <- resp |>
  dplyr::mutate(category = factor(tidyr::replace_na(title, "Not reported"), levels = stage_levels)) |>
  dplyr::count(category) |>
  dplyr::mutate(category = as.character(category))

exp_counts <- resp |>
  dplyr::mutate(expertise = tidyr::replace_na(expertise, "Not reported")) |>
  tidyr::separate_longer_delim(expertise, ";") |>
  dplyr::mutate(category = stringr::str_trim(expertise)) |>
  dplyr::filter(category != "") |>
  dplyr::distinct(id, category) |>
  dplyr::count(category)

bar_panel <- function(df, title, sort = TRUE, x_max = 1) {
  df <- df |>
    dplyr::mutate(
      category = if (sort) {
        forcats::fct_reorder(factor(category), n)
      } else {
        forcats::fct_rev(factor(category, levels = unique(category)))
      },
      category = if ("Not reported" %in% levels(category)) {
        forcats::fct_relevel(category, "Not reported", after = 0)
      } else {
        category
      }
    )

  ggplot2::ggplot(df, ggplot2::aes(x = n / n_resp, y = category)) +
    ggplot2::geom_col(fill = "grey35", width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%d (%.0f%%)", n, 100 * n / n_resp)),
      hjust = -0.1, size = 4, color = "grey25"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, x_max),
      labels = scales::percent,
      breaks = seq(0, x_max, 0.25),
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(title = title, x = "Respondents (%)", y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 20),
      plot.title.position = "plot",
      axis.text.y = ggplot2::element_text(color = "grey15", size = 14),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

p_role <- bar_panel(role_counts, "B) Professional role") +
  ggplot2::theme(axis.title.x = ggplot2::element_blank())
p_stage <- bar_panel(stage_counts, "C) Career stage", sort = FALSE)
p_exp <- bar_panel(exp_counts, "D) Areas of expertise")

left <- patchwork::wrap_plots(p_role, p_stage, ncol = 1, heights = c(1, 1.3))
bottom <- patchwork::wrap_plots(left, p_exp, nrow = 1, widths = c(1, 1.1))
fig1 <- patchwork::wrap_plots(
  patchwork::wrap_elements(full = map_a), bottom,
  ncol = 1, heights = grid::unit(c(6, 1), c("in", "null"))
)

ggplot2::ggsave("output/fig1_characteristics.png", fig1,
  width = 14, height = 11.5, dpi = 600, bg = "white"
)


