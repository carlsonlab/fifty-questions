# A) map: single-hue gradient, log10(respondents per country) -------------

max_n <- max(country_counts$n, na.rm = TRUE)
brks <- c(1, 2, 5, 10, 20, 50, 100, 200)
brks <- unique(c(brks[brks <= max_n / 1.5], max_n))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  dplyr::filter(iso_a3 != "ATA") |>
  dplyr::mutate(iso3 = iso_a3_eh)

poly_world <- suppressWarnings(sf::st_cast(world, "POLYGON"))
cen <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_geometry(poly_world))))

# fix the annoying GUF/FRA issue
world <- poly_world |>
  dplyr::mutate(
    iso3 = dplyr::if_else(iso3 == "FRA" & cen[, 1] < -40 & cen[, 2] < 20, "GUF", iso3)
  ) |>
  dplyr::left_join(country_counts, by = "iso3") |>
  dplyr::mutate(n = dplyr::if_else(!is.na(n) & n > 0, n, NA_real_)) |>
  sf::st_transform("+proj=eqearth")

map_a <- ggplot2::ggplot(world) +
  ggplot2::geom_sf(ggplot2::aes(fill = n), color = "white", linewidth = 0.15) +
  ggplot2::scale_fill_gradient(
    low = "#D6E6F2", high = "#08306B",
    trans = "log10",
    breaks = brks,
    limits = c(1, max_n),
    na.value = "grey85",
    name = "Respondents\nper country",
    guide = ggplot2::guide_colorbar(
      barheight = grid::unit(4, "cm"),
      barwidth = grid::unit(0.4, "cm"),
      ticks.colour = "white"
    )
  ) +
  ggplot2::labs(title = "A) Geographic distribution of respondents") +
  ggplot2::theme_void() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 12),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "white", color = NA),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.3),
    legend.title = ggplot2::element_text(size = 9, face = "bold", color = "#1A1A1A"),
    legend.text = ggplot2::element_text(size = 9, color = "#333333")
  )

ggplot2::ggsave("output/figS1_map.png", map_a,
  width = 14, height = 7, dpi = 600, bg = "white"
)