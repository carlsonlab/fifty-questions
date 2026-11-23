set.seed(42)

font_scale <- 1.3
node_size_range <- c(4, 15)
# 1 = label outside the node edge, 0 = label centered on node
label_hug <- 0.8

themes <- c(
  "ECAD" = "Environmental Changes and Anthropogenic Disturbance (ECAD)",
  "BHIC" = "Host Interactions and Communities (BHIC)",
  "MDT" = "Methods, Data, and Tools (MDT)",
  "OPPR" = "Outbreak Prevention, Preparedness, and Response (OPPR)",
  "TDE" = "Transmission Dynamics and Emergence (TDE)",
  "VE" = "Viral Evolution (VE)",
  "WDHD" = "Within-Host Dynamics and Host Defense (WDHD)"
)

theme_order <- c("VE", "WDHD", "BHIC", "TDE", "ECAD", "OPPR", "MDT")
theme_colors <- stats::setNames(
  as.character(MoMAColors::moma.colors("Panton", n = length(themes))),
  themes[theme_order]
)

df <- readr::read_csv("data/raw/50qs_network.csv", show_col_types = FALSE) |>
  dplyr::mutate(ms_theme = stringr::str_to_upper(stringr::str_squish(ms_theme))) |>
  dplyr::filter(!is.na(ms_theme), !is.na(ms_question)) |>
  dplyr::mutate(
    full_theme = unname(themes[ms_theme]),
    clean_text = stringr::str_to_lower(ms_question)
  )


# Keywords are defined here with regex -- it's a bit easier to look at the CSV, and then add string patterns here that you want to include for a category in order to be consistent about what gets a tag!
kw_patterns <- c(
  "immune function & stress" = "\\bimmun\\w*|stress\\w*",
  "competence & susceptibility" = "competen\\w*|susceptib\\w*",
  "traits & life history" = "\\btraits?\\b|life[- ]?histor(y|ies)|life[- ]?course|life[- ]?span|ontogen\\w*|host-intrinsic|host factors",
  "host range & specialism" = "host breadth|host range|specialis\\w*|generalis\\w*",
  "geographic range" = "geograph\\w*|\\brange[sd]?\\b|\\bdispers\\w*|movement",
  "habitat modification" = "land[- ]?use|land[- ]?cover|\\burban\\w*|habitat\\w*|fragment\\w*|restor\\w*|landscape\\w*",
  "human-animal interface" = "human[- ]wildlife|human[- ]animal|\\bcontact\\w*|interface\\w*|butcher\\w*|pet ownership",
  "forecasting & projections" = "predict\\w*|forecast\\w*|indicator\\w*",
  "countermeasures & interventions" = "intervention\\w*|countermeasure\\w*|vaccin\\w*|restoration",
  "sampling & surveillance" = "sampl\\w*|surveil\\w*|detect\\w*|bias(es)?|monitor\\w*",
  "data & infrastructure" = "archiv\\w*|museum|satellite|infrastructure|artificial intelligence|machine learning|data",
  "governance & implementation" = "one health|stakeholder\\w*|barrier\\w*|facilitator\\w*|\\bscaled\\b|scal(e|ing) up|adoption|cost-effective\\w*|institutional|interdisciplinary|transdisciplinary|multidisciplinary|polic(y|ies)",
  "human populations" = "human[s]?|clinical",
  "infection outcomes" = "asymptomatic|subclinical|severity|shedd?ing|\\bshed\\b|sheds\\b|viral load|virus load",
  "coinfection & microbial interactions" = "co[- ]?infection\\w*|microbiome",
  "spillover & emergence" = "spill[- ]?over|\\bemerg\\w*|establish\\w*",
  "farming & agriculture" = "livestock|agricultur\\w*|farm\\w*",
  "reservoirs" = "reservoir(s)?|host communities",
  "bats" = "\\bbats?\\b|guano",
  "arboviruses" = "arbovirus(es)?|\\bvectors?\\b",
  "evolution & selection" = "evolution\\w*|evolv\\w*|selection pressure\\w*|\\bfitness\\b|antigenic|immune escape|adaptive (mutation|change|evolution)\\w*|genetic diversity|mutat\\w*|reassort\\w*|recombin\\w*|genetic material exchange",
  "climate change" = "climate\\s+(change|hazard)\\w*|climatic|global\\s+(change|warming)|warming",
  "preparedness" = "preparedness|prepare\\w*",
  "participatory & indigenous knowledge" = "participatory|indigenous",
  "environmental factors" = "environment\\w*|anthropogen\\w*|ecosystem\\w*|thermal|temperature\\w*|global changes?|climate|land use|land-use|landscape[s]?|urbanization|natural systems|ecolog\\w*"
)

keywords <- names(kw_patterns)

# Match each keyword to questions 
match_long <- purrr::imap_dfr(kw_patterns, \(pat, kw) {
  df |>
    dplyr::filter(stringr::str_detect(clean_text, pat)) |>
    dplyr::transmute(ms_number, keyword = kw, full_theme)
})

# Row (question/keyword) = one edge 
# n_questions = weight
theme_edges <- dplyr::count(match_long, full_theme, keyword, name = "n_questions")

# print(match_long |> dplyr::count(keyword, sort = TRUE), n = Inf)

topics_by_q <- match_long |>
  dplyr::summarise(topics = paste(keyword, collapse = "; "), .by = ms_number)

df_out <- df |>
  dplyr::select(-dplyr::any_of(c("topics", "clean_text", "full_theme"))) |>
  dplyr::left_join(topics_by_q, by = "ms_number")

readr::write_csv(df_out, "data/processed/50qs_network_topics.csv")

# Node table for every theme and keyword and give it the type label to keep theme nodes separate from keyword/topic nodes
# size_val = number of questions matching each keyword
nodes <- tibble::tibble(
  name = c(names(theme_colors), keywords),
  node_type = rep(c("theme", "keyword"), c(length(theme_colors), length(keywords)))
) |>
  dplyr::filter(name %in% c(theme_edges$full_theme, theme_edges$keyword)) |>
  dplyr::left_join(dplyr::count(match_long, keyword, name = "size_val"), by = c(name = "keyword"))

# theme name (from) and keywords (to) 
# from_theme is its own column so edges can be colored by theme 
df_tg <- tidygraph::tbl_graph(
  nodes = nodes,
  edges = dplyr::transmute(theme_edges, from = full_theme, to = keyword, n_questions, from_theme = full_theme),
  directed = FALSE
) |>
  tidygraph::activate(nodes) |>
  dplyr::mutate(
    # Keywords only connect to themes, degree = number of themes a topic spans
    n_themes = tidygraph::centrality_degree(),
    # Concentric layout
    # themes on the outer ring (0), topics get the number of themes they span, so the more connected topics are in the middle
    cent = ifelse(node_type == "theme", 0, n_themes + 0.01),
    # label text + repel point size (themes get a big one so labels stay away from them), and abbreviations for inside the theme nodes
    lbl = ifelse(node_type == "keyword", stringr::str_wrap(name, 20), ""),
    node_pt = node_size_range[1] + diff(node_size_range) * sqrt(scales::rescale(size_val)),
    # shrink it by label_hug so labels stay
    pt = ifelse(node_type == "theme", 45, label_hug * node_pt),
    abbr = names(themes)[match(name, themes)]
  )

# Edit concentric layout to space the themes evenly around outer ring
# I found this to be really helpful: https://schochastics.github.io/netVizR/
xy <- graphlayouts::layout_with_centrality(df_tg, cent = igraph::V(df_tg)$cent)

# themes in order around the ring based on angle from center
th <- match(themes[theme_order], igraph::V(df_tg)$name)
outer_ring_radius <- mean(sqrt(rowSums(xy[th, ]^2)))

# evenly spaced angles (2*pi/n apart), starting at 12 o'clock (pi/2) and moving clockwise
a <- pi / 2 - 2 * pi * (seq_along(th) - 1) / length(th)
xy[th, ] <- cbind(outer_ring_radius * cos(a), outer_ring_radius * sin(a))

# Rotate each keyword to the weighted-average direction of its themes (weights = n_questions)
# keeps each keyword's radius from the centrality layout, so the more connected topics stay in the middle
theme_xy <- tibble::tibble(full_theme = igraph::V(df_tg)$name[th], x = xy[th, 1], y = xy[th, 2])
kw_ang <- theme_edges |>
  dplyr::left_join(theme_xy, by = "full_theme") |>
  dplyr::summarise(ang = atan2(sum(n_questions * y), sum(n_questions * x)), .by = keyword)

kw_idx <- which(igraph::V(df_tg)$node_type == "keyword")
new_ang <- kw_ang$ang[match(igraph::V(df_tg)$name[kw_idx], kw_ang$keyword)]
r0 <- sqrt(rowSums(xy[kw_idx, , drop = FALSE]^2))
xy[kw_idx, ] <- cbind(r0 * cos(new_ang), r0 * sin(new_ang))

# Get node size (nubmer of q's matching eatch kw)
sz <- igraph::V(df_tg)$size_val[kw_idx]

# Set radius - node_scale (bigger = more spacing, smalllest nodes spacing is about half the biggest nodes regardless)
node_scale <- 0.06 * outer_ring_radius
r_kw <- node_scale * (0.5 + 0.5 * sz / max(sz))

# Nudge overlapping keyword nodes apart
# packcircles does iterative pairwise repulsion: overlapping circles move apart until nothing overlaps
kw_circles <- packcircles::circleRepelLayout(
  data.frame(x = xy[kw_idx, 1], y = xy[kw_idx, 2], r = r_kw + 0.005 * outer_ring_radius),
  sizetype = "radius", wrap = FALSE, maxiter = 1000
)
xy[kw_idx, ] <- as.matrix(kw_circles$layout[, 1:2])

# keep keywords inside the theme ring so their labels have room and don't crowd the theme nodes
# (scale factor is 1 for anything already inside)
max_r <- 0.85 * outer_ring_radius
rr <- sqrt(rowSums(xy[kw_idx, , drop = FALSE]^2))
xy[kw_idx, ] <- xy[kw_idx, , drop = FALSE] * pmin(1, max_r / rr)

# MAKE THE BLOBS by creating a separate data frame for the "hull" coordinates
# lookup of final xy coordinates by node name, used for everything the blob needs to go around
node_xy <- tibble::tibble(name = igraph::V(df_tg)$name, x = xy[, 1], y = xy[, 2])

# starting with theme node, draw a heptagon around it and get coords of 7 corners
# geom_mark_hull needs >= 3 points per group to draw a polygon (ggforce treats 1 point as a point, 2 as a line),
# so pad each theme node with a ring of 7 points
# see: https://github.com/thomasp85/ggforce/issues/299
theme_ring <- tidyr::expand_grid(name = unname(themes[theme_order]), k = 0:6) |>
  dplyr::left_join(node_xy, by = "name") |>
  dplyr::transmute(
    grp = name,
    x = x + 20 * cos(2 * pi * k / 7),
    y = y + 20 * sin(2 * pi * k / 7)
  )

# xy coords of each keyword in each theme for geom_mark_hull, plus the theme rings
blob_df <- theme_edges |>
  dplyr::left_join(node_xy, by = c(keyword = "name")) |>
  dplyr::transmute(grp = full_theme, x, y) |>
  dplyr::bind_rows(theme_ring)


# xy = node coordinates (col 1 = x, col 2 = y)
# can play w concavity and radius for increasing the organic-ness of the shape
fig2 <- ggraph::ggraph(df_tg, layout = "manual", x = xy[, 1], y = xy[, 2]) +
  ggforce::geom_mark_hull(
    data = blob_df,
    ggplot2::aes(x = x, y = y, fill = grp, group = grp),
    concavity = 10, radius = grid::unit(5, "mm"),
    alpha = 0.1, color = NA,
    inherit.aes = FALSE, show.legend = FALSE
  ) +
  # theme to keyword with n_questions + colored by theme
  ggraph::geom_edge_link(
    ggplot2::aes(width = n_questions, color = from_theme, alpha = n_questions)
  ) +
  ggraph::scale_edge_width(
    range = c(0.6, 4), breaks = seq_len(max(theme_edges$n_questions)),
    name = "Number of questions\nin theme per topic"
  ) +
  # stronger links darker
  ggraph::scale_edge_alpha_continuous(range = c(0.15, 0.8), guide = "none") +
  ggraph::scale_edge_color_manual(values = theme_colors, guide = "none") +
  # keyword nodes sized by questions
  ggraph::geom_node_point(
    data = \(d) dplyr::filter(d, node_type == "keyword"),
    ggplot2::aes(size = size_val), shape = 21, fill = "grey92", color = "grey40", stroke = 0.3
  ) +
  ggplot2::scale_size_continuous(
    range = node_size_range,
    breaks = scales::breaks_pretty(n = 4),
    name = "Number of questions\nper topic"
  ) +
  # theme nodes are colorful and larger and heptagons
  ggfoundry::geom_casting(
    data = \(d) dplyr::filter(d, node_type == "theme"),
    ggplot2::aes(x = x, y = y, fill = name),
    shape = "heptagon", colour = "white", size = 0.18
  ) +
  # low force + high force_pull
  # (min.segment.length) for big pushes
  ggraph::geom_node_text(
    ggplot2::aes(label = lbl, point.size = pt),
    size = 3.8 * font_scale,
    lineheight = 0.85,
    color = "black",
    repel = TRUE,
    seed = 42,
    max.overlaps = Inf,
    force = 0.5,
    force_pull = 3,
    box.padding = 0.05,
    point.padding = 0,
    min.segment.length = grid::unit(0.5, "lines"),
    bg.color = "white", bg.r = 0.15,
    segment.size = 0.3, segment.color = "grey30"
  ) +
  #  abbreviations inside theme nodes
  ggraph::geom_node_text(
    data = \(d) dplyr::filter(d, node_type == "theme"),
    ggplot2::aes(label = abbr),
    size = 4.8 * font_scale, fontface = "bold", color = "white"
  ) +
  # LEGEND
  ggplot2::scale_fill_manual(
    values = theme_colors,
    limits = unname(themes[theme_order]),
    labels = unname(stringr::str_wrap(themes[theme_order], width = 32)),
    name = "Theme"
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      order = 1,
      override.aes = list(size = 15),
      theme = ggplot2::theme(
        legend.key.width = grid::unit(2.4, "lines"),
        legend.key.height = grid::unit(2.4, "lines")
      )
    ),
    # topic node size legend
    size = ggplot2::guide_legend(
      order = 2,
      theme = ggplot2::theme(
        legend.key.spacing.y = grid::unit(4, "pt")
      )
    ),
    edge_width = ggplot2::guide_legend(
      order = 3,
      theme = ggplot2::theme(
        legend.key.width = grid::unit(2.4, "lines"),
        legend.margin = ggplot2::margin(l = 10),
        legend.title = ggplot2::element_text(margin = ggplot2::margin(l = -10, b = 10))
      )
    )
  ) +
  ggplot2::theme_void(base_size = 12 * font_scale) +
  ggplot2::theme(
    legend.position = "right",
    legend.title = ggplot2::element_text(face = "bold", size = 16 * font_scale),
    legend.text = ggplot2::element_text(size = 13 * font_scale, lineheight = 0.9),
    legend.key.size = grid::unit(1, "lines"),
    legend.key.spacing.y = grid::unit(2, "pt"),
    legend.box.spacing = grid::unit(2, "cm"),
    plot.margin = ggplot2::margin(10, 40, 10, 10)
  ) +
  ggplot2::coord_fixed(clip = "off")

ggplot2::ggsave("output/fig2_network.png", fig2, width = 18, height = 12, dpi = 600, bg = "white")