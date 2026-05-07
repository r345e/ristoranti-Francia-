#!/usr/bin/env Rscript

# ================================
# Mappa densità ristoranti - Sud Francia
# ================================

set.seed(42)

detect_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  file_path <- sub(file_arg, "", args[grep(file_arg, args)])
  if (length(file_path) > 0) return(dirname(normalizePath(file_path)))
  getwd()
}

bootstrap_packages <- function(base_dir) {
  suppressPackageStartupMessages({
    local_lib <- file.path(base_dir, ".r_libs")
    if (!dir.exists(local_lib)) dir.create(local_lib, recursive = TRUE)
    .libPaths(c(local_lib, .libPaths()))

    is_macos <- grepl("darwin", R.version$platform)
    core_pkgs <- c("readr", "dplyr", "stringr", "ggplot2", "viridis", "maps")
    optional_pkgs <- c("curl", "sf")

    missing_core <- core_pkgs[!vapply(core_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_core) > 0) {
      install.packages(
        missing_core,
        repos = "https://cloud.r-project.org",
        type = if (is_macos) "binary" else "source"
      )
    }

    missing_opt <- optional_pkgs[!vapply(optional_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing_opt) > 0) {
      # Non bloccare lo script se optional falliscono (sf è spesso problematico su macOS).
      try(
        install.packages(
          missing_opt,
          repos = "https://cloud.r-project.org",
          type = if (is_macos) "binary" else "source"
        ),
        silent = TRUE
      )
      if ("sf" %in% missing_opt && !requireNamespace("sf", quietly = TRUE)) {
        # Tentativo extra: r-universe per pacchetti spatial
        try({
          options(repos = c(r_spatial = "https://r-spatial.r-universe.dev", CRAN = "https://cloud.r-project.org"))
          install.packages("sf")
        }, silent = TRUE)
      }
    }

    still_missing_core <- core_pkgs[!vapply(core_pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(still_missing_core) > 0) {
      stop(
        paste0("Pacchetti core non installati: ", paste(still_missing_core, collapse = ", "), "."),
        call. = FALSE
      )
    }

    invisible(lapply(core_pkgs, library, character.only = TRUE))
    if (requireNamespace("curl", quietly = TRUE)) library(curl)
    if (requireNamespace("sf", quietly = TRUE)) library(sf)
  })
}

download_with_fallback <- function(primary_url, fallback_url, destfile, label) {
  for (url in c(primary_url, fallback_url)) {
    message(sprintf("Download %s da: %s", label, url))
    ok <- tryCatch({
      if (requireNamespace("curl", quietly = TRUE)) {
        curl::curl_download(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
      } else {
        utils::download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)
      }
      file.exists(destfile) && isTRUE(file.info(destfile)$size > 0)
    }, error = function(e) FALSE)
    if (ok) {
      message(sprintf("OK %s salvato in: %s", label, destfile))
      return(invisible(TRUE))
    }
  }
  stop(sprintf("Impossibile scaricare %s (URL primario e fallback).", label), call. = FALSE)
}

normalize_names <- function(x) stringr::str_to_lower(gsub("[^a-zA-Z0-9]", "", x))

pick_first_existing <- function(normalized_named, candidates) {
  idx <- match(candidates, normalized_named)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) return(NA_character_)
  names(normalized_named)[idx[1]]
}

detect_coord_columns <- function(df) {
  original_names <- names(df)
  normalized <- normalize_names(original_names)
  names(normalized) <- original_names
  lon_candidates <- c("lon", "lng", "long", "longitude", "x", "coordlon", "centerlon")
  lat_candidates <- c("lat", "latitude", "y", "coordlat", "centerlat")
  list(
    lon = pick_first_existing(normalized, lon_candidates),
    lat = pick_first_existing(normalized, lat_candidates)
  )
}

as_numeric_safe <- function(x) suppressWarnings(as.numeric(as.character(x)))

make_bbox_polygon <- function(xmin, xmax, ymin, ymax, crs = 4326) {
  sf::st_as_sfc(sf::st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), crs = sf::st_crs(crs)))
}

simulate_points_in_bbox <- function(n, bbox, clusters = 5) {
  # Simula punti con cluster nel bbox (per dataset senza coordinate reali)
  centers <- data.frame(
    lon = runif(clusters, bbox$xmin + 0.5, bbox$xmax - 0.5),
    lat = runif(clusters, bbox$ymin + 0.5, bbox$ymax - 0.5)
  )
  which_center <- sample(seq_len(clusters), n, replace = TRUE)
  lon <- rnorm(n, mean = centers$lon[which_center], sd = (bbox$xmax - bbox$xmin) / 25)
  lat <- rnorm(n, mean = centers$lat[which_center], sd = (bbox$ymax - bbox$ymin) / 25)
  lon <- pmin(pmax(lon, bbox$xmin), bbox$xmax)
  lat <- pmin(pmax(lat, bbox$ymin), bbox$ymax)
  data.frame(lon = lon, lat = lat)
}

main <- function() {
  script_dir <- detect_script_dir()
  bootstrap_packages(script_dir)

  data_dir <- file.path(script_dir, "data")
  output_dir <- file.path(script_dir, "output")
  dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  geojson_url <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes.geojson"
  csv_url <- "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_on_french_states.csv"

  geojson_file <- file.path(data_dir, "france_communes.geojson")
  csv_file <- file.path(data_dir, "ristoranti.csv")

  # BBox indicativa Sud Francia (in WGS84)
  south_bbox <- list(xmin = -1.8, xmax = 9.8, ymin = 41.1, ymax = 45.2)

  # Se sf non c'è, il GeoJSON non è necessario per produrre la mappa (fallback su maps).
  if (requireNamespace("sf", quietly = TRUE)) {
    download_with_fallback(geojson_url, geojson_url, geojson_file, "GeoJSON confini comuni")
  }
  download_with_fallback(csv_url, csv_url, csv_file, "CSV ristoranti (fallback)")

  restaurants_raw <- readr::read_csv(csv_file, show_col_types = FALSE)
  coords <- detect_coord_columns(restaurants_raw)

  if (!is.na(coords$lon) && !is.na(coords$lat)) {
    restaurants <- restaurants_raw %>%
      dplyr::mutate(
        lon = as_numeric_safe(.data[[coords$lon]]),
        lat = as_numeric_safe(.data[[coords$lat]])
      ) %>%
      dplyr::filter(!is.na(lon), !is.na(lat)) %>%
      dplyr::filter(dplyr::between(lat, -90, 90), dplyr::between(lon, -180, 180)) %>%
      dplyr::distinct(lon, lat, .keep_all = TRUE)
  } else {
    message("CSV senza coordinate riconoscibili: simulo punti nel Sud della Francia.")
    n <- max(1500L, min(8000L, nrow(restaurants_raw) * 40L))
    restaurants <- cbind(restaurants_raw[rep(1, n), , drop = FALSE], simulate_points_in_bbox(n, south_bbox))
  }

  if (requireNamespace("sf", quietly = TRUE)) {
    restaurants_sf <- sf::st_as_sf(restaurants, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

    communes <- sf::st_read(geojson_file, quiet = TRUE) %>%
      sf::st_make_valid() %>%
      sf::st_transform(4326)

    south_poly <- make_bbox_polygon(
      xmin = south_bbox$xmin, xmax = south_bbox$xmax,
      ymin = south_bbox$ymin, ymax = south_bbox$ymax
    )

    communes_south <- communes[sf::st_intersects(communes, south_poly, sparse = FALSE), ]
    if (nrow(communes_south) == 0) stop("Nessun comune trovato nel bounding box del Sud Francia.", call. = FALSE)

    restaurants_south <- sf::st_join(restaurants_sf, communes_south, join = sf::st_within, left = FALSE)
    if (nrow(restaurants_south) == 0) stop("Nessun punto cade nei confini del Sud Francia.", call. = FALSE)

    bbox_obj <- sf::st_bbox(communes_south)

    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = communes_south, fill = "grey96", color = "grey80", linewidth = 0.08) +
      ggplot2::stat_density_2d_filled(
        data = sf::st_drop_geometry(restaurants_south),
        ggplot2::aes(x = lon, y = lat, fill = after_stat(level)),
        alpha = 0.70,
        contour_var = "ndensity"
      ) +
      ggplot2::geom_point(
        data = sf::st_drop_geometry(restaurants_south),
        ggplot2::aes(x = lon, y = lat),
        size = 0.30,
        alpha = 0.20,
        color = "#111111"
      ) +
      ggplot2::coord_sf(
        xlim = c(bbox_obj["xmin"], bbox_obj["xmax"]),
        ylim = c(bbox_obj["ymin"], bbox_obj["ymax"]),
        expand = FALSE
      ) +
      viridis::scale_fill_viridis(discrete = TRUE, option = "C", direction = 1, name = "Densità") +
      ggplot2::labs(
        title = "Mappa di densità dei ristoranti - Sud della Francia",
        subtitle = "Heatmap (densità) + punti; coordinate simulate se assenti nel CSV",
        caption = "Fonti: GeoJSON comuni (gregoiredavid/france-geojson) + CSV fallback (R Graph Gallery)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.2),
        legend.position = "right"
      )
  } else {
    message("Pacchetto 'sf' non disponibile: uso fallback con 'maps' (mappa semplificata).")
    france <- ggplot2::map_data("france")
    france_south <- france %>%
      dplyr::filter(dplyr::between(long, south_bbox$xmin - 1, south_bbox$xmax + 1)) %>%
      dplyr::filter(dplyr::between(lat, south_bbox$ymin - 1, south_bbox$ymax + 1))

    restaurants_south <- restaurants %>%
      dplyr::filter(dplyr::between(lon, south_bbox$xmin, south_bbox$xmax)) %>%
      dplyr::filter(dplyr::between(lat, south_bbox$ymin, south_bbox$ymax))

    if (nrow(restaurants_south) == 0) stop("Nessun punto nel bbox del Sud Francia.", call. = FALSE)

    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(
        data = france_south,
        ggplot2::aes(x = long, y = lat, group = group),
        fill = "grey96",
        color = "grey80",
        linewidth = 0.2
      ) +
      ggplot2::stat_density_2d_filled(
        data = restaurants_south,
        ggplot2::aes(x = lon, y = lat, fill = after_stat(level)),
        alpha = 0.70,
        contour_var = "ndensity"
      ) +
      ggplot2::geom_point(
        data = restaurants_south,
        ggplot2::aes(x = lon, y = lat),
        size = 0.30,
        alpha = 0.20,
        color = "#111111"
      ) +
      ggplot2::coord_equal(
        xlim = c(south_bbox$xmin, south_bbox$xmax),
        ylim = c(south_bbox$ymin, south_bbox$ymax),
        expand = FALSE
      ) +
      viridis::scale_fill_viridis(discrete = TRUE, option = "C", direction = 1, name = "Densità") +
      ggplot2::labs(
        title = "Mappa di densità dei ristoranti - Sud della Francia",
        subtitle = "Fallback senza 'sf': heatmap su bbox (confini semplificati)",
        caption = "Fonti: maps + CSV fallback (R Graph Gallery)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "grey90", linewidth = 0.2),
        legend.position = "right"
      )
  }

  png_path <- file.path(output_dir, "mappa_densita_ristoranti_sud_francia.png")
  ggplot2::ggsave(filename = png_path, plot = p, width = 1600, height = 1100, dpi = 220, units = "px", bg = "white")

  csv_out <- file.path(output_dir, "ristoranti_sud_francia_filtrati.csv")
  if (inherits(restaurants_south, "sf")) {
    readr::write_csv(sf::st_drop_geometry(restaurants_south), csv_out)
    geojson_out <- file.path(output_dir, "confini_sud_francia.geojson")
    sf::st_write(communes_south, geojson_out, delete_dsn = TRUE, quiet = TRUE)
  } else {
    readr::write_csv(restaurants_south, csv_out)
    geojson_out <- NA_character_
  }

  message("Completato.")
  message(sprintf("PNG: %s", png_path))
  message(sprintf("CSV: %s", csv_out))
  if (!is.na(geojson_out)) message(sprintf("GeoJSON: %s", geojson_out))
  message(sprintf("Punti nel Sud Francia: %d", nrow(restaurants_south)))
}

main()

