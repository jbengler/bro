
# do not @export
get_colors <- function(x) {

  files <- list.files(path = "~/GoogleDrive/lab/R/color_schemes/", pattern = ".txt")
  file.names <- gsub(".txt", "", files)

  bro_pals <- list()
  for (i in 1:length(files)) {

    lines <- readLines(paste0("~/GoogleDrive/lab/R/color_schemes/",files[i]))

    library(stringr)
    cols <- str_match(lines, "#\\w{6}")[, 1]
    names(cols) <- str_match(lines, "\\$(.*):")[, 2]

    bro_pals[[i]] <- toupper(cols)
    names(bro_pals)[i] <- file.names[i]
  }
  return(bro_pals)
}

# bro_pals <- get_colors()
# bro_colors <- unlist(unname(bro_pals))
# usethis::use_data(bro_pals, bro_colors, overwrite = TRUE)

# construction of ggplot2 palettes inspired by this blog post:
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

# do not @export
bro_pal <- function(palette = "metro_ui", reverse = FALSE, ...) {
  pal <- bro_pals[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

#' @export
bro_scale_color_c <- function(palette = "blue_pink_yellow", reverse = FALSE, ...) {
  pal <- bro_pal(palette = palette, reverse = reverse)
  scale_color_gradientn(colours = pal(256), ...)
}

#' @export
bro_scale_color_d <- function(palette = "metro_ui", reverse = FALSE, ...) {
  pal <- bro_pal(palette = palette, reverse = reverse)
  discrete_scale("colour", paste0("bro_", palette), palette = pal, ...)
}

#' @export
bro_scale_fill_c <- function(palette = "blue_pink_yellow", reverse = FALSE, ...) {
  pal <- bro_pal(palette = palette, reverse = reverse)
  scale_fill_gradientn(colours = pal(256), ...)
}

#' @export
bro_scale_fill_d <- function(palette = "metro_ui", reverse = FALSE, ...) {
  pal <- bro_pal(palette = palette, reverse = reverse)
  discrete_scale("fill", paste0("bro_", palette), palette = pal, ...)
}

#' @export
bro_pals_show <- function(pals = bro_pals, show_labels = FALSE, ncol = NULL, nrow = NULL, width = NULL, height = NULL, ...) {
  tidy_pals <-
    tibble::enframe(pals, name = "pal") %>%
    tidyr::unnest(cols = c(value)) %>%
    tibble::rownames_to_column("nr") %>%
    mutate(
      nr = factor(nr, levels = row_number()),
      label_color = if_else(as(colorspace::hex2RGB(value), "HLS")@coords[,2] > 0.6, "#000000", "#FFFFFF"))

  fill_colors <- deframe(tidy_pals[c(3,3)])
  label_colors <- deframe(tidy_pals[c(4,4)])

  gg <-
    ggplot(tidy_pals, aes(x = nr, y = 1, fill = value)) +
    geom_tile() +
    { if (show_labels) geom_text(aes(label = value, color = label_color), angle = 90, size = 2.5) else NULL
    } +
    scale_x_discrete(expand = c(0, 0)) +
    scale_fill_manual(values = fill_colors, guide = FALSE) +
    scale_color_manual(values = label_colors, guide = FALSE) +
    bro_style_white_bg() +
    bro_style_no_axis() +
    bro_style_font_size() +
    theme(
      axis.ticks = element_blank(), axis.title = element_blank(), axis.text = element_blank())

  gg %>% bro_facet_wrap_paged(facet_var = pal, ncol = ncol, nrow = nrow, width = width, height = height, ...)
}

#' @export
bro_modify_HLS <- function(x, H = 1, L = 1, S = 1) {
  hls_cols <- as(colorspace::hex2RGB(x), "HLS")
  hls_cols@coords[,1] <- hls_cols@coords[,1] * H
  hls_cols@coords[,2] <- hls_cols@coords[,2] * L
  hls_cols@coords[,3] <- hls_cols@coords[,3] * S
  return(colorspace::hex(as(hls_cols, "sRGB")))
}

