
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
bro_pals_show <- function(pals = bro_pals) {
  pals <-
    tibble::enframe(pals, name = "pal") %>%
    tidyr::unnest() %>%
    tibble::rownames_to_column("nr") %>%
    dplyr::mutate(nr = as.double(nr),
                  nr_value = paste0(nr,value)
    )

  ggplot(pals, aes(x = nr, y = 1, fill = haven::as_factor(nr_value))) +
    geom_tile() +
    #geom_text(data = . %>% filter((nr %% 2) == 0), aes(label = value), angle = 90) +
    scale_fill_manual(values = pals$value, guide = "none") +
    theme_void() +
    theme(aspect.ratio=0.2) +
    facet_wrap(~pal, scales = "free_x", ncol = 2)
}

#' @export
bro_modify_HLS <- function(x, H = 1, L = 1, S = 1) {
  hls_cols <- as(colorspace::hex2RGB(x), "HLS")
  hls_cols@coords[,1] <- hls_cols@coords[,1] * H
  hls_cols@coords[,2] <- hls_cols@coords[,2] * L
  hls_cols@coords[,3] <- hls_cols@coords[,3] * S
  return(colorspace::hex(as(hls_cols, "sRGB")))
}

