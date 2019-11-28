
#' Determine maximum plot dimensions
#'
#' @param gg ggplot or list of ggplots
#'
#' @return returns a named vector with the maximum overall width and height found in the input.
#' @export
bro_get_ggsize <- function(gg) {
  # BUG!
  # Facet wraps with fixed dimensions are not recognized as has_fixed_dimensions

  if (class(gg)[1] %in% c("patchwork", "gg", "ggplot")) gg <- list(gg)

  dimensions <-
    map(gg, function(x) {
    if (!is.ggplot(x)) stop("Please provide a ggplot or list of ggplots as input to 'gg'")
    gtab <- patchwork:::plot_table(x, 'auto')

    has_fixed_dimensions <-
      all(as.character(gtab$widths) != "1null") &
      all(as.character(gtab$heights) != "1null")

    if (has_fixed_dimensions) {
      width <- grid::convertWidth(sum(gtab$widths) + unit(1, "mm"), unitTo = "in", valueOnly = TRUE)
      height <- grid::convertHeight(sum(gtab$heights) + unit(1, "mm"), unitTo = "in", valueOnly = TRUE)
      tibble(width = width, height = height)
    } else {
      tibble(width = NA, height = NA)
    }
  }) %>%
    bind_rows()

  if (all(is.na(dimensions$width)) | all(is.na(dimensions$height))) {
    c(width = NA,
      height = NA)
  } else {
    c(width = max(dimensions$width, na.rm = TRUE),
      height = max(dimensions$height, na.rm = TRUE))
  }
}


#' Universal ggplot export function
#'
#' This function takes a ggplot (for single page) or list of ggplots (for multi page) and writes them to a pdf file.
#' In case the input has absolute dimensions (e.g. as a result of egg:set_panel_size or
#' patchwork::plot_layout), width and height of the pdf are overridden to fit the content.
#'
#' @param gg ggplot or list of ggplots
#'
#' @param filename filename for export
#' @param width width of the pdf (default 7 inch). When the plot has absolute dimensions this parameter gets overridden to fit the content.
#' @param height height of the pdf (default 7 inch). When the plot has absolute dimensions this parameter gets overridden to fit the content.
#' @param ... gets passed to pdf()
#'
#' @export
bro_ggsave <- function(gg = last_plot(), filename, device = NULL, path = NULL, scale = 1,
                       width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE, ...) {
  # this function is adapted from ggplot2::ggsave()
  if (class(gg)[1] %in% c("patchwork", "gg", "ggplot")) gg <- list(gg)

  dimensions <- bro_get_ggsize(gg)
  if(all(!is.na(dimensions))) {
    width <- dimensions[["width"]]
    height <- dimensions[["height"]]
  }

  dpi <- ggplot2:::parse_dpi(dpi)
  dev <- ggplot2:::plot_dev(device, filename, dpi = dpi)
  dim <- ggplot2:::plot_dim(c(width, height), scale = scale, units = units,
                            limitsize = limitsize)
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  old_dev <- grDevices::dev.cur()
  dev(filename = filename, width = dim[1], height = dim[2],
      ...)
  on.exit(utils::capture.output({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev)
  }))
  map(gg, ~grid::grid.draw(.x))
  invisible()
  if (!is.na(width) & !is.na(height)) message("Saving ", round(width, 2), " x ", round(height, 2), " in image (adjusted to absolute plot dimensions)")
}

#' @export
bro_wrap_plots_paged <- function(plotlist, ncol = 1, nrow = 1, width = NULL, height = NULL, ...) {
  if (!is.numeric(nrow) | !is.numeric(ncol))
    stop("Values for 'nrow' and 'ncol' need to be integer")
  plots_per_page <- nrow * ncol
  pages <-
    split(plotlist, ceiling(seq_along(plotlist)/plots_per_page)) %>%
    map(., ~patchwork::wrap_plots(.x, ncol = ncol, nrow = nrow, widths = width, heights = height, guides = "collect", ...))
  unname(pages)
}

#' @export
bro_facet_wrap_paged <- function(gg, facet_var, ncol = 1, nrow = 1, width = NULL, height = NULL, ...) {
  if (!is.numeric(nrow) | !is.numeric(ncol))
    stop("Values for 'nrow' and 'ncol' need to be integer")
  df <-
    gg$data %>%
    nest(data = -{{facet_var}}) %>%
    arrange({{facet_var}})
  plots <-
    map2(df$data, df %>% pull({{facet_var}}),
         function(data, facet_title) {
           gg %+% data + ggtitle(facet_title)
         })
  bro_wrap_plots_paged(plots, ncol = ncol, nrow = nrow, width = width, height = height, ...)
}

# This function can be used with genuine ggplot facet wraps
# However, better use 'bro_facet_wrap_paged()' as this works with 'bro_ggsave()'
#' @export
bro_set_panel_size <- function (gg, width = unit(40, "mm"), height = unit(40, "mm")) {
  if (class(gg)[1] %in% c("patchwork", "gg", "ggplot")) gg <- list(gg)

  gg <-
    map(gg, function(x){
      g <- ggplot2::ggplotGrob(x)
      panels <- grep("panel", g$layout$name)
      panel_index_w <- unique(g$layout$l[panels])
      panel_index_h <- unique(g$layout$t[panels])
      nw <- length(panel_index_w)
      nh <- length(panel_index_h)
      g$widths[panel_index_w] <- rep(width, nw)
      g$heights[panel_index_h] <- rep(height, nh)
      ggpubr::as_ggplot(g)
    })

  if(length(gg) == 1) gg <- gg[[1]]
  gg
}

