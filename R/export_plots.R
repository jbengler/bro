bro_get_ggsize <- function(plot) {
  gtab <- patchwork:::plot_table(plot, 'auto')

  has_fixed_dimensions <-
    !gtab$widths %>% map(~is.null(attr(.x, "unit"))) %>% unlist() %>% any() |
    !gtab$heights %>% map(~is.null(attr(.x, "unit"))) %>% unlist() %>% any()

  if (has_fixed_dimensions) {
    width <- grid::convertWidth(sum(gtab$widths) + unit(1, "mm"), unitTo = "mm", valueOnly = TRUE)
    height <- grid::convertHeight(sum(gtab$heights) + unit(1, "mm"), unitTo = "mm", valueOnly = TRUE)
    c(width = width, height = height)
  } else {
    c(width = NA, height = NA)
  }
}

#' @export
bro_ggsave <- function(plot = last_plot(), filename, width = NA, height = NA, units = c("in", "cm", "mm"), ...) {
  width <- bro_get_ggsize(plot)[["width"]]
  height <- bro_get_ggsize(plot)[["height"]]
  if(!is.na(width)) message("Saving plot with fixed dimensions: ", round(width, 1), " x ", round(height, 1), " mm")
  ggsave(filename = filename, plot = plot, width = width, height = height, units = "mm", ...)
}

#' @export
bro_gg_export <- function(gg, file = "test.pdf", facet_var = NULL, nrow = NULL, ncol = NULL, width = 30, height = 30, style = NULL, scales = "free", ...) {
  # check input
  if(is.ggplot(gg)) {
    input_type <- "ggplot"
  } else if (is.list(gg) & is.ggplot(gg[[1]])) {
    input_type <- "plotlist"
  } else {
    stop("gg needs to be a ggplot object or list of ggplot objects!")
  }

  if(input_type == "ggplot") {
    if (!missing(facet_var)) {
      if (is.numeric(ncol) & is.numeric(nrow)) {
        # fixed grid facet wrap
        # returns list of multiple pages if plots do not fit on one page
        plots_per_page <- ncol*nrow
        n_plots <- gg$data %>%
          distinct({{facet_var}}) %>%
          nrow(.)
        pages <- ceiling(n_plots / plots_per_page)

        gtabs <- map(
          1:pages,
          ~{gg +
              style +
              ggforce::facet_wrap_paginate(
                vars({{facet_var}}),
                ncol = ncol,
                nrow = nrow,
                scales = scales,
                page = .x)} %>%
            egg::set_panel_size(p = ., width = unit(width, "mm"), height = unit(height, "mm"))
        )
      } else {
        # classical facet wrap
        gtabs <- list(
          {gg + style + facet_wrap(vars({{facet_var}}), nrow, ncol, scales = scales, ...)} %>%
            egg::set_panel_size(p = ., width = unit(width, "mm"), height = unit(height, "mm"))
        )
      }
    } else {
      # no facet wrap
      gtabs <- list(
        egg::set_panel_size(p = gg + style, width = unit(width, "mm"), height = unit(height, "mm"))
      )
    }

    # export plot
    page_dimensions <-
      map(gtabs,
          ~tibble(
            page_width = grid::convertWidth(sum(.x$widths) + unit(1, "mm"), unitTo = "in", valueOnly = TRUE),
            page_height = grid::convertHeight(sum(.x$heights) + unit(1, "mm"), unitTo = "in", valueOnly = TRUE)
          )) %>%
      bind_rows()

    ggpubr::ggexport(map(gtabs, ~ggpubr::as_ggplot(.)),
                     filename = file,
                     width = max(page_dimensions$page_width),
                     height = max(page_dimensions$page_height))
  }

  if(input_type == "plotlist") {
    gtabs <- map(gg, ~egg::set_panel_size(p = .x + style, width = unit(width, "mm"), height = unit(height, "mm")))

    # export plot
    page_dimensions <-
      map(gtabs,
          ~tibble(
            page_width = grid::convertWidth(sum(.x$widths) + unit(1, "mm"), unitTo = "in", valueOnly = TRUE),
            page_height = grid::convertHeight(sum(.x$heights) + unit(1, "mm"), unitTo = "in", valueOnly = TRUE)
          )) %>%
      bind_rows()

    n_plots <- length(gtabs)
    ncol <- ceiling(sqrt(n_plots))
    nrow <- ceiling(n_plots/ncol)

    ggpubr::ggexport(plotlist = map(gtabs, ~ggpubr::as_ggplot(.)),
                     filename = file,
                     nrow = nrow,
                     ncol = ncol,
                     width = max(page_dimensions$page_width)*ncol,
                     height = max(page_dimensions$page_height)*nrow)
  }
}

# library(ggplot2)
# library(tidyverse)
# gg <- ggplot(mtcars, aes(cyl, mpg)) +
#   geom_point()
# gg2 <- ggplot(mtcars, aes(cyl, mpg)) +
#   geom_col()
# ll <- list(gg, gg2)
# bro_gg_export(gg)
# bro_gg_export(gg2)
# bro_gg_export(ll)
# bro_gg_export(gg, facet_var = gear)
#
# bro_gg_export(gg, style = bro::bro_theme_nature())
# bro_gg_export(gg2, style = bro::bro_theme_nature())
# bro_gg_export(ll, style = bro::bro_theme_nature())
# bro_gg_export(gg, facet_var = gear, style = bro::bro_theme_nature())

