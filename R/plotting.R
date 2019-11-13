
#' @export
bro_gg_barscatter <- function(df, x, y, title = NULL, bar_color = "#EA883D") {
  df %>%
    ggplot(aes({{x}}, {{y}})) +
    stat_summary(fun.y = mean, geom = "bar", width = 0.4, fill = bar_color) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
    geom_point(position = position_jitter(width = 0.1, height = 0)) +
    ggtitle(title)
}

#' @export
bro_gg_lollipop <- function(df, x, y, point_color = "#EA883D") {
  df %>%
    ggplot(aes({{x}}, {{y}})) +
    geom_segment(aes(x = {{x}}, y = 0, xend = {{x}}, yend = {{y}}), color = "grey50") +
    geom_point(color = point_color)
}

#' @export
bro_heatmap_data <- function(df, rows, columns, values, ann_row = NA, ann_col = NA){
  m <-
    df %>%
    select({{rows}}, {{columns}}, {{values}}) %>%
    pivot_wider(names_from = {{columns}}, values_from = {{values}}) %>%
    column_to_rownames(colnames(.)[1])

  if(!any(is.na(ann_col))) {
    ann_col <-
      df %>%
      select({{columns}}, !!ann_col) %>%
      distinct() %>%
      column_to_rownames(colnames(.)[1])
  }

  if(!any(is.na(ann_row))) {
    ann_row <-
      df %>%
      select({{rows}}, !!ann_row) %>%
      distinct() %>%
      column_to_rownames(colnames(.)[1])
  }
  list(m = m, ann_row = ann_row, ann_col = ann_col)
}

#' @export
bro_plot_heatmap <- function(df, rows, columns, values, ann_row = NA, ann_col = NA, ann_colors = NA, breaks = NA,
                             scale = "row", fontsize = 7, cellwidth = 7, cellheight = 7,
                             color_scale_min = NA, color_scale_max = NA, color_scale_n = 16,
                             color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name ="RdYlBu")))(color_scale_n), ...) {

  heatmap_data <-
    bro_heatmap_data(df, {{rows}}, {{columns}}, {{values}}, ann_row = ann_row, ann_col = ann_col)

  if(is.numeric(color_scale_min) & is.numeric(color_scale_max) & is.numeric(color_scale_n)) {
    breaks <- seq(color_scale_min, color_scale_max, length.out = color_scale_n)
  }

  pheatmap::pheatmap(heatmap_data$m,
                     cluster_rows = FALSE,
                     cluster_cols = FALSE,
                     border_color = NA,
                     annotation_row = heatmap_data$ann_row,
                     annotation_col = heatmap_data$ann_col,
                     annotation_colors = ann_colors,
                     scale = scale,
                     fontsize = fontsize,
                     cellwidth = cellwidth,
                     cellheight = cellheight,
                     color = color,
                     breaks = breaks,
                     ...)
}

#' @export
bro_gg_export <- function(gg, file = "test.pdf", facet_var = NULL, nrow = NULL, ncol = NULL, width = 30, height = 30, scales = "free", ...) {
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
        {gg + facet_wrap(vars({{facet_var}}), nrow, ncol, scales = scales, ...)} %>%
          egg::set_panel_size(p = ., width = unit(width, "mm"), height = unit(height, "mm"))
      )
    }
  } else {
    # no facet wrap
    gtabs <- list(
      egg::set_panel_size(p = gg, width = unit(width, "mm"), height = unit(height, "mm"))
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
