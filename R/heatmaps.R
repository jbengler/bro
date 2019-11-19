#' @export
bro_heatmap_data <- function(df, rows, columns, values, ann_row = NA, ann_col = NA, gaps_row = NULL, gaps_col = NULL){
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

  if(!is.null(gaps_row)) {
    # implement here
  }

  if(!is.null(gaps_col)) {
    # implement here
  }

  list(m = m, ann_row = ann_row, ann_col = ann_col, gaps_row = gaps_row, gaps_col = gaps_col)
}

#' @export
bro_plot_heatmap <- function(df, rows, columns, values, ann_row = NA, ann_col = NA,
                             gaps_row = NULL, gaps_col = NULL, ann_colors = NA, breaks = NA,
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
                     gaps_row = heatmap_data$gaps_row,
                     gaps_col = heatmap_data$gaps_col,
                     annotation_colors = ann_colors,
                     scale = scale,
                     fontsize = fontsize,
                     cellwidth = cellwidth,
                     cellheight = cellheight,
                     color = color,
                     breaks = breaks,
                     ...)
}
