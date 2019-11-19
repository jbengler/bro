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

########### bro heatmap function

# library(tidyverse)
# library(bro)
#
# my_genes <- c("Tex26", "Syngr4", "Plekhb1", "Tia1", "Nhlh1", "Mxd3")
#
# ann_colors <- list(
#   timepoint = c("9w" = "#1B9E77", "14w" = "#D95F02"),
#   genotype = c(WT = "#7570B3", TDP43.WT = "#E7298A", TDP43.MUT = "#66A61E"),
#   sample_type = c(SC = "#C0C0C0", TRAP = "#000000"),
#   candidate = c(up = "#00CCFC", down = "#CF0CF0")
# )
#
# my_df <-
#   df %>%
#   filter(external_gene_name %in% my_genes) %>%
#   mutate(external_gene_name = fct_relevel(external_gene_name, my_genes)) %>%
#   arrange(external_gene_name, sample_type, desc(timepoint), desc(genotype)) %>%
#   mutate(candidate = if_else(external_gene_name %in% c("Tex26", "Syngr4", "Mxd3"), "up", "down"))
#
# bro_plot_heatmap(my_df,
#                  rows = external_gene_name,
#                  columns = sample,
#                  values = expression,
#                  ann_col = c("genotype", "timepoint", "sample_type"),
#                  ann_row = c("candidate"),
#                  ann_colors = ann_colors,
#                  color_scale_min = -2,
#                  color_scale_max = 2
# )

