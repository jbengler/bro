
#' @title Determine duplicate elements
#' @description This function determines which elements of a vector or data frame exists
#' more than once, and returns a logical vector indicating those elements (rows) as duplicates.
#' In contrast to the base R function `duplicated()` it marks all occurences of the duplicated element,
#' including the first occurence.
#' @param x A vector of elements
#' @export
bro_duplicated <- function(x){
  duplicated(x) | duplicated(x, fromLast = TRUE)
}

#' @export
bro_order <- function(...) {
  order(order(...))
}

#' @title Filter top n rows by value
#' @description This function sorts a data frame by one or more columns and returns the top n rows.
#' In contrast to `dplyr::top_n`, this function alows to sort by multiple columns and returns
#' exactly n rows even if ties are present in the columns used for sorting.
#' @param df data frame to filter
#' @param n number of rows to return
#' @param ... names of columns to sort by
#' @export
bro_top_n <- function(df, n, ...) {
  df %>%
    arrange(...) %>%
    filter(row_number() %in% 1:n)
}

bro_check_var <- function(df, var) {
  count(df, {{var}}) %>% count(n)
}

#' @export
bro_str_capture <- function(string, pattern) {
  m <- stringr::str_match(string, pattern)
  if (ncol(m) > 1) {
    return(m[,2])
  } else {
    return(m)
  }
}

#' @export
bro_breaks <- function(low, mid, high, n) {
  c(seq(low, mid, length.out = ceiling(n/2)),
    seq(mid, high, length.out = ceiling(n/2))[-1]
  )
}

#' @export
bro_style_decimals <- function(value, n = 1) {
  sprintf(paste0("%0.",n,"f"), round(value, digits = n))
}

#' @export
bro_overlap_lists <- function(ll) {
  ll %>%
    enframe(name = "list_name", value = "list_entry") %>%
    unnest(cols = c(list_entry)) %>%
    group_by(list_entry) %>%
    summarise(
      is_in_list = paste(list_name, collapse = "+"),
      is_in_n_lists = length(list_name)
    )
}

#' @export
bro_pca <- function(object, ntop=500, scale){
  rv <- apply(object, 1, var)
  select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
  pca <- prcomp(t(object[select, ]), scale = scale)
  percentVar <- pca$sdev^2/sum(pca$sdev^2)

  d <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], name = colnames(object))
  attr(d, "percentVar") <- percentVar[1:2]
  return(d)
}

