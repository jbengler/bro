
bro_duplicated <- function(x){
  duplicated(x) | duplicated(x, fromLast = TRUE)
}

bro_top_n <- function(df, n, ...) {
  df %>%
    arrange(...) %>%
    filter(row_number() %in% 1:n)
}

bro_str_capture <- function(string, pattern) {
  m <- stringr::str_match(string, pattern)
  if (ncol(m) > 1) {
    return(m[,2])
  } else {
    return(m)
  }
}

bro_breaks <- function(low, mid, high, n) {
  c(seq(low, mid, length.out = ceiling(n/2)),
    seq(mid, high, length.out = ceiling(n/2))[-1]
  )
}

bro_style_decimals <- function(value, n = 1) {
  sprintf(paste0("%0.",n,"f"), round(value, digits = n))
}

bro_pca <- function(object, ntop=500, scale){
  rv <- apply(object, 1, var)
  select <- order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
  pca <- prcomp(t(object[select, ]), scale = scale)
  percentVar <- pca$sdev^2/sum(pca$sdev^2)

  d <- data.frame(PC1 = pca$x[, 1], PC2 = pca$x[, 2], name = colnames(object))
  attr(d, "percentVar") <- percentVar[1:2]
  return(d)
}
