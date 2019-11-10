
#' @export
bro_style_white_bg <- function() {
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
    plot.background = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    strip.background = element_rect(fill = NA, colour = NA),
    panel.background = element_rect(fill = NA, colour = NA),
    panel.border = element_rect(fill = NA, colour = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_line(colour = "black", size = 0.25)
  )
}

#' @export
bro_style_font_size <- function(fs = 7) {
  theme(
    plot.title = element_text(size = fs, colour = "black", hjust = 0.5, vjust = 0.5),
    plot.subtitle = element_text(size = fs, colour = "black", hjust = 0.5, vjust = 0.5),
    text = element_text(size = fs, colour = "black"),
    axis.text = element_text(size = fs, colour = "black"),
    axis.title = element_text(size = fs, colour = "black"),
    legend.title = element_text(size = fs, colour = "black"),
    legend.text = element_text(size = fs, colour = "black"),
    strip.text = element_text(size = fs, colour = "black")
  )
}

#' @export
bro_style_just_xy <- function() {
  bro_style_white_bg() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(size = 0.25, colour = "black"),
    axis.ticks = element_line(size = 0.25, colour = "black")
  )
}

#' @export
bro_style_no_axis <- function() {
  theme(
    panel.border = element_blank()
  )
}

#' @export
bro_style_no_legend <- function() {
  theme(
    legend.position="none"
  )
}

#' @export
bro_style_centered_title <- function() {
  theme(
    plot.title = element_text(hjust = 0.5)
  )
}

#' @export
bro_style_rotate_labels <- function(angle = 45) {
  theme(
    axis.text.x = element_text(angle = angle, hjust = 1)
  )
}

#' @export
bro_style_minimal <- function() {
  theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm"),
    plot.background = element_rect(fill = NA, colour = NA),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key = element_rect(fill = NA, colour = NA),
    panel.background = element_rect(fill = NA, colour = NA),
    panel.border = element_rect(fill = NA, colour = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_line(colour = "grey", size = 0.25)
  )
}

#' @export
bro_theme_nature <- function() {
    bro_style_no_legend() +
    bro_style_just_xy() +
    bro_style_font_size()
}


