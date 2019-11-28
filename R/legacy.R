multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

set_panel_size <- function(p=NULL, g=ggplotGrob(p), file=NULL,
                           margin = unit(1,"mm"),
                           width=unit(4, "cm"),
                           height=unit(4, "cm")){

  panels <- grep("panel", g$layout$name)
  panel_index_w<- unique(g$layout$l[panels])
  panel_index_h<- unique(g$layout$t[panels])
  nw <- length(panel_index_w)
  nh <- length(panel_index_h)

  if(getRversion() < "3.3.0"){

    # the following conversion is necessary
    # because there is no `[<-`.unit method
    # so promoting to unit.list allows standard list indexing
    g$widths <- grid:::unit.list(g$widths)
    g$heights <- grid:::unit.list(g$heights)

    g$widths[panel_index_w] <-  rep(list(width),  nw)
    g$heights[panel_index_h] <- rep(list(height), nh)

  } else {

    g$widths[panel_index_w] <-  rep(width,  nw)
    g$heights[panel_index_h] <- rep(height, nh)

  }

  if(!is.null(file))
    ggsave(file, g,
           width = convertWidth(sum(g$widths) + margin,
                                unitTo = "in", valueOnly = TRUE),
           height = convertHeight(sum(g$heights) + margin,
                                  unitTo = "in", valueOnly = TRUE))

  g
}


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

