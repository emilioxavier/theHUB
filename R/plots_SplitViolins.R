
#' @title Create Quantile Segments `data.frame` for Split Violin Plots
#'
#' @description Constructs the [ggplot2] `GeomSplitViolin` function.
#'
#'   Based on the code provided on [stackoverflow](https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-with-quantiles)
#'   [https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-with-quantiles](https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-with-quantiles)
#'   originally posted on 04/Oct/2018. Last accessed 12/Jan/2024.
#'
#' @return [ggplot2] geom function
#'
#' @importFrom base all cbind rep setdiff transform
#' @importFrom ggplot2 GeomViolin ggname
#' @importFrom grid grobTree
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
GeomSplitViolin <- ggplot2:::ggproto("GeomSplitViolin", ggplot2::GeomViolin,
                                     draw_group = function(self, data, ..., draw_quantiles = NULL) {
                                       # Original function by Jan Gleixner (@jan-glx)
                                       # Adjustments by Wouter van der Bijl (@Axeman)
                                       data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                                       grp <- data[1, "group"]
                                       # newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                                       newdata <- dplyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                                       newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                                       newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])

                                       if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                                         stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
                                         quantiles <- create_quantile_segment_frame(data, draw_quantiles, split = TRUE, grp = grp)
                                         aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                                         aesthetics$alpha <- rep(1, nrow(quantiles))
                                         both <- cbind(quantiles, aesthetics)
                                         quantile_grob <- GeomPath$draw_panel(both, ...)
                                         ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                                       }
                                       else {
                                         ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                                       }
                                     }
)


#' @title Create Quantile Segments `data.frame` for Split Violin Plots
#'
#' @description Helper function for the creation of split violin plots with
#'   quantile segments. The function creates the `data.frame` with the quantile
#'   segments. This function is called from [Geom] `data.frame`
#'
#'   Based on the code provided on [stackoverflow](https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-with-quantiles)
#'   [https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-with-quantiles](https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-with-quantiles)
#'   originally posted on 04/Oct/2018. Last accessed 12/Jan/2024.
#'
#' @param data data to evaluate
#' @param draw_quantiles
#' @param split ; default value is `FALSE`
#' @param grp
#'
#' @return ggplot2 graphing object
#' @export
#'
#' @importFrom base cumsum rep
#' @importFrom stats cumsum
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
create_quantile_segment_frame <- function(data, draw_quantiles, split=FALSE, grp=NULL) {

  ## density values ----
  dens <- cumsum(data$density) / sum(data$density)
  ## values for the empirical cumulative distribution function ----
  ecdf <- stats::approxfun(dens, data$y)
  ys <- ecdf(draw_quantiles)

  ## calculate the minimum and maximum values ----
  violin.xminvs <- (stats::approxfun(data$y, data$xminv))(ys)
  violin.xmaxvs <- (stats::approxfun(data$y, data$xmaxv))(ys)
  ## calculate the shape of the violin plots ----
  violin.xs <- (stats::approxfun(data$y, data$x))(ys)

  ## create the data.frame based on if it has an even or odd number of violins ----
  if (grp %% 2 == 0) {
    data.frame(
      x = ggplot2:::interleave(violin.xs, violin.xmaxvs),
      y = rep(ys, each = 2), group = rep(ys, each = 2)
    )
  } else {
    data.frame(
      x = ggplot2:::interleave(violin.xminvs, violin.xs),
      y = rep(ys, each = 2), group = rep(ys, each = 2)
    )
  }
}



#' @title Create Split Violin Plots
#'
#' @description Function called to construct split violin plots via a collection of
#'   [ggplot2] functions.
#'
#'   Based on the code provided on [stackoverflow](https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-with-quantiles)
#'   [https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-with-quantiles](https://stackoverflow.com/questions/47651868/split-violin-plot-with-ggplot2-with-quantiles)
#'   originally posted on 04/Oct/2018. Last accessed 12/Jan/2024.
#'
#' @param mapping
#' @param data
#' @param stat ; default value is `"ydensity"`
#' @param position ; default value is `"identity"`
#' @param draw_quantiles; default value is `NULL`
#' @param trim; default value is `TRUE`
#' @param scale; default value is `"area"`
#' @param na.rm ; default value is `FALSE`
#' @param show.legend; default value is `NA`
#' @param inherit.aes; default value is `TRUE`
#'
#' @return `data.frame` with the quantile segments
#'
#' @importFrom ggplot2 layer
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {

  ggplot2:::layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, position = position,
                  show.legend = show.legend, inherit.aes = inherit.aes,
                  params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))

}

