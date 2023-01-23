#' @title Construct Donut Plot Data
#'
#' @description add the description of making the data
#'
#' @param data `tibble` (or `data.frame`) with the column of interest. _**NOTE**_:
#'   Do **NOT** use counted data.
#' @param category string with column of interest containing the categories to
#'   comprise the donut (_aka_ ring). Only provide **ONE** column name.
#' @param facetBy string indicating the column to group data by; for when you
#'   want to **facet** your donut plots via [ggplot2::facet_wrap()]; see
#'    [make.donut.plot()].
#' @param layer.order string indicating the order of the layers. There are four
#'   options:
#'
#'   * `ascend` where the inner ring (donut) has the smallest value and the outer
#'   donut has the greatest value
#'   * `descend` where the inner donut as the largest value and the outer ring
#'   has the smallest value
#'   * `alphabetical` where the rings are ordered alphabetically starting from
#'   the inner ring
#'   * `user defined` where the user provides the order of the donuts starting
#'   from inner ring. Only layers included in the vector (_e.g._, `c("4", "r", "f")`)
#'   are included in the resulting data.
#'
#'   For `alphabetical`, `ascend`, and `descend` only the first _**two**_ characters
#'   are needed.
#' @param category.order string indicating if you want the data to be ordered by
#'   `"count"` in _**decreasing**_ order or by `"category"` in alphabetical order;
#'   default: `"count"`.
#' @param levels.rev logical indicating if the order of the categories should be
#'   reversed.
#  @param category.count string with the column containing "counts" for each "category."
#    This parameter is _**required**_ when the count (or total) for each row was
#    pre-calculated and allows for the creation of donut data when raw data is
#    not available and one only has the summarised values.
#' @param r.inner numeric value defining the inner radius of the donut; default: `4`
#' @param r.outer numeric value defining the outer radius of the donut; default: `6`
#'
#' @return `tibble` with the
#' @export
#'
#' @examples
#' \dontrun{
#'   donut.DATA <- make.donut.data(data, category, facetBy=NULL,
#'                                 category.order="count", levels.rev=FALSE,
#'                                 r.inner=4, r.outer=6)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.donut.data <- function(data,
                            category,
                            facetBy=NULL,
                            layerBy=NULL,
                            layer.order="descend",
                            category.order="count",
                            levels.rev=FALSE,
                            category.count=NULL,
                            r.inner=4,
                            r.outer=6) {

  ## sanity check ----
  layer.order.orig <- layer.order
  ##_ if layer.order is set... ----
  if ( !is.null(layerBy) & (length(layer.order) == 1) ) {
    warning.message <- paste0("The provided layer.order of ", layer.order.orig, " was not `alphabetical`, `ascend`, or `descend` and was set to `descend`.")
    if ( nchar(layer.order) < 2 ) { layer.order <- "descend" }
    warning(warning.message,
            immediate.=TRUE)
  }

  ## extract needed data and rename ----
  donut.RAW <- dplyr::select(data, {{category}}, {{facetBy}}, {{layerBy}}) |>
    dplyr::rename("Categories"={{category}},
                  "FacetBy"={{facetBy}},
                  "LayerBy"={{layerBy}})

  ## number of facets ----
  if ( !is.null(facetBy) ) {
    n.facets <- unique(donut.RAW$FacetBy) |> length()
  }

  ## number of layers ----
  if ( !is.null(layerBy) ) {
    n.layers <- unique(donut.RAW$LayerBy) |> length()
    ##_ layer summary ----
    if ( n.layers > 1 ) {
      layer.summary <- dplyr::group_by(donut.RAW, LayerBy) |>
        dplyr::summarise(n=n())
      ##_ layer order ----
      layer.order.n <- length(layer.order)
      ##__ if only one layer is provided ----
      if ( layer.order.n == 1 ) {
        layer.order.mod <- substr(x=layer.order, start=1, stop=2)
        ##___ two characters provided ----
        if ( layer.order.mod == "as" ) { layer.order.type <- "ascend" }
        if ( layer.order.mod == "de" ) { layer.order.type <- "descend" }
        if ( layer.order.mod == "al" ) { layer.order.type <- "alphabetical" }
      }
      ##__ if multiple layers are provided ----
      if ( layer.order.n > 1 ) { layer.order.type <- "user" }
      ##__ re-order the layer.summary tibble ----
      if (layer.order.type == "ascend") { layer.summary <- arrange(layer.summary, n) }
      if (layer.order.type == "descend") { layer.summary <- arrange(layer.summary, desc(n)) }
      if (layer.order.type == "alphabetical") { layer.summary <- arrange(layer.summary, LayerBy) }
      if (layer.order.type == "user") {
        layer.order.tb <- tibble::tibble(LayerBy=layer.order)
        layer.summary <- left_join(x=layer.order.tb, y=layer.summary, by="LayerBy")
      }
      ##_ layer widths ----
      layer.width <- (r.outer - r.inner)/n.layers
      layer.breaks <- seq(from=r.inner, to=r.outer, by=layer.width)
      layer.inner <- layer.breaks[-c(n.layers+1)]
      layer.outer <- layer.breaks[-1]
      layer.summary <- tibble::add_column(layer.summary,
                                          xmin=layer.inner,
                                          xmax=layer.outer)
    }
  }

  ## check for each type of column ----
  colNames.RAW <- colnames(donut.RAW)

  ## determine columns of interest ----
  colNames.oi <- c(colNames.RAW[c("Categories", "FacetBy", "LayerBy") %in% colNames.RAW])

  ## calculate the counts for each category, facet, and layer ----
  donut.COUNTS <- dplyr::group_by(donut.RAW, dplyr::across(all_of(colNames.oi))) |>
    dplyr::tally(name="Counts") |>
    dplyr::group_by(dplyr::across(all_of(rev(colNames.oi[-1]))))

  ## how to order the data ----
  category.order <- tolower(category.order)
  if ( (category.order != "count") & (category.order != "category") ) {
    category.order <- "count"
  }


  ## arrange data by count OR category ----
  if ( category.order == "count" ) {
    donut.COUNTS <- dplyr::arrange(donut.COUNTS, desc(Counts))
  }
  if ( category.order == "category" ) {
    donut.COUNTS <- dplyr::arrange(donut.COUNTS, Categories)
  }
  # if ( !is.null(facetBy) ){
  #   donut.COUNTS <- dplyr::group_by(donut.COUNTS, FacetBy)
  # }

  donut.DATA <- dplyr::mutate(donut.COUNTS,
                              Total=sum(Counts),
                              Ratios=Counts/Total,
                              Percents=Ratios*100,
                              Labels.pct=paste(round(Percents, digits=0), "%", sep=""),
                              Labels.category=Categories,
                              Labels.short=paste(Categories, Labels.pct, sep=" "),
                              Labels.long=paste(Categories, "\n(", Labels.pct, "; n=", Counts, ")", sep=""),
                              count.label.pos=cumsum(Counts)-0.5*Counts,
                              pct.label.pos=count.label.pos/Total*100,
                              xmin={{r.inner}},
                              xmax={{r.outer}},
                              ymax=cumsum(Ratios),
                              ymin=dplyr::coalesce(lag(ymax), 0))

  if ( !is.null(facetBy) ) {
    donut.DATA <- dplyr::arrange(donut.DATA, FacetBy, ymin)
  }

  if ( !is.null(layerBy) ) {
    donut.DATA <- dplyr::left_join(x=dplyr::select(donut.DATA, -xmin, -xmax), y=layer.summary,
                                   by="LayerBy")
  }

  ## set the factor levels ----
  categories <- unique(donut.DATA$Categories)
  if (levels.rev == TRUE) {
    donut.DATA$Categories <- factor(x=donut.DATA$Categories,
                                    levels=rev(categories))
  } else {
    donut.DATA$Categories <- factor(x=donut.DATA$Categories,
                                    levels=categories)
  }


  ## return donut data ----
  return(donut.DATA)
}


#' @title Construct Donut Plot
#'
#' @description add the description of making the plot
#'
#' @param donut.DATA `tibble` (or `data.frame`) constructed via [make.donut.data()]
#' @param colour.palette vector of strings containing the colour palette.
#' @param label.col string with the column containing the labels. Setting to `NULL`
#'   will result in no labels.
#' @param label.size label sizes; default: `3.5`
#' @param facet.nrow numeric [ggplot2::facet_wrap()] rows; default: `NULL`
#' @param facet.ncol numeric [ggplot2::facet_wrap()] columns; default: `NULL`
#'
#' @return ggplot2 graphic object
#' @export
#'
#' @examples
#' \dontrun{
#'   donut.DATA <- make.donut.data(data, category, facetBy=NULL,
#'                                 category.order="count", levels.rev=FALSE,
#'                                 r.inner=4, r.outer=6)
#'   donut.PLOT <- make.donut.plot(donut.DATA, colour.palette=msu.palette,
#'                                 label.col=NULL, label.size=3.5,
#'                                 facet.nrow=NULL, facet.ncol=NULL)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.donut.plot <- function(donut.DATA,
                            colour.palette,
                            label.col=NULL,
                            label.size=3.5,
                            facet.nrow=NULL,
                            facet.ncol=NULL) {

  ## determine the xlimit maximum ----
  xlim.max <- max(donut.DATA$xmax)

  colNames.oi <- colnames(donut.DATA)

  ## build the donut plot ----
  donut.plot <- ggplot(data=donut.DATA, aes(fill=Categories, ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin)) +
    geom_rect() +
    coord_polar(theta="y") +
    labs(x=NULL, y=NULL, title=NULL) +
    scale_fill_manual(values=colour.palette,
                      # breaks=levels(.data$Categories),
                      breaks=levels(donut.DATA$Categories),
                      guide=guide_legend(nrow=3, byrow=TRUE, reverse=TRUE)) +
    xlim(c(0, xlim.max)) +
    guides(fill=guide_legend(title.position="top",  ## place title on top of legend (not needed)
                             title.hjust=0.5,  ## center the title (not needed)
                             label.position="bottom",
                             nrow=1)) +
    theme_bw() +
    cowplot::theme_cowplot() +
    theme(panel.border=element_blank(),
          panel.grid=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          line=element_blank(),
          legend.title=element_blank(),
          legend.text=element_text(size=8),
          legend.position="bottom",
          legend.key.height=unit(0.35, "cm"),
          legend.box.margin=margin(c(-50,0,0,0)))

  ## option to include labels ----
  if ( !is.null(label.col) ) {
    donut.plot <- donut.plot + ggrepel::geom_label_repel(aes(label=.data[[label.col]],
                                                             x=xmax-0.75,
                                                             y=(ymin+ymax)/2),
                                                         inherit.aes=FALSE,
                                                         show.legend=FALSE,
                                                         size=label.size)
  }

  ## option to make facets ----
  if ( !is.null(facet.nrow) | !is.null(facet.ncol) & ("FacetBy" %in% colNames.oi) ) {
    donut.plot <- donut.plot + facet_wrap(facets=vars(.data$FacetBy),
                                          nrow=facet.nrow,
                                          ncol=facet.ncol)
  }

  return(donut.plot)
}
