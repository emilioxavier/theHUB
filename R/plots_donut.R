#' @title Construct Donut Plot Data
#'
#' @description Construct the `data.frame` for the [theHUB::make.donut.plot()] function.
#'   The raw `data.frame` or a summarised `data.frame` is acceptable and will be
#'   converted into the needed data for the construction of a donut plot.
#'
#' @param data `tibble` (or `data.frame`) with the column of interest. _**NOTE**_:
#'   Do **NOT** use counted data.
#' @param category string with column of interest containing the categories to
#'   comprise the donut (_aka_ ring). Only provide **ONE** column name.
#' @param category.order string indicating if you want the data to be ordered by
#'   `"count"` in _**decreasing**_ order or by `"category"` in alphabetical order;
#'   default: `"count"`.
#' @param category.count string with the column containing "counts" for each "category."
#'   This parameter is _**required**_ when the count (or total) for each row was
#'   pre-calculated and allows for the creation of donut data when raw data is
#'   not available and one only has the summarised values.
#' @param facetBy string indicating the column to group data by; for when you
#'   want to **facet** your donut plots via [ggplot2::facet_wrap()]; see
#'    [theHUB::make.donut.plot()].
#' @param layerBy string indicating the column to group data by; for when you
#'   want to **add layers** your donut plots; see [theHUB::make.donut.plot()].
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
#' @param layer.alpha.min value indicating the minimum alpha value; default: `0.75`
#' @param levels.rev logical indicating if the order of the categories should be
#'   reversed.
#' @param r.inner numeric value defining the inner radius of the donut; default: `4`
#' @param r.outer numeric value defining the outer radius of the donut; default: `6`
#'
#' @return `tibble` with the data to construct the donut plot.
#' @export
#'
#' @importFrom tidyselect all_of
#' @importFrom dplyr arrange desc lag
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
                            category.order="count",
                            category.count=NULL,
                            facetBy=NULL,
                            layerBy=NULL,
                            layer.order="descend",
                            layer.alpha.min=0.75,
                            levels.rev=FALSE,
                            r.inner=4,
                            r.outer=6) {

  ## sanity check ----
  # layer.order.orig <- layer.order
  ##_ if layer.order is set... ----
  # if ( !is.null(layerBy) & (length(layer.order) == 1) ) {
  #   warning.message <- paste0("The provided layer.order of ", layer.order.orig,
  #                             " was not `alphabetical`, `ascend`, or `descend` and was set to `descend`.")
  #   if ( nchar(layer.order) < 2 ) { layer.order <- "descend" }
  #   warning(warning.message,
  #           immediate.=TRUE)
  # }

  ## how to order the data ----
  category.order <- tolower(category.order)
  if ( (category.order != "count") & (category.order != "category") ) {
    category.order <- "count"
  }

  ## extract needed columns and rename ----
  donut.RAW <- dplyr::select(data, {{category}}, {{facetBy}}, {{layerBy}}, {{category.count}}) |>
    dplyr::rename("Categories"={{category}},
                  "FacetBy"={{facetBy}},
                  "LayerBy"={{layerBy}},
                  "Counts"={{category.count}}) |>
    dplyr::group_by(Categories) |>
    dplyr::ungroup()

  ## check for each type of column ----
  colNames.RAW <- colnames(donut.RAW)
  ## determine columns of interest ----
  # colNames.oi <- c(colNames.RAW[c("Categories", "FacetBy", "LayerBy") %in% colNames.RAW])
  colNames.oi <- na.omit(colNames.RAW[match(x=c("Categories", "FacetBy", "LayerBy"), table=colNames.RAW)])

  if ( !is.null(category.count) ) {
    donut.COUNTS <- dplyr::group_by(donut.RAW, dplyr::across(all_of(colNames.oi))) |>
      dplyr::tally(Counts, name="Counts") |>
      dplyr::group_by(dplyr::across(all_of(rev(colNames.oi[-1]))))
  } else {
    donut.COUNTS <- dplyr::group_by(donut.RAW, dplyr::across(all_of(colNames.oi))) |>
      dplyr::summarise(Counts=n()) |>
      dplyr::group_by(dplyr::across(all_of(rev(colNames.oi[-1]))))
  }

  ## category, facet, and layer counts ----
  n.categories <- length(unique(donut.COUNTS$Categories))
  if ( !is.null(facetBy) ) {
    n.facets <- length(unique(donut.RAW$FacetBy))
  }
  if ( !is.null(layerBy) ) {
    n.layers <- length(unique(donut.RAW$LayerBy))
  }

  ## number of layers ----
  layer.order.orig <- layer.order
  if ( !is.null(layerBy) ) {
    n.layers <- length(unique(donut.RAW$LayerBy))

    ##_ alpha values ----
    alpha.max <- 1
    if ( is.null(layer.alpha.min) ) {
      layer.alpha.min <- alpha.max/n.layers
    }
    alpha.dist <- (alpha.max-layer.alpha.min)/(n.layers-1)
    LayerAlpha <- seq(from=layer.alpha.min, to=1, by=alpha.dist)

    ##_ layer summary ----
    if ( n.layers > 1 ) {
      layer.summary <- dplyr::group_by(donut.RAW, LayerBy) |>
        dplyr::summarise(n=n(),
                         total=sum(Counts))
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
                                          xmax=layer.outer,
                                          LayerAlpha=LayerAlpha)
    }
  } else {
    LayerAlpha <- n.layers <- 1
  }

  ## arrange data by count OR category ----
  if ( (category.order == "count") & (is.null(layerBy)) ) {
    donut.COUNTS <- dplyr::arrange(donut.COUNTS, desc(Counts))
  }
  if ( category.order == "category" ) {
    donut.COUNTS <- dplyr::arrange(donut.COUNTS, Categories)
  }

  ## construct the donut.DATA ----
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
                              ymin=dplyr::coalesce(lag(ymax), 0),
                              LayerAlpha=LayerAlpha[1])

  ##_ arrange by the facet ----
  if ( !is.null(facetBy) ) {
    donut.DATA <- dplyr::arrange(donut.DATA, FacetBy, ymin)
  }

  ##_ add the layer information ----
  if ( !is.null(layerBy) ) {
    donut.DATA <- dplyr::left_join(x=dplyr::select(donut.DATA, -xmin, -xmax, -LayerAlpha), y=layer.summary,
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
#' @description Construct the donut plot using the donut data constructed with
#'   [theHUB::make.donut.data()] function. This function automatically determines
#'   if the provided data includes information for creating a facet of donut
#'   plots and including layers.
#'
#' @param donut.DATA `tibble` (or `data.frame`) constructed via [make.donut.data()]
#' @param colour.palette vector of strings containing the colour palette.
#' @param colour.outline string indicating the colour of the donut's outline.
#' @param label.col string with the column containing the labels. Setting to `NULL`
#'   will result in no labels.
#' @param label.size label sizes; default: `3.5`
#' @param facet.nrow numeric [ggplot2::facet_wrap()] rows; default: `NULL` Only
#'   provide `facet.nrow` _**OR**_ `facet.ncol`.
#' @param facet.ncol numeric [ggplot2::facet_wrap()] columns; default: `NULL` Only
#'   provide `facet.nrow` _**OR**_ `facet.ncol`.
#'
#' @details
#' **Custom Colour Palette**
#'   When creating a collection of donut plots in a facet it is important to have
#'   the same colours assigned to the same segments for all donut plots. Often,
#'   each donut plot does not always include the same collection of segments. The
#'   following code ensures that the same segment-colour combination is used for each
#'   donut plot.
#'
#' ```
#' custom.palette <- msu.palette[1:9]
#' names(custom.palette) <- all.donut.DATA$Categories |> unique()
#' ```
#'
#'   If a "named colour palette" is not provided, one is created from the categories
#'   within the `donut.DATA` data.frame and the provided `colour.palette` to ensure
#'   a consistent colour palette is used for all donut plots.
#'
#' @return ggplot2 graphic object
#'
#' @importFrom ggplot2 geom_rect coord_polar scale_fill_manual vars
#' @importFrom ggplot2 guide_legend xlim guides theme_bw facet_wrap
#' @importFrom grid unit
#' @importFrom stringr str_wrap
#'
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
                            colour.outline="grey90",
                            label.col=NULL,
                            label.size=3.5,
                            facet.nrow=NULL,
                            facet.ncol=NULL) {

  ## determine the xlimit maximum ----
  xlim.max <- max(donut.DATA$xmax)

  ## check facet information ----
  colNames.oi <- colnames(donut.DATA)
  facet.tf <- any(colnames(donut.DATA) %in% "FacetBy")
  if ( facet.tf ) {
    facet.count <- length(unique(donut.DATA$FacetBy))

    ##_ facet information but no indication of the number of columns or rows ----
    if ( is.null(facet.ncol) & is.null(facet.nrow) ) {
      facet.mess <- paste0("The donut.DATA contains a FacetBy column with ",
                           facet.count, " individual donut plots, but the ",
                           "facet.nrow OR facet.ncol is not set. Please ",
                           "indicated the number of rows OR columns.")
      facet.mess <- stringr::str_wrap(facet.mess,
                                      width=80, indent=0, exdent=7,
                                      whitespace_only=TRUE)
      stop(facet.mess)
    }

  ##_ facet information but both the number of columns and rows provided ----
    if ( is.numeric(facet.ncol) & is.numeric(facet.nrow) ) {
      facet.mess <- paste0("The number of columns (", facet.ncol, ") and rows (",
                           facet.nrow, ") are provided and only ONE of these ",
                           "parameters is needed. Only the number of columns will ",
                           "be retained. In the future, please indicated the ",
                           "number of columns OR rows.")
      facet.mess <- stringr::str_wrap(facet.mess,
                                      width=80, indent=0, exdent=0,
                                      whitespace_only=TRUE)
      message(facet.mess)
      facet.nrow <- NULL
    }
  }

  ## construct the colour values vector ----
  if ( is.null(names(colour.palette)) ) {
    category.values <- unique(donut.DATA$Categories)
    colour.palette <- colour.palette[1:length(category.values)]
    names(colour.palette) <- category.values
  }

  ## build the donut plot ----
  # donut.plot <- ggplot(data=donut.DATA, aes(fill=Categories, ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin)) +
  # ggplot(data=donut.DATA, aes(fill=Categories, alpha=LayerBy, ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin)) +
  donut.plot <- ggplot(data=donut.DATA, aes(fill=Categories, ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin)) +
    geom_rect(colour=colour.outline, alpha=donut.DATA$LayerAlpha) +
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
                             nrow=1),
           alpha="none") +
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

  ## return plot to user ----
  return(donut.plot)
}
