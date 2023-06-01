#' @title Construct Donut Plot Data
#'
#' @description add the description of making the data
#'
#' @param data `tibble` (or `data.frame`) with the column of interest. _**NOTE**_:
#'   Do **NOT** use counted data.
#' @param col.oi string with column of interest. Only provide **ONE** column name.
#' @param facetBy string indicating the column to group data by; for when you
#'   want to **facet** your donut plots via [ggplot2::facet_wrap()]; see
#'    [make.donut.plot()].
#' @param category.order string indicating if you want the data to be ordered by
#'   `"count"` in _**decreasing**_ order or by `"category"` in alphabetical order;
#'   default: `"count"`.
#' @param levels.rev logical indicating if the order of the categories should be
#'   reversed.
#' @param col.count string with the column containing "counts" for each "category."
#'   This parameter is _**required**_ when the count (or total) for each row was
#'   pre-calculated and allows for the creation of donut data when raw data is
#'   not available and one only has the summarised values.
#' @param r.inner numeric value defining the inner radius of the donut; default: `4`
#' @param r.outer numeric value defining the outer radius of the donut; default: `6`
#'
#' @return `tibble` with the
#' @export
#'
#' @examples
#' \dontrun{
#'   donut.DATA <- make.donut.data(data, col.oi, facetBy=NULL,
#'                                 category.order="count", levels.rev=FALSE,
#'                                 r.inner=4, r.outer=6)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.donut.data <- function(data,
                            col.oi,
                            facetBy=NULL,
                            layerBy=NULL,
                            category.order="count",
                            levels.rev=FALSE,
                            col.count=NULL,
                            r.inner=4,
                            r.outer=6) {

  ## stat versus identity ----
  orig.nRows <- nrow(data)
  orig.nCats <- data[[col.oi]] |> unique() |> length()

  ## extract needed data and rename ----
  donut.RAW <- dplyr::select(data, {{col.oi}}, {{facetBy}}, {{layerBy}}) |>
    dplyr::rename("Categories"={{col.oi}},
                  "FacetBy"={{facetBy}},
                  "LayerBy"={{layerBy}})

  ## determine layer breaks ----
  layer.n <- donut.RAW$LayerBy |> unique() |> length()
  layer.width <- (r.outer - r.inner) / layer.n
  layer.width.breaks <- seq(from=r.inner, to=r.outer, by=layer.width) |> rev()
  layer.alpha <- 0.50/layer.n
  layer.alpha.values <- seq(from=0.5, to=1, by=layer.alpha) |> rev()
  layer.INFO <- dplyr::count(donut.RAW, LayerBy) |>
    dplyr::arrange(desc(n)) |>
    mutate(xmax=layer.width.breaks[-c(layer.n+1)],
           xmin=layer.width.breaks[-1],
           alpha.value=layer.alpha.values[-c(layer.n+1)]
           )

  ## add the counts ----
  ##_ determine the present columns ----
  colNames.RAW <- colnames(donut.RAW)
  colNames.oi <- c(colNames.RAW[c("Categories", "FacetBy", "LayerBy") %in% colNames.RAW]) |>
    rev()
  ##_ summarise the data based on present columns ----
  donut.COUNTS <- dplyr::group_by(donut.RAW, dplyr::across(all_of(colNames.oi))) |>
    dplyr::summarise(Count=n())

  ## how to order the data ----
  category.order <- tolower(category.order)
  if ( (category.order != "count") & (category.order != "category") ) {
    category.order <- "count"
  }
  ##_ arrange data by count ----
  if ( category.order == "count" ) {
    donut.COUNTS <- dplyr::arrange(donut.COUNTS, desc(Count), Categories)
  }
  ##_ arrange data by category ----
  if ( category.order == "category" ) {
    donut.COUNTS <- dplyr::arrange(donut.COUNTS, Categories)
  }

  ## create the donut data data.frame ----
  donut.DATA <- dplyr::mutate(donut.COUNTS,
                              Total=sum(Count),
                              Ratio=Count/Total,
                              Percent=Ratio*100,
                              Label.pct=paste(round(Percent, digits=0), "%", sep=""),
                              Label.category=Categories,
                              Label.short=paste(Categories, Label.pct, sep=" "),
                              Label.long=paste(Categories, "\n(", Label.pct, "; n=", Count, ")", sep=""),
                              count.label.pos=cumsum(Count)-0.5*Count,
                              pct.label.pos=count.label.pos/Total*100,
                              xmin={{r.inner}},
                              xmax={{r.outer}},
                              ymax=cumsum(Ratio),
                              ymin=dplyr::coalesce(lag(ymax), 0))
  ##_ facet re-arrange ----
  if ( !is.null(facetBy) ) {
    donut.DATA <- dplyr::arrange(donut.DATA, FacetBy, ymin)
  }
  ##_ replace the xmin and xmax values if layers are present ----
  if ( !is.null(layerBy) ) {
    donut.DATA <- dplyr::select(donut.DATA, -xmin, -xmax) |>
      dplyr::left_join(y=select(layer.INFO, -n), by="LayerBy")
  }

  ## set the factor levels ----
  ##_ categories ----
  categories <- unique(donut.DATA$Categories)
  if (levels.rev == TRUE) {
    donut.DATA$Categories <- factor(x=donut.DATA$Categories,
                                    levels=rev(categories))
  } else {
    donut.DATA$Categories <- factor(x=donut.DATA$Categories,
                                    levels=categories)
  }
  ##_ layers ----
  donut.DATA$LayerBy <- factor(x=donut.DATA$LayerBy,
                                  levels=layer.order$LayerBy)

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
#'   donut.DATA <- make.donut.data(data, col.oi, facetBy=NULL,
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

  ## determine if layers are present ----
  layers.TF <- "LayerBy" %in% colnames(donut.DATA)

  ## build the donut plot ----
  donut.plot <- ggplot(data=donut.DATA, aes(fill=Categories, ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin))
    ## add bar outline (aka colour) if layers are present ----
  if ( layers.TF==TRUE ) {
    donut.plot <- donut.plot + geom_rect(colour="white", alpha=donut.DATA$alpha.value)
  } else {
    donut.plot <- donut.plot + geom_rect()
  }
  donut.plot <- donut.plot + coord_polar(theta="y") +
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
  if ( !is.null(facet.nrow) | !is.null(facet.ncol) ) {
    donut.plot <- donut.plot + facet_wrap(facets=vars(.data$FacetBy),
                                          nrow=facet.nrow,
                                          ncol=facet.ncol)
  }

  # donut.plot
  ## return the plot ----
  return(donut.plot)
}
