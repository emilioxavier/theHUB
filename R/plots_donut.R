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

  ## how to order the data ----
  category.order <- tolower(category.order)
  if ( (category.order != "count") & (category.order != "category") ) {
    category.order <- "count"
  }

  ## select the needed columns ----
  if ( !is.null(col.oi) ) {
    donut.DATA <- dplyr::rename(data, "Categories"={{col.oi}})
  }

  if ( !is.null(facetBy) ) {
    donut.DATA <- dplyr::rename(data, "FacetBy"={{facetBy}})
  }

  if ( !is.null(layerBy) ) {
    donut.DATA <- dplyr::rename(data, "LayerBy"={{layerBy}})
  }

  if ( !is.null(col.count) ) {
    donut.DATA <- dplyr::rename(data, "Counts"={{col.count}})
  }

  # if ( is.null(facetBy) & is.null(layerBy)) {
  #   donut.DATA <- dplyr::select(data, {{col.oi}}) |>
  #     dplyr::rename("Categories"={{col.oi}}) |>
  #     tibble::add_column("FacetBy"=NA,
  #                        "LayerBy"=NA) |>
  #     dplyr::ungroup()
  # }
  # if ( !is.null(facetBy) ) {
  #   donut.DATA <- dplyr::select(data, {{col.oi}}, {{facetBy}}, {{col.count}}) |>
  #     dplyr::rename("Categories"={{col.oi}},
  #                   "FacetBy"={{facetBy}},
  #                   "Counts"={{col.count}}) |>
  #     dplyr::group_by(Categories, FacetBy)
  # }
  # if ( !is.null(facetBy) & !is.null(layerBy)) {
  #   donut.DATA <- dplyr::select(data, {{col.oi}}, {{facetBy}}, {{layerBy}}, {{col.count}}) |>
  #     dplyr::rename("Categories"={{col.oi}},
  #                   "FacetBy"={{facetBy}},
  #                   "LayerBy"={{layerBy}},
  #                   "Counts"={{col.count}}) |>
  #     dplyr::group_by(Categories) |>
  #     tibble::add_column("FacetBy"=NA)
  # }
  # if ( !is.null(col.count) ) {
  #   donut.DATA <- dplyr::select(data, {{col.count}}) |>
  #     dplyr::rename("Categories"={{col.oi}},
  #                   "FacetBy"={{facetBy}},
  #                   "LayerBy"={{layerBy}},
  #                   "Counts"={{col.count}}) |>
  #     dplyr::group_by(Categories) |>
  #     tibble::add_column("FacetBy"=NA)
  # }

  ## add the counts ----
  ##_ counts already calculated ----
  if ( !is.null(col.count) ) {
    donut.COUNTS <- dplyr::select(data, {{col.oi}}, {{col.count}}) |>
      dplyr::rename("Categories"={{col.oi}},
                    "Counts"={{col.count}}) |>
      dplyr::ungroup()
  }
  ##_ calculate the counts ----
  if ( ( orig.nRows != orig.nCats ) & ( is.null(col.count) ) ) {
    donut.COUNTS <- dplyr::summarise(donut.DATA, Counts=n()) |>
      dplyr::ungroup()
  }
  donut.DATA <- dplyr::left_join(x=donut.DATA, y=donut.COUNTS) |>
    select(Categories, FacetBy, Counts)

  ## arrange data by count OR category ----
  # if ( is.null(facetBy) ) {
  if ( category.order == "count" ) {
    donut.DATA <- dplyr::arrange(donut.DATA, desc(Counts))
  }
  if ( category.order == "category" ) {
    donut.DATA <- dplyr::arrange(donut.DATA, Categories)
  }
  # }
  if ( !is.null(facetBy) ){
    donut.DATA <- dplyr::group_by(donut.DATA, FacetBy)
  }

  donut.DATA <- dplyr::mutate(donut.DATA,
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

  ## set the factor levels ----
  categories <- unique(donut.DATA$Categories)
  if (levels.rev == TRUE) {
    donut.DATA$Categories <- factor(x=donut.DATA$Categories,
                                    levels=rev(categories))
  } else {
    donut.DATA$Categories <- factor(x=donut.DATA$Categories,
                                    levels=categories)
  }

  ## remove FacetBy columns that are NA ----

  ## adjust the donut rings ----
  # if ( any(colnames(donut.DATA)=="FacetBy") ) {
  #
  #
  # }

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
  if ( !is.null(facet.nrow) | !is.null(facet.ncol) ) {
    donut.plot <- donut.plot + facet_wrap(facets=vars(.data$FacetBy),
                                          nrow=facet.nrow,
                                          ncol=facet.ncol)
  }

  return(donut.plot)
}
