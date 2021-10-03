
#' @title Phrase/Integer Conversion
#'
#' @description Convert a collection of characters to another set of characters.
#'
#' @details The function was initially designed to convert Likert responses to
#'   integers, but it was quickly realized that it could easily be used for a
#'   multitude of sins against data. The function relies on a user provided
#'   `tibble` or `data.frame` with two columns; one with the characters to be
#'   converted and the characters to be converted to.
#'
#'   The function is designed to work with [dplyr::mutate()] allowing multiple
#'   conversions in a single command.
#'
#'   _**Note**_: Ideally, the integers are positive and non-zero.
#'
#' @param responses column to be converted; _e.g._, `Q1`
#' @param fromto.tb `tibble` (or `data.frame`) with the matched convertee and
#'   converted pairs; see the example below.
#' @param from column in the `fromto.tb` containing the characters to be converted
#' @param to column in the `fromto.tb` containing the characters being converted to
#'
#' @return a vector of converted characters/phrases
#' @export
#'
#' @examples
#' set.seed(13)
#' phrase2int.tb <- tibble::tibble(phrase=c("hated it!", "meh", "loved it!"),
#'                                 integer=c(-1, 0, 1))
#' responses.words <- sample(x=c("hated it!", "meh", "loved it!"), size=5, replace=TRUE)
#' responses.integers <- sample(x=c(-1, 0, 1), size=5, replace=TRUE)
#'
#' convert.fromto(responses=responses.words,
#'                fromto.tb=phrase2int.tb,
#'                from="phrase", to="integer")
#' # [1]  1 -1  0 -1  0
#'
#' convert.fromto(responses=responses.integers,
#'                fromto.tb=phrase2int.tb,
#'                from="integer", to="phrase")
#' # [1] "meh"       "hated it!" "loved it!" "hated it!" "meh"
#'
#' \dontrun{
#' mutate(tibble.oi, Q1.ints=convert.fromto(responses=Q1,
#'                                          fromto.tb=phrase2int.tb,
#'                                          from="phrase", to="integer"))
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
convert.fromto <- function(responses, fromto.tb, from, to) {

  converted <- fromto.tb[[to]][match(responses, table=fromto.tb[[from]])]

  return(converted)
}



#' @title Likert (and Likert-like) Barplot
#'
#' @description Constructs a [ggplot2::] barplot for Likert and Likert-like results
#'   with each option displayed as an individual, horizontal MSU dark-green bars.
#'   The percentage of responses are noted on the bar as white text.
#'
#'   This function is called by [likert.plot.matrix()] but can be called individually.
#'
#' @param data `tibble` (or `data.frame`) with Likert data converted to integers.
#'   Use the [convert.fromto()] function to convert text responses to integers.
#'   _**Note**_: Ensure the integers are positive and non-zero.
#' @param QoI string with the Question of Interest (the rows)
#'
#' @return ggplot2 graphics object
#' @export
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_bar labs geom_text after_stat position_stack stat_count
#' @importFrom cowplot theme_nothing
#'
#' @examples
#' \dontrun{
#' likert.barplot(data=fs20.likert, QoI="Q1")
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
likert.barplot <- function(data, QoI, value.range) {

  ## create data for plot ----
  ##_ tibble with values of interest ----
  data.QoI <- tibble::tibble(value=data[[QoI]])
  data.bar <- group_by(data.QoI, value) %>%
    summarise(count=n())
  ##_ tibble with range of values ----
  data.range <- tibble::tibble(value=value.range)
  ##_ construct plot data ----
  data.bar <- full_join(x=data.range, y=data.bar, by="value") %>%
    replace_na(list(count=0)) %>%
    mutate(total=sum(count),
           pct=as.integer(round(count/total*100), digits=0),
           label.full=paste(count, " (",pct,"%)", sep="")) %>%
    mutate(label.full=case_when(count==0~"",
                                TRUE~as.character(label.full)))

  ## construct plot ----
  Q.barplot <- ggplot(data=data.bar, aes(x=count, y=value)) +
    geom_col(fill=msu.darkGreen, orientation="y") +
    labs(x=NULL, y=NULL) +
    geom_text(aes(x=count/2, y=value, label=label.full),
              stat="identity",
              colour="white", family="Georgia",
              size=4) +
    theme_nothing()

  # bar.data <- dplyr::group_by(data, .data[[QoI]]) %>%
  #   dplyr::summarise(group.count=n()) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(total=sum(group.count),
  #                 pct=as.integer(round(group.count/total*100, digits=0)))

  # Q.barplot <- ggplot(data=data, aes(x=..count.., y=.data[[QoI]])) +
  # Q.barplot <- ggplot(data=data, aes(x=after_stat(count), y=.data[[QoI]])) +
  #   # geom_bar(fill=msu.darkGreen) +
  #   geom_bar(fill=msu.darkGreen) +
  #   labs(x=NULL, y=NULL) +
  #   # geom_text(stat="count", aes(label=..count..), colour="white", family="Georgia", size=5, position=position_stack(vjust=0.5)) +
  #   # geom_text(stat="count", aes(label=paste(..count.., " (", after_stat(round((count/sum(count)*100), digits=0)), "%)", sep="")),
  #   geom_text(stat="count", aes(label=paste(after_stat(count),
  #                                           " (", after_stat(round((.data$count/sum(.data$count)*100), digits=0)),
  #                                           "%)", sep="")),
  #                               colour="white", family="Georgia",
  #                               size=4, position=position_stack(vjust=0.5)) +
  #   theme_nothing()

  return(Q.barplot)
}


#' @title Construct Likert (and Likert-like) Pairwise Heatmap
#'
#' @description Constructs a [ggplot2::] heatmap for Likert and Likert-like results
#'   with each option for the question of interest (`QoI`) represented on the row
#'   while the columns are the question being compared (`Qcompared`). Each tile
#'   (square) is shaded based on the percent overlap and the percentage of
#'   overlapping responses are noted as MSU dark-green text on a white box.
#'
#'   This function is called by [likert.plot.matrix()] but can be called individually.
#'
#' @param data `tibble` (or `data.frame`) with Likert data converted to integers.
#'   Use the [convert.fromto()] function to convert text responses to integers.
#'   _**Note**_: Ensure the integers are positive and non-zero.
#' @param QoI string with the Question of Interest (the rows)
#' @param Qcompared string with the Question being Compared to (the columns)
#'
#' @return ggplot2 graphics object
#' @export
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_raster scale_fill_gradientn geom_label scale_x_reverse labs
#' @importFrom cowplot theme_nothing
#'
#' @examples
#' \dontrun{
#' likert.heatmap(data=fs20.likert, QoI="Q1", Qcompared="Q2")
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
likert.heatmap <- function(data, QoI, Qcompared, value.range) {

  data.heatmap <- make.likert.heatmap.data(data, QoI, Qcompared, value.range)

  QvQ.heatmap <- ggplot(data=data.heatmap, aes(x=.data$Qcompared.ints, y=.data[[QoI]], fill=.data$pct)) +
    geom_raster(aes(fill=hex)) +
    scale_fill_identity() +
    geom_label(aes(label=paste(.data$pct,"%", sep="")),
               fill="white", colour="#18453B", family="Georgia",
               size=5, hjust="middle", vjust="middle") +
    scale_x_reverse() +
    labs(x=NULL, y=NULL) +
    theme_nothing()
    # theme(panel.background=element_blank(),
    #       legend.position="none",
    #       axis.text=element_text(size = rel(1.5)),
    #       axis.ticks=element_blank())

  return(QvQ.heatmap)
}


#' @title Construct Pairwise Heatmap Data
#'
#' @description Construct the pairwise heatmap data for [likert.heatmap()].
#'
#'   This function is called by [likert.heatmap()] but can be called individually.
#'
#' @param data `tibble` (or `data.frame`) with Likert data converted to integers.
#'   Use the [convert.fromto()] function to convert text responses to integers.
#'   _**Note**_: Ensure the integers are positive and non-zero.
#' @param QoI string with the Question of Interest (the rows)
#' @param Qcompared string with the Question being Compared to (the columns)
#'
#' @return `tibble` (or `data.frame`) containing the needed count, percentage, and
#'   colour hex values to construct a heatmap with percentage values overlaid on
#'   each tile of the heatmap.
#'
#' @importFrom rlang .data
#' @importFrom dplyr left_join group_by summarise ungroup mutate select n
#' @importFrom tidyr pivot_longer pivot_wider separate
#' @importFrom magrittr %>%
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.likert.heatmap.data <- function(data, QoI, Qcompared, value.range) {

  options(dplyr.summarise.inform=FALSE)
  data.oi <- select(data, {{QoI}}, {{Qcompared}})

  data.range <- tibble::tibble(!!QoI:=value.range, !!Qcompared:=value.range)
  data.range <- expand(data.range, .data[[QoI]], .data[[Qcompared]])

  # QvQ.data <- group_by(data.oi, .data[[QoI]], .data[[Qcompared]]) %>%
  #   summarise("n"=n()) %>%
  #   ungroup() %>%
  #   mutate("QoI.n"=sum(n), "QoI.pct"=as.integer(round(n/.data$QoI.n*100, digits=0))) %>%
  #   select(-"QoI.n", -"n") %>%
  #   # # pivot_wider(names_from="Q.overall.comm.MSU.leaders", names_prefix="Q2.", values_from="QoI.pct", values_fill=0) %>%
  #   # pivot_wider(names_from="Q2.ints", names_prefix="Q2.", values_from="QoI.pct", values_fill=0) %>%
  #   pivot_wider(names_from={{Qcompared}}, names_prefix="Qcompare.", values_from="QoI.pct", values_fill=0) %>%
  #   pivot_longer(cols=-{{QoI}}, names_to="Q.compared", values_to="pct") %>%
  #   separate(col="Q.compared", into=c("pre", "Qcompared.ints"), sep="\\.") %>%
  #   select(-"pre") %>%
  #   mutate("Qcompared.ints"=as.integer(.data$Qcompared.ints)) %>%
  #   left_join(msu.heatmap.100, by="pct")

  QvQ.data.1 <- group_by(data.oi, .data[[QoI]], .data[[Qcompared]]) %>%
    summarise("n"=n()) %>%
    ungroup()

  QvQ.data <- full_join(x=data.range, y=QvQ.data.1,
                        by=c({{QoI}}, {{Qcompared}})) %>%
    replace_na(list(n=0L)) %>%
    mutate("QoI.n"=sum(n),
           "pct"=as.integer(round(n/.data$QoI.n*100, digits=0))) %>%
    left_join(msu.heatmap.100, by="pct") %>%
    rename("Qcompared.ints"=.data[[Qcompared]]) %>%
    mutate(hex=case_when(hex=="#E7ECEB"~"#ffffff",
                         TRUE~as.character(hex)))

  options(dplyr.summarise.inform=TRUE)
  return(QvQ.data)
}


#' @title Likert (and Likert-Like) Pairwise Analysis Plot
#'
#' @description Graphical representation of pairwise interactions between Likert
#'   and Likert-like responses. Likert-like responses are survey questions where
#'   the respondent can only select one response.
#'
#' @param data `tibble` (or `data.frame`) of Likert and categorical data with
#'   responses in integer form.
#' @param value.range XXXXXXX
#' @param title string with the title for the plot; _e.g._, "Likert Pairwise Analysis"
#' @param questions vector of strings with shortened versions of the questions
#'   used to label the plots; _e.g._, c("Q1. Overall", "Q2. MSU Leadership Communication").
#'   Use `\n` to denoted line breaks (aka carriage returns).
#' @param footnotes vector of strings to represent what the numeric (integer)
#'   values represent. Use `\n` to denoted line breaks (aka carriage returns).
#'
#' @return [ggplot2::] object constructed via [cowplot::]
#' @export
#'
#' @importFrom cowplot plot_grid draw_label ggdraw
#' @importFrom ggplot2 theme labs scale_x_continuous element_blank element_text geom_label margin
#'
#' @examples
#' \dontrun{
#' title <- "Likert & Likert-like Comparison"
#' questions <- c("Q1. Do you like everything bagels?", "Q2. Do you drink coffee?")
#' footnotes <- c("Q1. 1='Not a lot', 2='Meh', 3='Very much';\nQ2. 1=Never, 2=Some times, 3='Always'")
#'
#' likert.plot.matrix(data=select(fs20.data, Q1, Q2),
#'                    title=title,
#'                    questions=questions,
#'                    footnotes=footnote)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
likert.plot.matrix <- function(data, value.range, title, questions, footnotes=NULL) {

  ## data information ----
  n.Qs <- ncol(data)
  Qs.idc <- seq_len(length.out=n.Qs)
  Qs.names <- colnames(data)
  # value.range <- range(data)

  ## build the plot matrix ----
  the.plot <- NULL
  for (curr.row in Qs.idc) {
    QoI.name <- Qs.names[curr.row]
    curr.row.plots <- NULL
    for (curr.col in Qs.idc) {
      Qcompared.name <- Qs.names[curr.col]
      curr.pane <- NULL
      ##_ build the plot of interest ----
      if (curr.col == curr.row) {
        curr.pane <- likert.barplot(data=data, QoI=QoI.name, value.range)  ## build the barplot
        pane.type <- "bar"
      } else {
        curr.pane <- likert.heatmap(data=data, QoI=QoI.name, Qcompared=Qcompared.name, value.range) ## build the heatmaps
        pane.type <- "heat"
      }
      ##__ add y-axis labels to the first plot ----
      if (curr.col == 1) {
        curr.pane <- curr.pane +
          labs(y=questions[curr.row]) +
          theme(axis.text.y=element_text(),
                axis.title.y=element_text(angle=90),
                axis.ticks=element_blank())
        if (pane.type == "bar") {
          curr.pane <- curr.pane +
            labs(y=questions[curr.row]) +
            scale_x_continuous(position="top") +
            theme(axis.text.y=element_text(),
                  axis.title.y=element_text(angle=90),
                  axis.text.x=element_text(colour="white"))
        }
      }
      ##__ add x-axis labels to the first row plots ----
      if ((curr.row == 1) & (pane.type == "heat")) {
        curr.pane <- curr.pane + scale_x_reverse(position="top") +
          # theme(axis.text.x=element_text(size = rel(1.5)),
          theme(axis.text.x=element_text(),
                axis.ticks=element_blank())
      }
      ##__ add x-axis labels to the last row plots ----
      if (curr.row == n.Qs) {
        # print(questions[curr.col])
        if (pane.type == "heat") {
        curr.pane <- curr.pane + scale_x_reverse(position="bottom") +
          labs(x=questions[curr.col]) +
          theme(axis.title.x=element_text(),
                axis.text.x=element_text(),
                axis.ticks=element_blank())
        }
        if (pane.type == "bar") {
          # curr.pane <- curr.pane + scale_x_continuous(position="bottom", name=" ") +
          curr.pane <- curr.pane + scale_x_continuous(position="bottom") +
            labs(x=questions[curr.col]) +
            theme(axis.title.x=element_text(),
                  axis.text.x=element_text(colour="white"),
                  axis.ticks.x=element_blank())
        }
      }
      ##_ build the row of plots ----
      if ( is.null(curr.row.plots) ) {
        curr.row.plots <- cowplot::plot_grid(curr.row.plots, curr.pane,
                                             nrow=1, rel_widths=c(0, 1))
      } else {
        rel.plot.width <- 1/curr.col
        curr.row.plots <- cowplot::plot_grid(curr.row.plots, curr.pane,
                                             nrow=1, rel_widths=c(1-rel.plot.width, rel.plot.width))
      }
    }
    ##_ add rows to the plot ----
    if ( is.null(the.plot) ) {
      the.plot <- cowplot::plot_grid(the.plot, curr.row.plots,
                                     nrow=2, rel_heights=c(0, 1))
    } else {
      rel.plot.height <- 1/curr.row
      the.plot <- cowplot::plot_grid(the.plot, curr.row.plots,
                                     nrow=2, rel_heights=c(1-rel.plot.height, rel.plot.height),
                                     axis="l")
    }
  }

  ## add the title ----
  plot.title <- cowplot::ggdraw() +
    cowplot::draw_label(title, fontface="bold",
               size=14, x=0, hjust=0) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin=margin(0, 0, 0, 7)
    )
  the.plot <- cowplot::plot_grid(plot.title, the.plot,
                                 ncol=1,
                                 rel_heights=c(0.05, 1)) # rel_heights values control vertical title margins

  ## add the footnote ----
  if ( !is.null(footnotes) ) {
    plot.footnote <- cowplot::ggdraw() +
      cowplot::draw_label(footnotes,
                          # fontface='bold',
                          size=10,
                          x=0,
                          hjust=0) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin=margin(0, 0, 0, 7)
      )
    the.plot <- cowplot::plot_grid(the.plot, plot.footnote,
                                   ncol=1,
                                   rel_heights=c(1, 0.05)) # rel_heights values control vertical title margins
  }

  ## return the plot ----
  return(the.plot)

}


## /\/\/\/\ FUNCTIONS ----
## \/\/\/\/ PROTOCOLS ----
#
# library(tidyverse)
# library(tidytext)
# library(ggwordcloud)
# library(tm)
# library(ggraph)
# library(igraph)
# library(cowplot)
# library(ggstar)
# library(GGally)
#
# load(file="./data-raw/theHUB_datasets.RData")
#
# short <- likert.general[rowSums(is.na.data.frame(likert.general)) == 0, c(4,5,8)]
#
# phrase2int.tb <- tibble::tibble(phrase=c("Extremely dissatisfied", "Somewhat dissatisfied", "Neither satisfied nor dissatisfied", "Somewhat satisfied", "Extremely satisfied"),
#                                 integer=c(1,2,3,4,5))
# phrase2int.online.tb <- tibble::tibble(phrase=c("No, this was my first experience in fully online instruction",
#                                                 "Yes, I've taken one or two fully online courses",
#                                                 "Yes, I've taken many fully online courses"),
#                                 integer=c(1,2,3))
# short.ints <- mutate(short,
#                      Q1.ints=convert.fromto(responses=Q1.overall.MSU, fromto.tb=phrase2int.tb, from="phrase", to="integer"),
#                      Q2.ints=convert.fromto(responses=Q2.overall.comm.MSU.leaders, fromto.tb=phrase2int.tb, from="phrase", to="integer"),
#                      Q5.ints=convert.fromto(responses=Q5.online.before, fromto.tb=phrase2int.online.tb, from="phrase", to="integer")
# )
#
#
# likert.ints <- select(short.ints, ends_with("ints"))
#
#
#
#
#
# ## pairwise likert analysis ----
#
#
# select(likert.general.ints, ends_with(".ints")) %>%
#   group_by(Q1.ints) %>%
#   summarise(blah=mean(Q2.ints))
#
#
#
# ## pairwise multi-response analysis ----
# compare.dataset(data=select(multi.general, starts_with("Q.")))
#
#
#
#
#
# ggplot(likes.ints, aes(x=Q.overall.MSU, y=Q.online.before, fill=..count..)) +
#   geom_bin2d()
#
# likert.count.long <- pivot_longer(likert.counts,
#              cols=starts_with("Q.overall"),
#              names_to="Questions",
#              values_to="Counts") %>%
#   arrange(Questions) %>%
#   group_by(Questions) %>%
#   mutate(Total.Replies=sum(Counts)) %>%
#   ungroup() %>%
#   mutate(Ratios=Counts/Total.Replies,
#          Percents=Ratios*100,
#          Labels=paste0(round(Percents, digits=0), "%"))
#
# ggpairs(likert.ints,
#         columns=c("Q1.ints", "Q2.ints", "Q5.ints"))
#
#
#
# yad <- group_by(likes.ints, Q.overall.MSU, Q.overall.comm.MSU.leaders, Q.online.before) %>%
#   summarise(n=n()) %>%
#   pivot_longer(cols=Q.overall.comm.MSU.leaders, names_to="question", values_to="response") %>%
#   group_by(Q.overall.MSU) %>%
#   mutate(Q1.resp.n=sum(n)) %>%
#   ungroup() %>%
#   group_by(Q.online.before) %>%
#   mutate(Q5.resp.tot=sum(n))
#
#
# # group_by(likes, Q.overall.MSU, Q.overall.comm.MSU.leaders, Q.overall.comm.instr, Q.overall.train.support.OL, Q.online.before) %>%
# response.summary <- group_by(likes.ints, Q.overall.MSU, Q.overall.comm.MSU.leaders, Q.online.before) %>%
#   summarise(n=n()) %>%
#   pivot_wider(names_from=Q.online.before, values_from=n, values_fill=0, names_prefix="r.") %>%
#   ungroup()
#
# # response.summary <- group_by(likes.ints, Q.overall.MSU, Q.online.before) %>%
# group_by(short, select(short, everything())) %>%
#   summarise(n=n()) %>%
#   pivot_longer(cols=starts_with("Q"), names_to="question", values_to="response")
#
#
# response.colSums <- colSums(select(response.summary, starts_with("r.")))
# response.rowSums <- rowSums(select(response.summary, starts_with("r.")))
#
# select(response.summary, starts_with("r."))
