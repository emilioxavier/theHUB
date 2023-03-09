
#' @title Tanimoto Coefficient/Index
#'
#' @description Calculate the Tanimoto coefficient/index (also known as the Jaccard
#'   index). A value of `0` indicates **no similarity** and a value of `1` indicates
#'   **perfect similarity**.
#'
#' @details Either two `TRUE`/`FALSE` vectors or two `integer` vectors can be
#'   provided. The `TRUE`/`FALSE` vectors _**must be the same lengths**_ while
#'   the integer vectors only have to indicate indices of interest in each
#'   vector; see examples.
#'
#'   The Tanimoto coefficient is an indication of how similar two samples are to
#'   each other based on number of bits overlapping (in common) in relationship
#'   to the number of unique bits present in both samples.
#'
#' @param set1 vector of `TRUE`/`FALSE` values OR vector of integers. See **Details above**.
#' @param set2 vector of `TRUE`/`FALSE` values OR vector of integers. See **Details above**.
#'
#' @return numerical value indicating the Tanimoto coefficient
#' @export
#'
#' @examples
#'   set1 <- c(TRUE,TRUE,FALSE,FALSE,FALSE)
#'   set2 <- c(TRUE,FALSE,TRUE,TRUE,TRUE)
#'   Tanimoto.Idx(set1, set2)
#'   # 0.2
#'
#'   set1 <- which(set1)
#'   set1
#'   # 1 2
#'   set2 <- which(set2)
#'   set2
#'   # 1 3 4 5
#'   Tanimoto.Idx(set1, set2)
#'   # 0.2
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
Tanimoto.Idx <- function(set1, set2) {

  if ( is.logical(set1) & is.logical(set2) ) {

    if ( length(set1) != length(set2) ) {
      stop("The T/F vectors need to be the same length")
    }

    ab.Intersection <- sum((set1 + set2) == 2)
    ab.Union <- sum( (set1 + set2) > 0 )

    tanimoto.idx <- ab.Intersection/ab.Union
  } else {
    # message("in here?")
    a <- length(set1)
    b <- length(set2)
    ab <- sum(set1 %in% set2)
    # tanimoto <- ab / (a + b - ab)  ## as written in books
    tanimoto.idx <- ab * (a + b - ab)^-1
  }

  return(tanimoto.idx)
}


#' @title Tanimoto Distance
#'
#' @description Calculate the Tanimoto distance (also known as the Jaccard
#'   index). A value of `1` indicates **no similarity** and a value of `0` indicates
#'   **perfect similarity**.
#'
#' @details Either two `TRUE`/`FALSE` vectors or two `integer` vectors can be
#'   provided. The `TRUE`/`FALSE` vectors _**must be the same lengths**_ while
#'   the integer vectors only have to indicate indices of interest in each
#'   vector; see examples.
#'
#'   The Tanimoto coefficient is an indication of how similar two samples are to
#'   each other based on number of bits overlapping (in common) in relationship
#'   to the number of unique bits present in both samples. In this case, the
#'   Tanimoto Distance is a measure of dissimilarity.
#'
#' @param set1 vector of `TRUE`/`FALSE` values OR vector of integers. See **Details above**.
#' @param set2 vector of `TRUE`/`FALSE` values OR vector of integers. See **Details above**.
#'
#' @return numerical value indicating the Tanimoto distance
#' @export
#'
#' @examples
#'   set1 <- c(TRUE,TRUE,FALSE,FALSE,FALSE)
#'   set2 <- c(TRUE,FALSE,TRUE,TRUE,TRUE)
#'   Tanimoto.Dist(set1, set2)
#'   # 0.8
#'
#'   set1 <- which(set1)
#'   set1
#'   # 1 2
#'   set2 <- which(set2)
#'   set2
#'   # 1 3 4 5
#'   Tanimoto.Dist(set1, set2)
#'   # 0.8
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
Tanimoto.Dist <- function(set1, set2) {

  tanimoto.dist <- 1- Tanimoto.Idx(set1, set2)

  return(tanimoto.dist)
}


#' @title Compare Datasets
#'
#' @description Calculates the amount of similarity (or dissimilarity based on
#'   how you are looking at the data) within a dataset.
#'
#' @details The number and ratio (aka fraction) of overlapping samples within
#'   each area of interest along with the Tanimoto coefficient/index are
#'   calculated. The returned Tanimoto coefficients/indice matrices are symmetrical
#'   around the diagonal, but the count and ratio matrices are not. _**They can
#'   be interpreted two different ways.**_
#'
#'   **Count and Ratio Matrices**
#'   - The diagonal of the count and ratio matrices contain the number of
#'     samples containing the area of interest.
#'   - Within each column, the returned counts or ratio values indicate the number (ratio)
#'     of samples within each area that contains both areas (the column and the
#'     row). The returned column values are always to be compared to the column
#'     of interest. The returned count will _**never**_ be more than the diagonal
#'     value of the column.
#'   - Each row, especially for the ratio matrices, should be interpreted as
#'     the similarity of each column to each other. For example, the ratios within
#'     a row
#'   - Within each row, the returned counts or ratio values indicate the number (ratio)
#'     of samples within each area that contains both areas (the column and the
#'     row).
#'
#' @param data `tibble` (or `data.frame`) to be compared
#'
#' @return three matrices are returned.
#'   - **Tanimoto.Indices**: pairwise Tanimoto coefficients for all samples
#'   - **Overlap.Counts**: number of overlapping samples for each area of interest.
#'       Must be read column-wise because the number of counts is for each row
#'       area within the column.
#'   - **Overlap.Ratios**: ratio (aka fraction) of number of overlapping samples
#'       for each area of interest. Must be read column-wise because the ratio
#'       is for each row area within the column.
#'
#' @importFrom dplyr pull
#' @export
#'
#' @examples
#' \dontrun{
#' compare.dataset(data)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
compare.dataset <- function(data) {

  ## get general information ----
  col.names <- names(data)
  n.cols <- length(col.names)

  ## make data storage matrices ----
  col.idc <- seq_len(length.out=n.cols)
  sim.mat <- ratio.mat <- count.mat <- matrix(data=NA, nrow=n.cols, ncol=n.cols)
  ##_ add the row and column names ----
  rownames(sim.mat) <- colnames(sim.mat) <- col.names
  rownames(ratio.mat) <- colnames(ratio.mat) <- col.names
  rownames(count.mat) <- colnames(count.mat) <- col.names

  ## calculate counts and percentages ----
  for (col.idx in col.idc) {
    col.oi <- col.names[col.idx]
    col.oi.tf <- data[[col.oi]]
    count <- colSums(data[col.oi.tf, ])
    count.mat[col.idx, ] <- count
    ratio.mat[col.idx, ] <- count/count[[col.oi]]
  }

  ## calculate Tanimoto/Jaccard similarity ----
  for (col.idx in col.idc) {
    set.oi <- dplyr::pull(data, {{col.idx}})

    for (col.curr in col.idc) {
      set.curr <- dplyr::pull(data, {{col.curr}})

      sim.mat[col.curr, col.idx] <- (Tanimoto.Idx(set1=set.oi, set2=set.curr))
    }
  }

  ## return data ----
  list(Tanimoto.Indices=sim.mat,
       Overlap.Counts=count.mat,
       Overlap.Ratios=ratio.mat)
}


#' @title Make Fingerprints
#'
#' @description Construct fingerprints for a collection of samples from the provided
#'   integer and float values.
#'
#' @param data data.frame or tibble containing raw data to convert into fingerprints
#' @param id.col string indicating the identifying column
#' @param cols.oi vector of integers or strings indicting the columns of interest
#'
#' @return tibble of fingerprints with integer (count) or float values
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   where2eat <- tibble::tibble(what=c("BBQ", "Burgers", "Pizza"),
#'                               loc=c(1,2,3),
#'                               size=c(4,2,6),
#'                               cost=c(10,9,8))
#'   make.fps(data=where2eat, id.col="what", cols.oi=c("loc", "size", "cost"))
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.fps <- function(data, id.col, cols.oi) {

  fps <- tidyr::pivot_longer(data, cols=cols.oi, names_to="name", values_to="value") |>
    arrange(name, value) |>
    dplyr::mutate(bit=TRUE,
                  col.name=paste(name, value, sep="_")) |>
    tidyr::pivot_wider(id_cols={id.col}, names_from=col.name, values_from=value, values_fill=0)

  return(blah.fps)
}


#' @title Convert Integer Fingerprints into Binary Representation
#'
#' @description Convert integer fingerprints into binary (logical) representation.
#'
#' @param data data.frame or tibble with fingerprint values
#' @param id.col string indicating the identifying column
#'
#' @return tibble of fingerprints with logical (binary) values
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(tidyverse)
#'   where2eat <- tibble::tibble(what=c("BBQ", "Burgers", "Pizza"),
#'                               loc=c(1,2,3),
#'                               size=c(4,2,6),
#'                               cost=c(10,9,8))
#'   make.fps(data=where2eat, id.col="what", cols.oi=c("loc", "size", "cost")) |>
#'     convert.fps2binary()
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
convert.fps2binary <- function(data, id.col) {
  fps.tf <- mutate(data, across(where(is.numeric), ~as.logical(.x)))

  return(fps.tf)
}


#





