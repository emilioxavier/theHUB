
#' @title Extract Variance Explained (PCA)
#'
#' @description
#'
#' @export
#' @return
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
extract.VarExplain <- function(PCAmodel, nDigits.label=1) {

  ## extract standard deviation values ----
  sdev <- PCAmodel$sdev

  ## construct tibble ----
  VarExpl <- tibble::tibble(PC=seq_len(length.out=length(sdev)),
                            PC.num=paste0("PC", PC),
                            stDev=sdev,
                            VarExpl=stDev^2,
                            VarExpl.fct=VarExpl/sum(VarExpl),
                            VarExpl.tot.fct=cumsum(VarExpl.fct),
                            VarExpl.pct=VarExpl.fct*100,
                            VarExpl.tot.pct=cumsum(VarExpl.pct),
                            VarExpl.label.pct=paste0(round(VarExpl.pct, digits=nDigits.label), "%"),
                            VarExpl.label.PCpct=paste0("PC", PC, " (", round(VarExpl.pct, digits=nDigits.label), "%)"),
                            VarExpl.label.scree=paste0(round(VarExpl.pct, digits=nDigits.label), "% (", round(VarExpl.tot.pct, digits=nDigits.label), "%)")
  )

  ## return ----
  return(VarExpl)

}

#' @title Set Minimum Value Negative - Set Maximum Value Positive
#'
#' @description
#'
#' @export
#' @return
#'
## taken from base-R's biplot.R
negMin.posMax.range <- function(x) {
  c(-abs(min(x, na.rm=TRUE)), abs(max(x, na.rm=TRUE)))
}


#' @title Make PCA Plot Data (`prcomp`)
#'
#' @description
#'
#' @export
#' @return
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.PCAplot.DATA <- function(PCAmodel, PCs.oi, scale=1, expand=1) {

  ## checks ----
  if(inherits(PCAmodel, "prcomp")) message("Using prcomp!! :D") else stop("Please use prcomp to perform your PCAnalysis.")
  if(length(PCs.oi) != 2L) stop("Please provide two (2) PCs.oi!")
  if(scale<0 || scale>1) warning("The provided 'scale' value is outsid [0,1].")

  ## extract variance explained ----
  PCs.oi <- as.integer(PCs.oi) |> sort()
  VarExplained <- extract.VarExplain(PCAmodel=PCAmodel, nDigits.label=1)
  VarExpl.PCs.oi <- dplyr::filter(VarExplained, PC %in% PCs.oi)

  ## extract tables ----
  scores <- tibble::as_tibble(PCAmodel$x)
  loadings <- as.data.frame(PCAmodel$rotation) |>
    rownames_to_column(var="feature") |>
    tibble::as_tibble()
  n.obs <- nrow(scores)

  ## lambda values ----
  lambda <- PCAmodel$sdev[PCs.oi]
  # lambda <- lambda * sqrt(n.obs)
  if(scale!=0) lambda <- lambda^scale else lam <- 1

  ## scale scores ----
  scores.lambda <- t(t(scores[, PCs.oi]) / lambda) |>
    tibble::as_tibble()

  ## scale loadings ----
  loadings.lambda <- t(t(loadings[, PCs.oi+1]) * lambda) |>
    tibble::as_tibble()

  ##_ find optimal scaling value ----
  scores.range.1 <- negMin.posMax.range(scores.lambda[, 1L])
  scores.range.2 <- negMin.posMax.range(scores.lambda[, 2L])
  scores.range <- range(scores.range.1, scores.range.2)
  loadings.range.1 <- negMin.posMax.range(loadings.lambda[, 1L])
  loadings.range.2 <- negMin.posMax.range(loadings.lambda[, 2L])
  arrow.ratio <- max(loadings.range.1/scores.range, loadings.range.2/scores.range)/expand

  ##_ final scaling of loading values ----
  loadings.lambda <- (loadings.lambda / arrow.ratio) |>
    tibble::as_tibble() |>
    tibble::add_column(feature=loadings$feature, .before=1)


  ## return data ----
  return(list(biplot.scores=scores.lambda,
              biplot.loadings=loadings.lambda,
              VarExplained=VarExplained,
              scores=scores,
              loadings=loadings))
}

