
# grades <- make.pseudo.grades(assignment.params)
# crossover.pseudo.grades(grades=grades, crossover.point=5)
# crossover.pseudo.grades(grades=grades, crossover.point="random")

#' @title Construct Assignment Parameters
#'
#' @description add the description of making the data
#'
#' @param n.students number of students in the course; default: 15
#' @param n.assignments number of assignments or assessments; default: 10
#' @param max.points maximum number of points for all assignments; default: 25
#'
#' @return `tibble` with the input parameters for make.pseudo.grades
#' @export
#' @importFrom tibble tibble
#'
#' @examples
#' assignment.params <- make.assignment.params(n.students=5,
#'                                             n.assignments=5,
#'                                             max.points=25)
#' # # A tibble: 5 × 3
#' # n  size  prob
#' # <dbl> <dbl> <dbl>
#' # 1     5    25  0
#' # 2     5    25  0.25
#' # 3     5    25  0.5
#' # 4     5    25  0.75
#' # 5     5    25  1
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.assignment.params <- function(n.students, n.assignments, max.points) {

  prob.width <- 1 / (n.students - 1)
  probs <- seq(0, 1, prob.width)

  assign.params <- tibble::tibble(n=n.assignments,
                                  size=max.points,
                                  prob=probs)

  ## return results ----
  return(assign.params)

}


#' @title Construct Pseudo-Grades
#'
#' @description Constructs pseudo-grades based on the assignment parameters
#'   constructed by [make.assignment.params()]. Each row is a _unique student_
#'   with scores for requested number of assignments. The pseudo-grades are
#'   assigned using the [stats::rbinom()] function.
#'
#' @param assignment.params tibble of grade distributions constructed by
#'   [make.assignment.params()].
#'
#' @return `tibble` with the pseudo-grades
#' @export
#' @importFrom dplyr mutate
#' @importFrom tibble as_tibble
#' @importFrom stats rbinom
#'
#' @examples
#' set.seed(13)
#' assignment.params <- make.assignment.params(n.students=5,
#'                                             n.assignments=5,
#'                                             max.points=25)
#' make.pseudo.grades(assignment.params)
#' # # A tibble: 5 × 6
#' #   student   assign.1 assign.2 assign.3 assign.4 assign.5
#' #  <chr>        <int>    <int>    <int>    <int>    <int>
#' # 1 student.1        0        0        0        0        0
#' # 2 student.2        7        5        6        3       10
#' # 3 student.3        7       13       14       15        8
#' # 4 student.4       18       16       16       18       18
#' # 5 student.5       25       25       25       25       25
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.pseudo.grades <- function(assignment.params) {

  ## number of students and assignments ----
  n.students <- nrow(assignment.params)
  n.grades <- assignment.params$n[1]

  ## create student and assignment names ----
  student.names <- paste("student",
                         formatC(x=seq_len(length.out=n.students),
                                 width=nchar(n.students),
                                 format="d",
                                 flag="0"),
                         sep=".")
  assignment.names <- paste("assign",
                            formatC(x=seq_len(length.out=n.grades),
                                    width=nchar(n.grades),
                                    format="d",
                                    flag="0"),
                            sep=".")

  ## construct grades ----
  grades <- suppressMessages(purrr::pmap_dfc(assignment.params, rbinom)) |>
    as.data.frame() |>
    t() |>
    tibble::as_tibble(.name_repair="minimal")
  ##_ add assignment names ----
  colnames(grades) <- assignment.names
  ##_ add student names ----
  grades <- dplyr::mutate(grades, student=student.names, .before=1)

  ## crossover the grades ----
  # grades.co <- crossover.pseudo.grades(grades=grades, crossover.point="random")

  ## return grades ----
  return(grades)

}


#' @title Crossover Pseudo-Grade
#'
#' @description add the description of making the data
#'
#' @param grades data.frame with the pseudo-grades
#' @param crossover.point integer indicating the assignment crossover point
#'
#' @return data.frame with the crossovered (or is it crossed-over) grades
#' @export
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#'  crossover.pseudo.grades(grades=grades, crossover.point=5)
#'  crossover.pseudo.grades(grades=grades, crossover.point="random")
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
crossover.pseudo.grades <- function(grades, crossover.point="random") {

  ## get student names ----
  student.names <- paste0(grades$student, ".co")

  ## only the grades ----
  grades.orig <- grades
  grades.co <- grades.2co <- grades[, -1]
  grades.co <- as.data.frame(grades.co)

  ## the parents ----
  rows.n <- nrow(grades.2co)
  rows.idc <- seq_len(length.out=rows.n)
  rows.even.tf <- rows.n %% 2 == 0
  rows.half <- pairs.n <- floor(rows.n/2)
  rows.half.idc <- seq_len(length.out=rows.half)
  rows.p1 <- rows.idc[rows.half.idc]
  rows.p2 <- rev(rows.idc)[rows.half.idc]

  ## the genes ----
  cols.n <- ncol(grades.2co)

  ## crossover point -----
  swap.idc <- seq_len(length.out=cols.n)
  ##_ crossover.type checks ----
  if (is.null(crossover.point)) {
    swap.idx <- crossover.point <- floor(cols.n/2)
    crossover.type <- "fixed"
  }
  if (is.character(crossover.point) & crossover.point=="random") {
    crossover.type <- "random"
    message(paste0("the potential crossover points are ", min(swap.idc), " to ", max(swap.idc)))
  }
  if (is.character(crossover.point) & crossover.point!="random") {
    crossover.type <- "random"
    message(paste0("the potential crossover points are ", min(swap.idc), " to ", max(swap.idc)))
  }
  if (crossover.point <= min(swap.idc) || crossover.point >= max(swap.idc)) {
    swap.idx <- floor(cols.n/2)
  }
  # if (is.numeric(crossover.point) || crossover.point <= min(swap.idc) || crossover.point >= max(swap.idc)) {
  if (is.numeric(crossover.point) & crossover.point>=min(swap.idc) & crossover.point<=max(swap.idc)) {
    crossover.type <- "fixed"
    swap.idx <- crossover.point
    message(paste0("the crossover points is ", swap.idx))
  }

  if (crossover.type=="random") {
    buffer <- 2
    swap.idc <- swap.idc[buffer:(cols.n-buffer+1)]
    message(paste0("the swap indices are: ", paste(swap.idc, collapse=", ")))
  }

  ## crossover the grades ----
  for (curr.pair in rows.half.idc) {
    if (crossover.type == "random") {
      swap.idx <- sample(swap.idc, size=1)
      message(paste0(" the crossover point is: ", swap.idx))
    }

    p1.idc <- rows.p1[curr.pair]
    p2.idc <- rows.p2[curr.pair]
    message(paste0(" the current parents are: ", student.names[p1.idc], " and ", student.names[p2.idc]))

    p1.values <- as.vector(unlist(grades.2co[p1.idc, ]))
    p2.values <- as.vector(unlist(grades.2co[p2.idc, ]))

    half1.idc <- c(1:swap.idx)
    half2.idc <- c((swap.idx+1):cols.n)

    grades.co[p1.idc, ] <- c(p1.values[half1.idc], p2.values[half2.idc])
    grades.co[p2.idc, ] <- c(p2.values[half1.idc], p1.values[half2.idc])
  }

  ## add the student names ----
  grades.co <- dplyr::mutate(grades.co, student=student.names, .before=1)

  ## return crossover grades ----
  return(grades.co)

}

