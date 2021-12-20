


#' @title Get Operating System Name
#'
#' @description Return the _generic_ name of the operating system.
#'
#' @param OperatingSystem A string containing the operating system obtained by
#'   a Qualtrics survey; _e.g._, "Windows NT 4.0 SP6a"
#'
#' @return string with the _generic_ operating system name; _e.g._, "Windows"
#' @export
#'
#' @examples
#' clean.OStype("Windows NT 10.0")
#' # "Windows"
#'
#' clean.OStype("Windows NT 4.0 SP6a")
#' # "Windows"
#'
#' clean.OStype("CrOS x86_64 13505.63.0")
#' # "CrOS"
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
clean.OStype <- function(OperatingSystem) {

  clean.OS <- unlist(strsplit(x=OperatingSystem, split=" "))[1]

  return(clean.OS)
}


#' @title Get Main Version Number
#'
#' @description Returns the main version number.
#'
#' @param Version.number A string with the version number; _e.g._, "10.15.7".
#'   Because version numbers can have multiple points, they are not considered
#'   numbers within `R` and thus they are strings.
#'
#' @return integer of the main version number
#' @export
#'
#' @examples
#' clean.OSversion("10.15.7")
#' # 10
#'
#' clean.OSversion("11.3")
#' # 11
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
clean.OSversion <- function(Version.number) {

  clean.ver <- unlist(strsplit(x=Version.number, split="\\."))[1]
  clean.ver <- as.integer(clean.ver)

  return(clean.ver)
}


#' @title Clean Year of Birth
#'
#' @param YoB string with date of birth-like data information
#'
#' @return
#' @export
#'
#' @examples
#' clean.YoB("1969")
#' # 1969
#'
#' clean.YoB("01/23/1969")
#' # 1969
#'
#' clean.YoB("01/23/69")
#' # 1969
#'
#' clean.YoB("23 January 1969")
#' # 1969
#'
#' clean.YoB("19690123")
#' # 1969
#'
#' clean.YoB("69")
#' # 1969
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
clean.YoB <- function(YoB) {

  ## extract and clean the year of birth from provided date/information ----
  YoB.extracted <- suppressWarnings(lubridate::parse_date_time(x=YoB, c("y", "mdy", "ymd", "dmy")))
  YoB.cleaned <- as.integer(lubridate::year(YoB.extracted))

  ## return cleaned year of birth ----
  return(YoB.cleaned)
}


#' @title Clean Date of Birth
#'
#' @param DoB string with date of birth-like data information
#'
#' @return
#' @export
#'
#' @examples
#' clean.DoB("03301995")
#' # "1995-03-30"
#'
#' clean.DoB("03/30/1995")
#' # "1995-03-30"
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
clean.DoB <- function(DoB) {

  ## extract and clean the year of birth from provided date/information ----
  DoB.extracted <- suppressWarnings(lubridate::parse_date_time(x=DoB, c("mdy", "ymd", "dmy")))
  DoB.cleaned <- lubridate::as_date(DoB.extracted)

  ## return cleaned year of birth ----
  return(DoB.cleaned)
}





#' @title Convert to ASCII
#' @description Converts non-local characters to ASCII
#' @details Convert _**latin1**_ encoded characters to ASCII
#'
#' @param comment A string of words forming a sentence or phrase.
#'
#' @return string of cleaned characters forming words
#' @export
#'
#' @examples
#' comment <- "All day I see class mates and send e-mails.   I miss East Lansing."
#'
#' convert.toASCII(comment=comment)
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
convert.toASCII <- function(comment) {

  comment.clean <- iconv(x=comment, from="latin1", to="ASCII", sub="")

  return(comment.clean)
}


#' @title Convert Four-Digit Term Codes
#' @description Converts four-digit term codes to semester or quarter names with
#'   four-digit years.
#' @details Within CampusSolutions the term code is a four digit representation
#'   of the century (position 1), the two-digit calendar year (positions 2 and 3),
#'   and the term (position 4). The following is the translation between digits
#'   and four-digit years and term types.
#'
#'   | **Position** | **Information** |
#'   |:--------:|:-----------:|
#'   |  1  |  Century Code |
#'   |     |  (1 = 19xx & 2 = 20xx) |
#'   |  2 & 3  |  Two-Digit Calendar Year |
#'   |  4  |  Term Indicator (see below) |
#'
#'   The information in the following table is available in the constant
#'   [term.translation].
#'
#'   | **Term abbreviation** | **Full Term Name** | **Short Term Name** |
#'   |:-------------------:|:-------------:|:--------------:|
#'   |   1  | Winter Quarter (WinterQ) |  WQ  |
#'   |   2  | Spring | SS |
#'   |   3  | Spring Quarter (SpringQ) | SQ  |
#'   |   5  | Summer | US |
#'   |   6  | Summer Quarter | UQ |
#'   |   8  | Fall   | FS |
#'   |   9  | Fall Quarter (FallQ) | FQ |
#'
#'
#' @param term.code Four-digit code indicating the semester and year of the term.
#' @param term.type Indicate if the full or short semester (or quarter) designation
#'   is returned; default: `"full"`.
#'
#' @return string term and four-digit year
#' @export
#'
#' @examples
#' term.code <- "2208"
#'
#' convert.termCode(term.code, term.type="full")
#' # [1] "Fall 2020"
#'
#' convert.termCode(term.code, term.type="short")
#' # [1] "FS 2020"
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
convert.termCode <- function(term.code, term.type="full") {

  century <- substr(x=term.code, start=1, stop=1)
  year <- substr(x=term.code, start=2, stop=3)
  if (century == "1") {
    year <- paste0("19", year)
  } else {
    year <- paste0("20", year)
  }

  term <- substr(x=term.code, start=4, stop=4)
  if (tolower(term.type) == "full") {
    term <- term.translation$full[term.translation$abbrev == term]
  }
  if (tolower(term.type) == "short") {
    term <- term.translation$short[term.translation$abbrev == term]
  }

  termcode.readable <- paste(term, year, sep=" ")

  return(termcode.readable)

}


#' @title Honours Course?
#'
#' @description Determine if a course is an honours course based on the course
#'   code (`crse_code`). The fourth character is an "H". This designation is
#'   based on the ["Definitions of Course Characteristics"](https://reg.msu.edu/Read/UCC/Courselistings.pdf)
#'   document provided on the Office of the Registrar's website.
#'
#' @param crse_code single character vector with the course code of interest.
#'
#' @return logical
#' @export
#'
#' @examples
#' crse_code <- "183H"
#' is.honours(crse_code)
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
is.honours <- function(crse_code) {
  honours.tf <- FALSE

  characters <- unlist(strsplit(x=crse_code, split=""))
  n.characters <- length(characters)
  if ((n.characters == 4) & (characters[4] == "H")) {
    honours.tf <- TRUE
  }

  return(honours.tf)
}


#' @title Course Prep?
#'
#' @description Determine if a course is a preparation, aka prep, course based
#'   on the course code (`crse_code`). The fourth character, in this case a
#'   digit is five (5) or less. This designation is based on the
#'   ["Definitions of Course Characteristics"](https://reg.msu.edu/Read/UCC/Courselistings.pdf)
#'   document provided on the Office of the Registrar's website.
#'
#' @param crse_code single character vector with the course code of interest.
#'
#' @return logical
#' @export
#'
#' @examples
#' crse_code <- "1831"
#' is.prep(crse_code)
#' # TRUE
#'
#' crse_code <- "1838"
#' is.prep(crse_code)
#' # FALSE
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
is.prep <- function(crse_code) {
  prep.tf <- FALSE

  characters <- unlist(strsplit(x=crse_code, split=""))
  n.characters <- length(characters)
  if ((n.characters == 4) & (characters[4] <= 5)) {
    prep.tf <- TRUE
  }

  return(prep.tf)
}


#' @title Determine the Course Type
#'
#' @description Determine if a course is a preparation (aka prep), honours (H),
#'   multi-diversity (I), national diversity (N), international and multicultural
#'   diversity (D), or writing (W) course based
#'   on the course code (`crse_code`). Theese designations are based on the
#'   ["Definitions of Course Characteristics"](https://reg.msu.edu/Read/UCC/Courselistings.pdf)
#'   document provided on the Office of the Registrar's website.
#'
#' @param crse_code single character vector with the course code of interest.
#'
#' @return logical
#' @export
#'
#' @examples
#' crse_code <- "1831"
#' course.type(crse_code)
#' # "prep"
#'
#' crse_code <- "1838"
#' is.prep(crse_code)
#' # ""
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
course.type <- function(crse_code) {
  course.type <- ""

  # characters <- unlist(strsplit(x=course.type, split=""))
  characters <- unlist(strsplit(x=crse_code, split=""))
  n.characters <- length(characters)
  character.4 <- characters[4]
  if ((n.characters == 4) & (character.4 <= 5)) { course.type <- "prep" }
  if (character.4 == "H") { course.type <- "honours"}
  if (character.4 == "I") { course.type <- "multi.diversity"}
  if (character.4 == "N") { course.type <- "nat.diversity"}
  if (character.4 == "D") { course.type <- "imn.diversity"}
  if (character.4 == "W") { course.type <- "writing"}

  return(course.type)
}



#' @title Make Course-Section Designation
#'
#' @description Constructs the course-section designation to match the course-section
#'   description in the `crsesect_firstday`, `crsesect_qrtrterm`, and `crsesect_endterm`
#'   columns/fields
#'
#' @param course string containing the course designation; a combination of
#'   `subj_code` and `crse_code`. For example, `"PKG 491"`.
#' @param sctn_code string (or integer) with the section code.
#'
#' @return string with the subject code, course code, and section code to form
#'   a course-section designation in the same format as the `crsesect_firstday`,
#'   `crsesect_qrtrterm`, and `crsesect_endterm` columns/fields.
#' @export
#'
#' @examples
#' course <- "PKG 491"
#' sctn_code <- "6"
#'
#' make.crsesect(course, sctn_code)
#' # "PKG 491-006"
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.crsesect <- function(course, sctn_code) {

  course.section <- paste(course, formatC(x=sctn_code, width=3, flag="0"), sep="-")

  return(course.section)

}


#' Bin Values
#'
#' @description Bin and name a collection of values.
#'
#' @details Commonly used to bin ages and survey responses that are continuous;
#'   _e.g._, `1, 3, 1, 2, 5, 4`. Values that are not bin-able return a `"Not Provided`"
#'   classification.
#'
#' @param x vector of (likely) integer values representing age
#' @param bins vector of bins for use with [base::findInterval()]
#' @param bin.names vector of bin names. _**NEEDS**_ to be the same
#'   length as `bins`
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(13)
#' age <- c(NA, sample(1:10, 20, replace=TRUE), NA)
#' age.bins <- c(0, 2, 4, 6, 8, 10)
#' age.bin.names <- c("0", "2-3", "4-5", "6-7", "8-9", "10")
#' make.bins(x=age, bins=age.bins, bin.names=age.bin.names)
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
make.bins <- function(x, bins, bin.names) {

  binning <- findInterval(x=x, vec=bins)
  ranges <- bin.names[binning]
  ranges[is.na(ranges)] <- "Not Provided"

  ## return ranges ----
  return(ranges)
}




#' @title File Name Date and Time
#'
#' @description Construct date and time components for a file name. Ideally, this
#'   function is called when the script (or main function) is initiated.
#'
#'   - `file.date()`: returns the date in "%b%d%Y" format; _e.g._, "Jan232021"
#'   - `file.time()`: returns the time in "%H%M" format; _e.g._, "1453"
#'   - `file.datetime()`: returns the date and time in the "%b%d%Y_%H%M" format;
#'      _e.g._, "Jan232021_1453"
#'
#' @param date.time string extracted using [base::Sys.time()] or passing `NULL`
#'   will result in the time the `Sys.time()` function is called being used.
#'
#'   _**NOTE**_: Do _**NOT**_ pass `NULL` to the individual date and time
#'   functions. Instead, call `file.datetime()` and split the string using
#'   [base::strsplit()] in the form `strsplit(x=file.datetime(), split="_")`
#'   to obtain the date and time.
#'
#'
#' @return string with the date, time, or date_time in the format noted above.
#'
#' - For `file.date()`, the date "Jan232021"
#' - For `file.time()`, the time "1453"
#' - For `file.datetime()`, the date and time "Jan232021_1453"
#'
#' @family filenames
#' @examples
#' date.time <- as.POSIXlt("2021-01-23 14:53:13 EST")
#'
#' file.date(date.time=date.time)
#' # "Jan232021"
#'
#' file.time(date.time=date.time)
#' # "1453"
#'
#' file.datetime(date.time=date.time)
#' # "Jan232021_1453"
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
#' @name filename-times
NULL

#' @rdname filename-times
#' @export
file.datetime <- function(date.time=NULL) {

  ## if no date.time is provided, use current Sys.time() ----
  if ( is.null(date.time) ) {
    date.time <- Sys.time()
  }

  ## construct the date_time ----
  date.mdy <- file.date(date.time=date.time)
  date.hm <- file.time(date.time=date.time)
  date.mdy_hm <- paste(date.mdy, date.hm, sep="_")

  ## return date_time ----
  return(date.mdy_hm)
}

#' @rdname filename-times
#' @export
file.date <- function(date.time=NULL) {

  ## if no date.time is provided, use current Sys.time() ----
  if ( is.null(date.time) ) {
    date.time <- Sys.time()
  }

  ## extract and format the date ----
  date.mdy <- format(date.time, "%b%d%Y")

  ## return date ----
  return(date.mdy)
}

#' @rdname filename-times
#' @export
file.time <- function(date.time=NULL) {

  ## if no date.time is provided, use current Sys.time() ----
  if ( is.null(date.time) ) {
    date.time <- Sys.time()
  }

  ## extract and format the time ----
  date.hm <- format(date.time, "%H%M")

  ## return time ----
  return(date.hm)
}


