#' @title Make Canonical Name
#'
#' @description Often there is a need to construct a name from several different
#'   components. While a common method to construct these names from the provided
#'   components in the provided order, it can lead to different names for the
#'   same thing.
#'
#'   If the name for an object is composed from two or
#'   more columns there is the possibility that the components change order within
#'   the columns. In some cases the name "yaddy-blah" is the same as "blah-yaddy."
#'   This function takes the provided components, replaces `NA`s with "Unk",
#'   orders the components alphabetically and in numerical order, and then
#'   combines the components with "-"s.
#'
#' @param name.vector A string containing the components to comprise the
#'   canonical name.
#'
#' @return string with the canonical name
#' @export
#' @importFrom tidyr replace_na
#'
#' @examples
#' canonical.name(c("yaddy", "yaddy", "blah", "ugh"))
#'
#' \dontrun{
#'   ## the following is how to implement the `canonical.name()` via `dplyr::mutate()`
#'   mutate(name=canonical.name(c(column.1, column.2)))
#'
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
canonical.name <- function(name.vector) {

  name.vector.clean <- tidyr::replace_na(name.vector, replace="Unk")
  name.vector.sorted <- sort(name.vector.clean)
  name.vector.canonical <- paste(name.vector.sorted, collapse="-")

  return(name.vector.canonical)
}


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
#' @return integer with the determined year of birth
#' @export
#' @importFrom lubridate as_date parse_date_time
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
#' @description This function uses the [lubridate::parse_date_time()] function to determine
#'   the date of birth by trying
#'   - Month - Day - Year
#'   - Year - Month - Day
#'   - Day - Month - Year
#'
#' @param DoB string with date of birth-like data information.
#'
#' @return date with determined date of birth
#' @export
#' @importFrom lubridate as_date parse_date_time
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


#' @title Clean Date
#'
#' @description This function expands [theHUB::clean.DoB()] function and uses the
#'   [lubridate::parse_date_time()] function to determine the provided date by trying
#'   - Month - Day - Year
#'   - Year - Month - Day
#'   - Day - Month - Year
#'   - Month - Year
#'   - Year -Month
#'
#' The function determines if the provided date contains month-day-year or month-
#' year information.
#'
#' **This function is currently not available. Need to add a logical parameter.**
#' A date with a year five-years before the current year or later
#' is reduced by 100-years. For example, if the resulting year is 2020 (this
#' description was written in 2022) the returned date has a year of 1920.
#'
#' @param dates string with date of birth-like data information.
#'
#' @return date
#' @export
#' @importFrom lubridate parse_date_time year dyears
#' @importFrom stringr str_count
#'
#' @examples
#' clean.DATE("03301995")
#' # "1995-03-30"
#'
#' clean.DATE("03/30/1995")
#' # "1995-03-30"
#'
#' clean.DATE("03/1995")
#' # "1995-03-30"
#'
#' clean.DATE("031995")
#' # "1995-03-30"
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
clean.DATE <- function(dates) {

  ## number of punctuation ----
  n.punct <- stringr::str_count(string=dates, pattern="[[:punct:]]")
  n.char <- nchar(dates)

  ## get indices ----
  mdy.idc <- which( (n.punct == 2L) | (n.punct == 0L) )
  my.idc <- which( (n.punct == 1L) | (n.char == 6L) )

  ## convert the dates into date-time format ----
  dates.clean <- suppressWarnings(lubridate::parse_date_time(x=dates, c("mdy", "ymd", "dmy")))
  my.date.clean <- suppressWarnings(lubridate::parse_date_time(x=dates[my.idc], c("my", "ym")))
  dates.clean[my.idc] <- my.date.clean

  ## adjust future years to the past ----
  # curr.YEAR <- as.integer(format(Sys.Date(), "%Y")) - 5L
  # dates.YEAR <- lubridate::year(dates.clean)
  # dates.oi.TF <- dates.YEAR >= curr.YEAR
  # dates.oi.idc <- which(dates.oi.TF)
  # dates.clean[dates.oi.idc] <- dates.clean[dates.oi.idc] - lubridate::dyears(x=100)

  ## return the dates ----
  return(dates.clean)

}




.Machine$integer.max

.Machine$double.digits


ID <- "100000000"
ID <- "000001111"

IDs <- c("1234500000", "12345", NA, 12345, 1234.56, "puzzle", "00011111")

ID.convert <- function(ID, deID.value=12345) {

  ## gather information ----
  ID.nchar <- nchar(ID)

  ## convert character to numeric ----
  ID.num <- as.numeric(ID)

  ## add de-identifying value ----
  ID.num <- ID.num + deID.value

  ## convert to character ----
  # ID.char <- as.character(ID.num)
  ID.char <- stringr::str_pad(string=ID.num, width=ID.nchar, pad="0")

  ## return to user ----
  return(ID.char)
}


#' @title Create Collection of Random IDs
#'
#' @description Given a collection of IDs, create random IDs from a large pool
#'   of potential IDs. The
#'
#' @param ID string with ID or collection of IDs
#'
#' @return string vector
#' @export
#'
#' @examples
#' IDs <- c("1234500000", "12345", NA, 12345, 1234.56, "puzzle", "00011111")
#'
#' set.seed(13)
#' ID.random(ID=IDs)
#' [1] "66253" "68228" "23889" "44599" "35845" "41519" "26556"
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#' @author Seth Walker \email{walker893@@msu.edu}
#'   ([https://github.com/walker893](https://github.com/walker893))
#'
ID.random <- function(ID, ID.type="mixed") {

  ## number of IDs ----
  n.IDs <- length(ID)

  ## create pool of potential random IDs ----
  maxSample.n <- n.IDs * 1000
  maxSample.log10 <- log10(maxSample.n)
  maxInteger <- .Machine$integer.max
  maxInteger.log10 <- log10(maxInteger)
  max.diff <- floor(maxInteger.log10 - maxSample.log10)

  if ( max.diff > 4 ) {
    maxSample.n <- n.IDs * 1000
  } else {
    maxSample.n <- n.IDs * 10^max.diff
  }
  maxSample.nchar <- max(maxSample.n) |>
    nchar()

  if ( ID.type == "letters") { random.options <- c(letters, LETTERS) }
  if ( ID.type == "mixed") { random.options <- c(letters, LETTERS, 0:9) }

  ## create ID pool of mixed values ----
  if ( ID.type == "mixed") {
    random.IDs.vector <- rep_len(x=NA, length.out=maxSample.n)
    for ( curr.ID in 1:maxSample.n) {
      random.IDs.vector[curr.ID] <- sample(x=random.options, size=20, replace=TRUE) |> paste0(collapse="")
    }
    dup.TF <- duplicated(random.IDs.vector)
    if ( any(dup.TF) ) {
      random.IDs.vector <- random.IDs.vector[!dup.TF]
    }
  }




  ## select random IDs ----
  random.IDs <- sample(x=1:maxSample.n, size=n.IDs, replace=FALSE) |>
    stringr::str_pad(width=maxSample.nchar, pad="0")

  ## return to user ----
  return(random.IDs)
}


#' @title Convert Yes-No indicators to TRUE-FALSE
#'
#' @description Often True-False data is returned as a vector of Ys and Ns or 1s
#'   and 0s. These vectors often contain blanks or `NA`s that cannot, and should
#'   not, be converted into Falses. This function converts
#'
#'   - "Y" -->> `TRUE`
#'   - "N" -->> `FALSE`
#'   - "Yes" -->> `TRUE`
#'   - "No" -->> `FALSE`
#'   - "1" -->> `TRUE`
#'   - "0" -->> `FALSE`
#'   - 1 -->> `TRUE`
#'   - 0 -->> `FALSE`
#'
#'   and leave blanks and `NA`s in the vector.
#'
#' @param YN.string string with "Yes", "No" indicators such as "Y", "N",
#'   "yes", "no", "1", or "0"
#'
#' @return logical vector
#' @export
#'
#' @examples
#' YN.string <- c("Y", "N", "1", "0", "yes", "no", NA, NA)
#' # [1]  TRUE FALSE  TRUE FALSE    NA    NA
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
convert.YN2TF <- function(YN.string) {

  YN.string <- toupper(YN.string)

  ## basic vector information ----
  vector.len <- length(YN.string)
  TF.vector <- rep_len(NA, length.out=vector.len)

  ## convert characters to TRUE/FALSE ----
  TF.vector[YN.string == "Y"]   <- TRUE
  TF.vector[YN.string == "YES"] <- TRUE
  TF.vector[YN.string == "1"]   <- TRUE
  TF.vector[YN.string == 1]   <- TRUE
  TF.vector[YN.string == "N"]   <- FALSE
  TF.vector[YN.string == "NO"]  <- FALSE
  TF.vector[YN.string == "0"]   <- FALSE
  TF.vector[YN.string == 0]   <- FALSE

  ## return TRUE/FALSE vector ----
  return(TF.vector)

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

  millennium <- substr(x=term.code, start=1, stop=1)
  year <- substr(x=term.code, start=2, stop=3)
  if (millennium == "1") {
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


#' @title Is String Blanks?
#'
#' @description Determine which strings within a vector of strings is either
#'   a string with no characters or a string comprised entirely of blanks.
#'   This function uses [stringr::str_detect()] and the regular expression
#'   `"^[[:space:]]*$"` to determine if the string contains no characters or
#'   all blanks.
#'
#' @param string or vector of strings to evaluate.
#'
#' @return logical
#' @export
#'
#' @examples
#' string <- c("", " ", "  ", "   ", " abc", "abc ", " abc ")
#' is.BLANK(string=string)
#' # [1]  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
is.BLANK <- function(string) {

  blank.TF <- stringr::str_detect(string=string,
                                  pattern="^[[:space:]]*$") |>
    dplyr::coalesce(FALSE)

  return(blank.TF)

}


#' @title Honours (or Honors) Course?
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
#' @return string with interval values
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


