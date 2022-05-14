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


#' @title Extract Email Address(es) From String
#'
#' @description Extract all email address(es) with a specific domain (_e.g._,
#'   msu.edu, gmail.com, hotmail.com, etc.)
#'
#' @param emails.string string with emails.
#' @param domains string with the domains; _e.g._, "msu.edu".
#'
#' @return string of emails
#' @export
#'
#' @examples
#' extract.emails(emails.string="emilio.dork@gmail.com 2.that.dork@egr.msu.edu dork@egr.msu.edu 517565656325@162.123 thatDork@msu.edu", domains="msu.edu")
#' # [1] "2.that.dork@egr.msu.edu" "thatdork@msu.edu"
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
extract.emails <- function(emails.string, domains="msu.edu") {

  ## make pattern search string ----
  domains.clean <- gsub(pattern="\\.", replacement="\\\\.", x=domains)
  # pattern.text <- paste("[[:alnum:]]{1,}@[[:alnum:]]{1,}\\.", domains.clean, "|[[:alnum:]]{1,}@", domains.clean, sep="")
  # pattern.text <- paste("[[:graph:]]{1,}@[[:alnum:]]{1,}\\.", domains.clean, "|[[:graph:]]{1,}@", domains.clean, sep="")
  pattern.text <- paste("[[:alnum:]]{1,}[[:graph:]]{1,}@[[:graph:]]*", domains.clean, sep="")

  ## clean emails ----
  emails.lower <- tolower(emails.string) |>
    gsub(pattern="&lt;", replacement=" ") |>
    gsub(pattern="&gt;", replacement=" ") |>
    gsub(pattern=";", replacement=" ") |>
    gsub(pattern=",", replacement=" ") |>
    gsub(pattern="\\|", replacement=" ")

  emails.oi <- stringr::str_extract_all(pattern=pattern.text, string=emails.lower) |>
    unlist() |>
    list()

  return(emails.oi)
}


