#' @title Convert Control Characters to Spaces
#' @description Converts control characters into physical spaces.
#' @details One of several text cleaning functions. The function replaces the
#'   following control characters `\n`, `\r`, `\t`, `\v`, and `\f` (those
#'   identified by the `[:cntrl:]` regular expression) with a series of three (3)
#'   physical spaces. While multiple, concurrent and connected control characters
#'   (`\n\r\t\v\f`) would be replace by three spaces, control characters separated
#'   by any printable character (defined by `[:print:]`), even spaces, are retained;
#'   see example.
#'
#' @param text A string of words forming a sentence or phrase.
#'
#' @return string of cleaned characters forming a sentence or phrase.
#' @export
#'
#' @examples
#' comment <- "All day I see class mates and send e-mails.\n\r\t\v\fI miss East Lansing."
#' clean.cntrl(text=comment)
#' # [1] "All day I see class mates and send e-mails.   I miss East Lansing."
#'
#' comment <- "All day I see class mates and send e-mails.\n  \r  \t  \v\fI miss East Lansing."
#' clean.cntrl(text=comment)
#' # [1] "All day I see class mates and send e-mails.                  I miss East Lansing."
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
#' @family "text cleaning"
#'
clean.cntrl <- function(text) {

  cntrl.less <- gsub(x=text, pattern="[[:cntrl:]]+", replacement="   ")

  return(cntrl.less)
}


#' @title Convert all Spaces to "Normal" Spaces
#' @description Converts various type of spaces into "normal" spaces.
#' @details One of several text cleaning functions. The function replaces various
#'   types of spaces with ASCII spaces that are breakable. The reason for this
#'   function is to replace non-breakable space with standard, breakable spaces.
#'
#' @param text A string of words forming a sentence or phrase.
#'
#' @return string of cleaned characters forming a sentence or phrase.
#' @export
#'
#' @examples
#' \dontrun{
#'   comment <- "All day I see class mates and send e-mails.   I miss East Lansing."
#'
#'   convert.spaces(text=comment)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
#' @family "text cleaning"
#'
convert.spaces <- function(text) {
  # see https://stackoverflow.com/questions/31790440/regex-to-replace-no-break-space
  # found the no-break-space by converting strings to UTF-32 via stringi::stri_enc_toutf32()

  normal.spaces <- stringr::str_replace_all(string=text,
                                            pattern="\u0020|\u00A0|\u1680|\u180E|\u2000|\u2001|\u2002|\u2003|\u2004|\u2005|\u2006|\u2007|\u2008|\u2009|\u200A|\u200B|\u202F|\u205F|\u3000|\uFEFF",
                                            replacement=" ")

  return(normal.spaces)
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
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
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
#' extract.emails(emails.string="emilio.dork@gmail.com 2.that.dork@egr.msu.edu
#' dork@egr.msu.edu 517565656325@162.123 thatDork@msu.edu", domains="msu.edu")
#' # [1] "2.that.dork@egr.msu.edu" "thatdork@msu.edu"
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
#' @family "text cleaning"
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


