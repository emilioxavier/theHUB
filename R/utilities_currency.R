

range.mean <- function(string) {
  if (grepl(x=string, pattern="\\-")) {
    string <- str_split(string=string, pattern="[[:space:]]{0,3}-[[:space:]]{0,3}") |>
      unlist() |>
      gsub(pattern="[[:punct:]]", replacement="") |>
      as.integer() |>
      mean()
  }
  return(string)
}

convert.k2number <- function(string) {

  if ( grepl(x=string, pattern="\\-") ) {
     string <- strsplit(x=string, split="\\-") |> unlist()
  }
    # n.grands <- gsub(x=string, pattern="k", replacement="", ignore.case=TRUE) |>
    #   as.integer()
    # ##_ multiple by 1000 ----
    # value <- n.grands * 1000

  k.tf <- grepl(x=string, pattern="[[:digit:]](k|K)")

  if ( grepl(x=string, pattern="[[:digit:]](k|K)") ) {
    ##_ remove "k" ----
    value <- purrr::map_int(.x=string[k.tf], .f=function(x.string) {
      gsub(x=x.string, pattern="k", replacement="", ignore.case=TRUE) |>
        as.integer() * 1000L
    })

    values.all <- c(value, as.integer(string[!k.tf]))

    value.mu <- mean(values.all) |> as.integer()
  }

}


convert.2annual <- function(string) {

  ## if it is a weekly wage ----
  if (grepl(x=string, pattern="week")) {
    string <- gsub(x=string, pattern="[[:alpha:]]{1,}", replacement="", ignore.case=TRUE) |>  ## drop alpha characters
      gsub(pattern="\\$", replacement="") |>  ## remove dollar sign
      trimws() |>
      gsub(pattern="[[:punct:]]$", replacement="") |>
      # gsub(pattern="\\.[[:digit:]]{2}$", replacement="") |>
      gsub(pattern="(?<=[[:digit:]])\\,(?=[[:digit:]])", replacement="", perl=TRUE) |>  ## remove comma
      as.numeric() * 52
    string <- as.integer(string)
  }

  ## if it is a monthly wage ----
  if (grepl(x=string, pattern="month")) {
    string <- gsub(x=string, pattern="[[:alpha:]]{1,}", replacement="", ignore.case=TRUE) |>  ## drop alpha characters
      gsub(pattern="\\$", replacement="") |>  ## remove dollar sign
      trimws() |>
      gsub(pattern="[[:punct:]]$", replacement="") |>
      # gsub(pattern="\\.[[:digit:]]{2}$", replacement="") |>
      gsub(pattern="(?<=[[:digit:]])\\,(?=[[:digit:]])", replacement="", perl=TRUE) |>  ## remove comma
      as.numeric() * 12
    string <- as.integer(string)
  }

  ## return annual salary ----
  return(string)
}






clean.currency <- function(amount) {

  amount <- Admiss.DATA$parent.grossIncome

  ## convert NAs of various forms to NA ----
  amount <- gsub(x=amount, pattern="n/a", replacement="_NA_", ignore.case=TRUE) |>
    gsub(pattern="na", replacement="_NA_", ignore.case=TRUE)

  ## remove approximates ----
  ##__ approximately ----
  # amount <- grep(x=amount, pattern="~", value=TRUE) |>
  amount <- gsub(amount, pattern="^[[:space:]]{0,3}~[[:space:]]{0,3}", replacement="_approx_", ignore.case=TRUE) |>
    gsub(pattern="[[:space:]]{0,3}~[[:space:]]{0,3}$", replacement="_approx_", ignore.case=TRUE) |>
    gsub(pattern="[[:space:]]{0,3}approximately[[:space:]]{0,3}", replacement="_approx_", ignore.case=TRUE) |>
    gsub(pattern="[[:space:]]{0,3}approx(\\.|[[:space:]]{0,3})", replacement="_approx_", ignore.case=TRUE) |>
    gsub(pattern="^[[:space:]]{0,3}about(\\.|[[:space:]]{0,3})", replacement="_approx_", ignore.case=TRUE) |>
    gsub(pattern="^[[:space:]]{0,3}around(\\.|[[:space:]]{0,3})", replacement="_approx_", ignore.case=TRUE) |>
    gsub(pattern="^[[:space:]]{0,3}ca(\\.|[[:space:]]{0,3})", replacement="_approx_", ignore.case=TRUE)

  ## remove "pluses" ----
  # amount <- grep(x=amount, pattern="\\+", value=TRUE) |>
  # grep(x=amount, pattern="\\+", value=TRUE) |>
    gsub(x=amount, pattern="\\+\\/\\-", replacement="_approx_") |>
    gsub(pattern="\\+\\/\\-", replacement="_approx_") |>
    gsub(pattern="^\\+", replacement="_plus-start_") |>
    gsub(pattern="\\+$", replacement="_plus-end_") |>
    gsub(pattern="\\+", replacement="_plus-mid_")

  ## remove dots used to separate thousands ----
    # gsub(x="_12_", pattern="(?<=_(?=\\d{2}_))\\d+", replacement="|", perl=TRUE)
  grep(x=amount, pattern="[[:digit:]]{1,3}\\.([[:digit:]]{3}|[[:digit:]]{1,})", value=TRUE) |>
    gsub(pattern="(?<=[[:digit:]]{1})\\.(?=[[:digit:]]{3})", replacement="_dotks_", perl=TRUE) |>
    gsub(pattern="(?<=[[:digit:]]{2})\\.(?=[[:digit:]]{3})", replacement="_dotks_", perl=TRUE) |>
    gsub(pattern="(?<=[[:digit:]]{3})\\.(?=[[:digit:]]{3})", replacement="_dotks_", perl=TRUE)

  ##_ replace negative values with 0 ----
  amount[grepl(x=amount, pattern="^-", ignore.case=TRUE)] <- "_none_"
  amount[grepl(x=amount, pattern="^minus", ignore.case=TRUE)] <- "_none_"
  amount[grepl(x=amount, pattern="^\\(", ignore.case=TRUE)] <- "_none_"
  amount[grepl(x=amount, pattern="unemploy", ignore.case=TRUE)] <- "_none_"
  amount[grepl(x=amount, pattern="none", ignore.case=TRUE)] <- "_none_"

  ##_ replace keyword containing amounts with NA ----
  amount[grepl(x=amount, pattern="not sure", ignore.case=TRUE)] <- "_UNK_"
  amount[grepl(x=amount, pattern="do not know", ignore.case=TRUE)] <- "_UNK_"
  amount[grepl(x=amount, pattern="don\\'t know", ignore.case=TRUE)] <- "_UNK_"
  amount[grepl(x=amount, pattern="dont know", ignore.case=TRUE)] <- "_UNK_"
  amount[grepl(x=amount, pattern="idk", ignore.case=TRUE)] <- "_UNK_"
  amount[grepl(x=amount, pattern="unknown", ignore.case=TRUE)] <- "_UNK_"
  amount[grepl(x=amount, pattern="unkown", ignore.case=TRUE)] <- "_UNK_"
  amount[grepl(x=amount, pattern="no comment", ignore.case=TRUE)] <- "_UNK_"

  gsub(x=amount, pattern="^[[:digit:]]{1,}$", replacement="_integer_") |>
  gsub(pattern="^\\$[[:digit:]]{1,}$", replacement="_$noCommasNoCents_") |>
  gsub(pattern="^\\$[[:digit:]]{1,3}\\,([[:digit:]]{3}\\,[[:digit:]]{3}|[[:digit:]]{3})$", replacement="_$commasNoCents_") |>
  gsub(pattern="^[[:digit:]]{1,3}\\,([[:digit:]]{3}\\,[[:digit:]]{3}|[[:digit:]]{3})$", replacement="_commasNoCents_") |>
  gsub(pattern="^\\$[[:digit:]]{1,3}\\,(|[[:digit:]]{3}\\,[[:digit:]]{3}|[[:digit:]]{3})\\.[[:digit:]]{2}$", replacement="_$commasWithCents_") |>
  gsub(pattern="^[[:digit:]]{1,3}\\,(|[[:digit:]]{3}\\,[[:digit:]]{3}|[[:digit:]]{3})\\.[[:digit:]]{2}$", replacement="_commasWithCents_") |>
  gsub(pattern="^\\$[[:digit:]]{1,}\\.[[:digit:]]{2}$", replacement="_$noCommaWithCents_") |>
  gsub(pattern="^[[:digit:]]{1,}\\.[[:digit:]]{2}$", replacement="_noCommaWithCents_") |>
  gsub(pattern="^\\$[[:digit:]]{1,}\\.[[:digit:]]{2}$", replacement="_$noCommaWithCents_") |> table() |> sort(decreasing=T)


  grep(x=amount, pattern="(?<=[[:digit:]])(k|K|thousand)", perl=TRUE, value=TRUE)



  amount <- purrr::map_chr(amount, convert.2annual)
  amount <- purrr::map_chr(amount, range.mean)
  # amount <- convert.2annual(amount)
  # amount <- range.mean(amount)


  return(amount)

}


#' @title
#'
#' @description
#'
#' translate English spelled quantities into their integer counterparts.
#'
#' @details Allows you to translate to integer numerical words spelled in English
#' Text must be previously cleaned & removed extraneous words or symbols
#' Quantities MUST be written in a correct English (this is not a grammar tool)
#' The upper limit is up to the millions range. Cents must be removed.
#'
#' Taken from [Ben Marwick](https://github.com/benmarwick)'s
#' [words2number](https://github.com/benmarwick/words2number)
#' package for simplicity of installing and longevity of using `theHUB`.
#'
#' @export
#' @param x A English word for a number.
#' @examples
#' to_number("one thousand and seventy two")
#' to_number(c("thirty seven", "forty two"))
#'
convert.2number <- function(word.number) {

  expr <-
    gsub(word.number, pattern="-", replacement=" ") |>
    gsub(pattern="eleventh", replacement="+11", ignore.case=TRUE) |>
    gsub(pattern="twelfth", replacement="+12", ignore.case=TRUE) |>
    gsub(pattern="thirteenth", replacement="+13", ignore.case=TRUE) |>
    gsub(pattern="fourteenth", replacement="+14", ignore.case=TRUE) |>
    gsub(pattern="fifteenth", replacement="+15", ignore.case=TRUE) |>
    gsub(pattern="sixteenth", replacement="+16", ignore.case=TRUE) |>
    gsub(pattern="seventeenth", replacement="+17", ignore.case=TRUE) |>
    gsub(pattern="eighteenth", replacement="+18", ignore.case=TRUE) |>
    gsub(pattern="nineteenth", replacement="+19", ignore.case=TRUE) |>
    gsub(pattern="twentieth", replacement="+20", ignore.case=TRUE) |>
    gsub(pattern="thirtieth", replacement="+30", ignore.case=TRUE) |>
    gsub(pattern="fortieth", replacement="+40", ignore.case=TRUE) |>
    gsub(pattern="fiftieth", replacement="+50", ignore.case=TRUE) |>
    gsub(pattern="sixtieth", replacement="+60", ignore.case=TRUE) |>
    gsub(pattern="seventieth", replacement="+70", ignore.case=TRUE) |>
    gsub(pattern="eightieth", replacement="+80", ignore.case=TRUE) |>
    gsub(pattern="ninetieth", replacement="+90", ignore.case=TRUE) |>
    gsub(pattern="eleven", replacement="+11", ignore.case=TRUE) |>
    gsub(pattern="twelve", replacement="+12", ignore.case=TRUE ) |>
    gsub(pattern="thirteen", replacement="+13", ignore.case=TRUE) |>
    gsub(pattern="fourteen", replacement="+14", ignore.case=TRUE) |>
    gsub(pattern="fifteen", replacement="+15", ignore.case=TRUE) |>
    gsub(pattern="sixteen", replacement="+16", ignore.case=TRUE) |>
    gsub(pattern="seventeen", replacement="+17", ignore.case=TRUE) |>
    gsub(pattern="eighteen", replacement="+18", ignore.case=TRUE) |>
    gsub(pattern="nineteen", replacement="+19", ignore.case=TRUE) |>
    gsub(pattern="twenty", replacement="+20", ignore.case=TRUE) |>
    gsub(pattern="thirty", replacement="+30", ignore.case=TRUE) |>
    gsub(pattern="forty", replacement="+40", ignore.case=TRUE) |>
    gsub(pattern="fifty", replacement="+50", ignore.case=TRUE) |>
    gsub(pattern="sixty", replacement="+60", ignore.case=TRUE) |>
    gsub(pattern="seventy", replacement="+70", ignore.case=TRUE) |>
    gsub(pattern="eighty", replacement="+80", ignore.case=TRUE) |>
    gsub(pattern="ninety", replacement="+90", ignore.case=TRUE) |>

    gsub(pattern="one hundredth", replacement="+100", ignore.case=TRUE) |>
    gsub(pattern="two hundredth", replacement="+200", ignore.case=TRUE) |>
    gsub(pattern="three hundredth", replacement="+300", ignore.case=TRUE) |>
    gsub(pattern="four hundredth", replacement="+400", ignore.case=TRUE) |>
    gsub(pattern="five hundredth", replacement="+500", ignore.case=TRUE) |>
    gsub(pattern="six hundredth", replacement="+600", ignore.case=TRUE) |>
    gsub(pattern="seven hundredth", replacement="+700", ignore.case=TRUE) |>
    gsub(pattern="eight hundredth", replacement="+800", ignore.case=TRUE) |>
    gsub(pattern="nine hundredth", replacement="+900", ignore.case=TRUE) |>
    gsub(pattern="one hundred", replacement="+100", ignore.case=TRUE) |>
    gsub(pattern="two hundred", replacement="+200", ignore.case=TRUE) |>
    gsub(pattern="three hundred", replacement="+300", ignore.case=TRUE) |>
    gsub(pattern="four hundred", replacement="+400", ignore.case=TRUE) |>
    gsub(pattern="five hundred", replacement="+500", ignore.case=TRUE) |>
    gsub(pattern="six hundred", replacement="+600", ignore.case=TRUE) |>
    gsub(pattern="seven hundred", replacement="+700", ignore.case=TRUE) |>
    gsub(pattern="eight hundred", replacement="+800", ignore.case=TRUE) |>
    gsub(pattern="nine hundred", replacement="+900", ignore.case=TRUE) |>
    gsub(pattern="zero", replacement="0", ignore.case=TRUE) |>
    gsub(pattern="first", replacement="+1", ignore.case=TRUE) |>
    gsub(pattern="second", replacement="+2", ignore.case=TRUE) |>
    gsub(pattern="third", replacement="+3", ignore.case=TRUE) |>
    gsub(pattern="fourth", replacement="+4", ignore.case=TRUE) |>
    gsub(pattern="fifth", replacement="+5", ignore.case=TRUE) |>
    gsub(pattern="sixth", replacement="+6", ignore.case=TRUE) |>
    gsub(pattern="seventh", replacement="+7", ignore.case=TRUE) |>
    gsub(pattern="eighth", replacement="+8", ignore.case=TRUE) |>
    gsub(pattern="ninth", replacement="+9", ignore.case=TRUE) |>
    gsub(pattern="one", replacement="+1", ignore.case=TRUE) |>
    gsub(pattern="two", replacement="+2", ignore.case=TRUE) |>
    gsub(pattern="three", replacement="+3", ignore.case=TRUE) |>
    gsub(pattern="four", replacement="+4", ignore.case=TRUE) |>
    gsub(pattern="five", replacement="+5", ignore.case=TRUE) |>
    gsub(pattern="six", replacement="+6", ignore.case=TRUE) |>
    gsub(pattern="seven", replacement="+7", ignore.case=TRUE) |>
    gsub(pattern="eight", replacement="+8", ignore.case=TRUE) |>
    gsub(pattern="nine", replacement="+9", ignore.case=TRUE) |>

    gsub(pattern="millions", replacement=")*(1000000)+(0", ignore.case=TRUE) |>
    gsub(pattern="million", replacement=")*(1000000)+(0", ignore.case=TRUE) |>
    gsub(pattern="mil", replacement=")*(1000000)+(0", ignore.case=TRUE) |>
    gsub(pattern="thousandth", replacement=")*(1000)+(0", ignore.case=TRUE) |>
    gsub(pattern="thousand", replacement=")*(1000)+(0", ignore.case=TRUE) |>
    gsub(pattern="hundredth", replacement="+100", ignore.case=TRUE) |>
    gsub(pattern="hundred", replacement="+100", ignore.case=TRUE) |>
    gsub(pattern="tenth", replacement="+10", ignore.case=TRUE) |>
    gsub(pattern="ten", replacement="+10", ignore.case=TRUE) |>
    gsub(pattern="first", replacement="+1", ignore.case=TRUE) |>
    gsub(pattern="one", replacement="+1", ignore.case=TRUE) |>
    gsub(pattern="and", replacement="", ignore.case=TRUE) |>
    gsub(pattern=" ", replacement="", ignore.case=TRUE) |>
    gsub(pattern="^", replacement="(0", ignore.case=TRUE) |>
    gsub(pattern="$", replacement=")", ignore.case=TRUE) |>
    gsub(pattern="\\(0\\(", replacement="", ignore.case=TRUE ) |>
    gsub(pattern="\\+\\+", replacement="\\+\\(", ignore.case=TRUE ) |>
    gsub(pattern="\\)\\+\\)", replacement="\\)", ignore.case=TRUE )

  if (any(grepl('[[:alpha:]]', expr)))
    stop("expression ", expr, " cannot be evaluated")

  result <- sapply(expr, function(y) eval(parse(text = y)), USE.NAMES=FALSE)
  setNames(as.integer(result), word.number)
}
