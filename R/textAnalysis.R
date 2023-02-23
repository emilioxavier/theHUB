

#' @title No Comment Present
#'
#' @description Indicates if the presented comment was intended to be a "none-
#'   comment" by the author. Steps to determine if the user supplied comment is
#'   considered a non-comment:
#'   - convert string to lower case
#'   - count the number of words
#'   - count the number of characters
#'   - if the string is a single word OR the number of characters is zero (0)
#'     - remove all punctuation
#'     - if the single word is: "", "na", "no", "none", "nothing" indicate the
#'       comment was intended to be a "non-comment"
#'
#' @param comment string with the comment
#'
#' @return logical indicating if the comment is a common non-response
#' @export
#'
#' @examples
#'  is.none("today is awesome!")
#'  # FALSE
#'
#'  is.none("NA")
#'  # TRUE
#'
#'  is.none("N/A")
#'  # TRUE
#'
#'  is.none("")
#'  # TRUE
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
is.none <- function(comment) {

  comment.lower <- trimws(tolower(comment))
  n.words <- word.count(comment=comment.lower)
  n.char <- nchar(comment.lower)

  if ( n.words == 1 | n.char == 0) {
    comment.lower <- gsub(pattern="[[:punct:]]", replacement="", x=comment.lower)

    none.tf <- any(is.na(comment.lower) |
                     grepl(x=comment.lower, pattern="") |
                     grepl(x=comment.lower, pattern="na") |
                     grepl(x=comment.lower, pattern="no") |
                     grepl(x=comment.lower, pattern="none") |
                     grepl(x=comment.lower, pattern="nothing")
    )
  } else {
    none.tf <- FALSE
  }

  return(none.tf)
}


#' @title Word Count
#'
#' @description Count the number of words in a (text) string. A word is considered
#'   any number of characters seperated by at least one physical space.
#'
#' @param comment string of interest
#'
#' @return integer indicating the number of words in the provided string.
#' @export
#'
#' @examples
#'  word.count("today is awesome!")
#'  # 3
#'
#'  word.count("no")
#'  # 1
#'
#'  word.count("")
#'  # 0
#'
#'  word.count("yes, 3 is a word.")
#'  # 5
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
word.count <- function(comment) {

  comment.split <- unlist(strsplit(x=comment, split=" "))
  n.words <- as.integer(length(comment.split))

  ## return word count ----
  return(n.words)
}



#' @title Comment Summary
#'
#' @description Brief, numeric summary of the comments added to the `tibble`
#'   (or `data.frame`).
#'
#' @param data `tibble` (or `data.frame`) with the comments of interest
#' @param comment.col column name with the comments; _e.g._, `"comments"`
#'
#' @return augmented `tibble` (or `data.frame`) with the following additional
#'   information:
#'   - is the comment a non-comment (see [is.none()])
#'   - number of words in the comment (see [word.count()])
#'   - number of keyword areas contained within the comment
#'
#'   A summary for all comments is returned to the user.
#'
#' @importFrom stats fivenum
#' @importFrom tidyselect ends_with
#'
#' @export
#' @importFrom dplyr filter mutate select
#'
#' @examples
#' \dontrun{
#' comment.summary(data=comment.data, comment.col="comment.clean")
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
comment.summary <- function(data, comment.col) {

  ## comment == NA | "none" ----
  data <- dplyr::mutate(data, "comment.present"=!purrr::map_lgl(data[[comment.col]], is.none))

  ## comment length ----
  data <- dplyr::mutate(data, "n.words"=purrr::map_int(data[[comment.col]], word.count))
  data$n.words[!data$comment.present] <- 0

  ## area counts ----
  data.areas <- dplyr::select(data, ends_with(".tf"))
  data$n.areas <- rowSums(data.areas)
  data$n.areas[!data$comment.present] <- 0

  ## collect information to report ----
  n.entries <- nrow(data)
  ##_ number of comments ----
  n.with.comments <- sum(data$comment.present)
  n.without.comments <- n.entries - n.with.comments
  data.comments <- dplyr::filter(data, .data$comment.present==TRUE)
  comments.mu <- mean(data.comments$n.areas)
  comments.fivenum <- fivenum(data.comments$n.areas) ## minimum, lower-hinge, median, upper-hinge, maximum
  ##_ number of words ----
  words.mu <- mean(data.comments$n.words)
  words.fivenum <- fivenum(data.comments$n.words) ## minimum, lower-hinge, median, upper-hinge, maximum

  ## report the summary information ----
  message("Information about the comments:\n")
  message(paste("  - Number of entries: ", n.entries, sep=""))
  message(paste("  - Number with comments: ", n.with.comments, " (", round(n.with.comments/n.entries*100, digits=0), "%)", sep=""))
  message(paste("  - Areas coverage:"))
  message(paste("    + Average number of areas in each comment: ", round(comments.mu, digits=1), sep=""))
  message(paste("    + Minimum number of areas covered: ", comments.fivenum[1], sep=""))
  message(paste("    + First Quartile: ", comments.fivenum[2], sep=""))
  message(paste("    + Median number of areas covered: ", comments.fivenum[3], sep=""))
  message(paste("    + Third Quartile: ", comments.fivenum[4], sep=""))
  message(paste("    + Maximum number of areas covered: ", comments.fivenum[5], sep=""))
  message(paste("  - Word counts:"))
  message(paste("    + Average number of words in each comment: ", round(words.mu, digits=1), sep=""))
  message(paste("    + Minimum number of words: ", words.fivenum[1], sep=""))
  message(paste("    + First Quartile: ", words.fivenum[2], sep=""))
  message(paste("    + Median number of words: ", words.fivenum[3], sep=""))
  message(paste("    + Third Quartile: ", words.fivenum[4], sep=""))
  message(paste("    + Maximum number of words: ", words.fivenum[5], sep=""))

  ## return tibble ----
  return(data)
}


#' @title Keywords Present
#'
#' @description Using either the [keywords] provided within [theHUB] or a `tibble`
#'   with a collection of user defined terms and areas. The `tibble` needs to
#'   have each keyword in the _keyword_ column and the corresponding area in the
#'   _area_ column.
#'
#' @details Convert the [keywords] to a `tibble` with a row for each _area_ using
#'   the following command.
#'
#'   ```
#'   keywords
#'    A tibble: 206 x 2
#'     keyword    area
#'     <chr>      <chr>
#'   1 adviser    acadSupport
#'   2 advising   acadSupport
#'   3 advisor    acadSupport
#'   4 assignment assessment
#'
#'   keywords.tb <- dplyr::group_by(keywords, area) |>
#'     dplyr::summarise(query=paste(keyword, collapse="|"))
#'
#'   keywords.tb
#'   # A tibble: 16 x 2
#'     area          query
#'     <chr>         <chr>
#'   1 acadSupport   adviser|advising|advisor
#'   2 assessment    assignment|...
#'   ```
#'
#' @param data `tibble` (or `data.frame`) with the comments of interest
#' @param comment.col string indicating the comments of interest; _e.g._, `"comments.clean"`
#' @param keywords.tb `tibble` with the keywords and area
#'
#' @return `tibble` (or `data.frame`) with the original data plus a column
#'   for each provided area. Each _area_ column has a `TRUE` or `FALSE` for
#'   each row indicating if the comment had a _keyword_ present in the comment.
#' @export
#' @importFrom dplyr mutate pull
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' keywords.tb <- group_by(keywords, area) |>
#'     summarise(query=paste(keyword, collapse="|"))
#'
#' fs20.comments <- has.keywords(data=fs20.comments,
#'                               comment.col="comments.clean",
#'                               keywords.tb=keywords.tb)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
has.keywords <- function(data, comment.col, keywords.tb) {

  ## pull areas of interest ----
  # areas <- dplyr::pull(keywords.tb, area)
  areas <- keywords.tb$area

  ## loop over data and indicate areas ----
  for (area.oi in areas) {
    query <- dplyr::pull(dplyr::filter(keywords.tb, .data$area==area.oi), query)
    query.name <- paste(area.oi, ".tf", sep="")
    data <- dplyr::mutate(data,
                          {{query.name}}:=stringr::str_detect(string=.data[[comment.col]], pattern=query))
  }

  ## return tibble ----
  return(data)
}


#' @title Construct Unified Terms
#'
#' @description In text analysis, the concept of compounds words is difficult to
#'   accurately capture, especially when performing term frequency analysis. To
#'   accommodate the identification and counting of a multi-term idea/concept,
#'   terms comprised of two or more words are combined into a unified term.
#'
#' @param comment string of characters (usually text) to be evaluated and unified.
#'   Alternatively, the column containing the comments can be passed to the
#'   function as often is done via [dplyr::mutate()]
#' @param clean logical indicating if the function should also clean the comments
#'   to be unified; default: `TRUE`. See [clean.comment()] for the details
#'   on comment cleaning.
#'
#' @export
#' @return vector of comments where predetermined terms are unified (and possibly
#'   cleaned unless the user has changed the default). _**NOTE**_: Unified terms
#'   are camel case. For example, "wi-fi" is returned as "WiFi" and "burn out"
#'   is returned as "burnOut".
#'
#' @examples
#' comment <- "The Wi-Fi is down. My mental health is bad because  I lost my financial Aid."
#'
#' unified.terms(comment=comment)
#' # "the WiFi is down. my mentalHealth is bad because i lost my finAid."
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
unified.terms <- function(comment, clean=TRUE) {

  ## clean the comments ----
  if ( clean == TRUE ) {
    cleanish <- clean.comment(comment=comment)
  } else {
    cleanish <- comment
  }

  ## create unified terms ----
  unified <- gsub(x=cleanish, pattern="academic dishonesty", replacement="academicDishonesty")
  unified <- gsub(x=unified, pattern="at home", replacement="atHome")
  unified <- gsub(x=unified, pattern="burn out", replacement="burnOut")
  unified <- gsub(x=unified, pattern="burned out", replacement="burnOut")
  unified <- gsub(x=unified, pattern="burnt out", replacement="burnOut")
  unified <- gsub(x=unified, pattern="burn me out", replacement="burnOut")
  unified <- gsub(x=unified, pattern="burned me out", replacement="burnOut")

  unified <- gsub(x=unified, pattern="class material", replacement="classMaterial")
  unified <- gsub(x=unified, pattern="course material", replacement="classMaterial")
  unified <- gsub(x=unified, pattern="lecture material", replacement="classMaterial")
  unified <- gsub(x=unified, pattern="class note", replacement="classNote")
  unified <- gsub(x=unified, pattern="course note", replacement="classNote")
  unified <- gsub(x=unified, pattern="lecture note", replacement="classNote")
  unified <- gsub(x=unified, pattern="class work", replacement="classWork")
  unified <- gsub(x=unified, pattern="course work", replacement="classWork")
  unified <- gsub(x=unified, pattern="lecture work", replacement="classWork")
  unified <- gsub(x=unified, pattern="school work", replacement="classWork")
  unified <- gsub(x=unified, pattern="discussion based", replacement="discussionBased")
  unified <- gsub(x=unified, pattern="discussion board", replacement="discussionBoard")
  unified <- gsub(x=unified, pattern="discussion post", replacement="discussionPost")
  unified <- gsub(x=unified, pattern="final exam", replacement="finalExam")
  unified <- gsub(x=unified, pattern="financial aid", replacement="finAid")
  unified <- gsub(x=unified, pattern="financial hardship", replacement="finHardship")
  unified <- gsub(x=unified, pattern="financial relief", replacement="finRelief")
  unified <- gsub(x=unified, pattern="greek life", replacement="greeklife")
  unified <- gsub(x=unified, pattern="honor code", replacement="honorCode")
  unified <- gsub(x=unified, pattern="honour code", replacement="honorCode")
  unified <- gsub(x=unified, pattern="international student", replacement="internationalStudent")
  unified <- gsub(x=unified, pattern="internet connection", replacement="internetConnection")
  unified <- gsub(x=unified, pattern="jet lag", replacement="jetLag")
  unified <- gsub(x=unified, pattern="lockdown browser", replacement="lockdownBrowser")
  unified <- gsub(x=unified, pattern="mental health", replacement="mentalHealth")
  unified <- gsub(x=unified, pattern="michigan state university", replacement="msu")
  unified <- gsub(x=unified, pattern="michigan state uni", replacement="msu")
  unified <- gsub(x=unified, pattern="michigan state", replacement="msu")
  unified <- gsub(x=unified, pattern="microsoft team", replacement="microsoftTeam")
  unified <- gsub(x=unified, pattern="office hour", replacement="officeHour")
  unified <- gsub(x=unified, pattern="paid leave", replacement="paidLeave")
  unified <- gsub(x=unified, pattern="pay attention", replacement="payAttention")
  unified <- gsub(x=unified, pattern="paying attention", replacement="payAttention")
  unified <- gsub(x=unified, pattern="professional development", replacement="professionalDevelopment")
  unified <- gsub(x=unified, pattern="self care", replacement="selfCare")  ## includes asynchronous
  unified <- gsub(x=unified, pattern="social interaction", replacement="socialInteraction")  ## includes asynchronous
  unified <- gsub(x=unified, pattern="support system", replacement="supportSystem")  ## includes asynchronous
  unified <- gsub(x=unified, pattern="synchronous class", replacement="synchClass")  ## includes asynchronous
  unified <- gsub(x=unified, pattern="synchronous course", replacement="synchClass")  ## includes asynchronous
  unified <- gsub(x=unified, pattern="synchronous lecture", replacement="synchClass")  ## includes asynchronous
  unified <- gsub(x=unified, pattern="synchronous sessions", replacement="synchClass")  ## includes asynchronous
  unified <- gsub(x=unified, pattern="synch sessions", replacement="synchClass")  ## includes asynchronous
  unified <- gsub(x=unified, pattern="time zone", replacement="timeZone")
  unified <- gsub(x=unified, pattern="work fulltime", replacement="workFullTime")
  unified <- gsub(x=unified, pattern="work schedule", replacement="workSchedule")
  unified <- gsub(x=unified, pattern="workschedule", replacement="workSchedule")
  unified <- gsub(x=unified, pattern="zoom class", replacement="zoomClass")
  unified <- gsub(x=unified, pattern="zoom course", replacement="zoomClass")
  unified <- gsub(x=unified, pattern="zoom lecture", replacement="zoomClass")

  ## return the cleaned comment ----
  return(unified)

}

#' @title Clean Comments
#'
#' @description Normalize (aka _clean_) comments to reduce the number of small
#'   counts for similar or related words.
#'
#' @details Often similar words or ideas have different forms depending on their
#'   location within a sentence. For example, the term "advisor" can have three
#'   different forms within a comment: "adviser", "advisers", "advisors" and all
#'   three are converted (transformed) to "advisor".
#'
#' @param comment string of characters (usually text) to be evaluated and unified.
#'   Alternatively, the column containing the comments can be passed to the
#'   function as often is done via [dplyr::mutate()]
#'
#' @export
#' @importFrom stringr str_replace_all
#'
#' @return vector of comments where predetermined terms are cleaned.
#'   _**NOTE**_: Some cleaned terms are returned in camel case.
#'   For example, "e-mail" is returned as "eMail" and "face to face" and
#'   "face-to-face" are returned as "faceTOface".
#'
#' @examples
#' comment <- "All day I see class mates and send e-mails.   I miss East Lansing."
#'
#' clean.comment(comment=comment)
#' # "allDay i see classmate and send eMail. i miss eastLansing."
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
clean.comment <- function(comment) {

  ## remove carriage returns ----
  cleanish <- gsub(x=comment, pattern="\n", replacement=" ")
  cleanish <- gsub(x=cleanish, pattern="\r", replacement=" ")
  cleanish <- gsub(x=cleanish, pattern="\t", replacement=" ")
  cleanish <- gsub(x=cleanish, pattern="\f", replacement=" ")

  ## convert multiple-spaces to single-spaces ----
  cleanish <- stringr::str_replace_all(string=cleanish,
                                       pattern="[[:space:]]{2,}",
                                       replacement=" ")

  ## convert to ASCII ----
  cleanish <- tolower(convert.toASCII(cleanish))

  ## remove 's ----
  cleanish <- gsub(x=cleanish, pattern="\\'s", replacement="") ## contains stop word

  ## create normalize terms ----
  cleanish <- gsub(x=cleanish, pattern="advisors", replacement="advisor") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="advisers", replacement="advisor") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="adviser", replacement="advisor") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="all day", replacement="allDay") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="a lot", replacement="aLot") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="assignments", replacement="assignment")
  cleanish <- gsub(x=cleanish, pattern="at home", replacement="atHome") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="boards", replacement="board") ## discussion _____
  cleanish <- gsub(x=cleanish, pattern="calls", replacement="call")
  cleanish <- gsub(x=cleanish, pattern="careers", replacement="career")
  ## children -->> child see below
  cleanish <- gsub(x=cleanish, pattern="classes", replacement="class")
  cleanish <- gsub(x=cleanish, pattern="class mates", replacement="classmate")
  cleanish <- gsub(x=cleanish, pattern="classmates", replacement="classmate")
  cleanish <- gsub(x=cleanish, pattern="clubs", replacement="club")
  cleanish <- gsub(x=cleanish, pattern="costs", replacement="cost")
  cleanish <- gsub(x=cleanish, pattern="courses", replacement="class")
  cleanish <- gsub(x=cleanish, pattern="course", replacement="class")
  cleanish <- gsub(x=cleanish, pattern="covid 19", replacement="covid")
  cleanish <- gsub(x=cleanish, pattern="covid-19", replacement="covid")
  cleanish <- gsub(x=cleanish, pattern="covid19", replacement="covid")
  cleanish <- gsub(x=cleanish, pattern="dates", replacement="date")
  cleanish <- gsub(x=cleanish, pattern="discussions", replacement="discussion")
  cleanish <- gsub(x=cleanish, pattern="east lansing", replacement="eastLansing")
  cleanish <- gsub(x=cleanish, pattern="e-mail", replacement="email")
  cleanish <- gsub(x=cleanish, pattern="emails", replacement="email")
  cleanish <- gsub(x=cleanish, pattern="email", replacement="eMail")
  cleanish <- gsub(x=cleanish, pattern="every day", replacement="everyDay") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="everyday", replacement="everyDay") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="exams", replacement="exam")
  cleanish <- gsub(x=cleanish, pattern="examples", replacement="example")
  cleanish <- gsub(x=cleanish, pattern=" fa ", replacement="financial aid")
  cleanish <- gsub(x=cleanish, pattern="face to face", replacement="faceTOface")
  cleanish <- gsub(x=cleanish, pattern="face-to-face", replacement="faceTOface")
  cleanish <- gsub(x=cleanish, pattern="full price", replacement="fullTuition") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="full tuition", replacement="fullTuition") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="full time", replacement="fullTime") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="grades", replacement="grade")
  cleanish <- gsub(x=cleanish, pattern="health care", replacement="healthCare")
  cleanish <- gsub(x=cleanish, pattern="healthcare", replacement="healthCare")
  cleanish <- gsub(x=cleanish, pattern="help room", replacement="helpRoom")
  cleanish <- gsub(x=cleanish, pattern="helproom", replacement="helpRoom")
  cleanish <- gsub(x=cleanish, pattern=" hw ", replacement=" homeWork ")
  cleanish <- gsub(x=cleanish, pattern=" hws ", replacement=" homeWork ")
  cleanish <- gsub(x=cleanish, pattern="home work", replacement="homeWork")
  cleanish <- gsub(x=cleanish, pattern="homework", replacement="homeWork")
  cleanish <- gsub(x=cleanish, pattern="hours", replacement="hour")
  cleanish <- gsub(x=cleanish, pattern="in person", replacement="inperson")
  cleanish <- gsub(x=cleanish, pattern="in-person", replacement="inperson")
  cleanish <- gsub(x=cleanish, pattern="inperson", replacement="inPerson")
  cleanish <- gsub(x=cleanish, pattern="internships", replacement="internship")
  cleanish <- gsub(x=cleanish, pattern="jobs", replacement="job")
  cleanish <- gsub(x=cleanish, pattern="keep up", replacement="keepUp") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="keeping up", replacement="keepUp") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="keeping up", replacement="keepUp") ## contains stop word
  ## kids -->> kid see below
  cleanish <- gsub(x=cleanish, pattern="labs ", replacement="laboratory ")
  cleanish <- gsub(x=cleanish, pattern=" labs", replacement=" laboratory")
  cleanish <- gsub(x=cleanish, pattern="lab ", replacement="laboratory ")
  cleanish <- gsub(x=cleanish, pattern=" lab", replacement=" laboratory")
  cleanish <- gsub(x=cleanish, pattern="laboratories", replacement="laboratory")
  cleanish <- gsub(x=cleanish, pattern="laboratorys", replacement="laboratory")
  cleanish <- gsub(x=cleanish, pattern="laboratoryoratory", replacement="laboratory")
  cleanish <- gsub(x=cleanish, pattern="lack of", replacement="lackOf") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="lectures", replacement="lecture")
  cleanish <- gsub(x=cleanish, pattern="lon capa", replacement="lonCapa")
  cleanish <- gsub(x=cleanish, pattern="materials", replacement="material")
  cleanish <- gsub(x=cleanish, pattern="meetings", replacement="meeting")
  cleanish <- gsub(x=cleanish, pattern="mental-health", replacement="mental health")
  cleanish <- gsub(x=cleanish, pattern="mentalhealth", replacement="mental health")
  cleanish <- gsub(x=cleanish, pattern="msu\\'s", replacement="msu")
  cleanish <- gsub(x=cleanish, pattern="msus", replacement="msu")
  cleanish <- gsub(x=cleanish, pattern="on line", replacement="onLine") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="on-line", replacement="onLine") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="online", replacement="onLine") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="parents", replacement="parent")
  cleanish <- gsub(x=cleanish, pattern="platforms", replacement="platform")
  cleanish <- gsub(x=cleanish, pattern="posts", replacement="post") ## discussion _____
  cleanish <- gsub(x=cleanish, pattern="ratemyinstructor", replacement="rate my instructor")
  cleanish <- gsub(x=cleanish, pattern="ratemyprofessor", replacement="rate my instructor")
  cleanish <- gsub(x=cleanish, pattern="ratemyprof", replacement="rate my instructor")
  cleanish <- gsub(x=cleanish, pattern="sessions", replacement="session")
  ## slide terms -->> powerpoint
  cleanish <- gsub(x=cleanish, pattern="power point", replacement="powerPoint")
  cleanish <- gsub(x=cleanish, pattern="slidedeck", replacement="powerPoint") ## slide(s) -->> powerPoint
  cleanish <- gsub(x=cleanish, pattern="slide deck", replacement="powerPoint") ## slide(s) -->> powerPoint
  cleanish <- gsub(x=cleanish, pattern="slides", replacement="powerPoint") ## slide(s) -->> powerPoint
  cleanish <- gsub(x=cleanish, pattern="slide", replacement="powerPoint") ## slide(s) -->> powerPoint
  cleanish <- gsub(x=cleanish, pattern="powerpoints", replacement="powerPoint")
  cleanish <- gsub(x=cleanish, pattern="powerpoint", replacement="powerPoint")
  cleanish <- gsub(x=cleanish, pattern="pre-recorded", replacement="preRecorded")
  cleanish <- gsub(x=cleanish, pattern="prerecorded", replacement="preRecorded")
  cleanish <- gsub(x=cleanish, pattern="questions", replacement="question")
  cleanish <- gsub(x=cleanish, pattern="quizzes", replacement="quiz")
  cleanish <- gsub(x=cleanish, pattern="students", replacement="student")
  cleanish <- gsub(x=cleanish, pattern="teaching", replacement="lecturing")
  cleanish <- gsub(x=cleanish, pattern="tests", replacement="test")
  cleanish <- gsub(x=cleanish, pattern=" uni ", replacement=" university ")
  cleanish <- gsub(x=cleanish, pattern="videos", replacement="video")
  cleanish <- gsub(x=cleanish, pattern="websites", replacement="website") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="well being", replacement="wellBeing") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="well-being", replacement="wellBeing") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="wellbeing", replacement="wellBeing") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="wi-fi", replacement="WiFi")
  cleanish <- gsub(x=cleanish, pattern="wifi", replacement="WiFi")
  cleanish <- gsub(x=cleanish, pattern="will power", replacement="willPower") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="will-power", replacement="willPower") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="willpower", replacement="willPower") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="workload", replacement="workLoad") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="work load", replacement="workLoad") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="work schedule", replacement="workSchedule") ## contains stop word
  cleanish <- gsub(x=cleanish, pattern="zones", replacement="zone")

  ## term normalization ----
  ##_ children/child ----
  cleanish <- gsub(x=cleanish, pattern="children\\'s", replacement="child")
  cleanish <- gsub(x=cleanish, pattern="childrens", replacement="child")
  cleanish <- gsub(x=cleanish, pattern="children", replacement="child")
  cleanish <- gsub(x=cleanish, pattern="child\\'s", replacement="child")
  cleanish <- gsub(x=cleanish, pattern="childs", replacement="child")
  cleanish <- gsub(x=cleanish, pattern="kid\\'s", replacement="child")
  cleanish <- gsub(x=cleanish, pattern="kids", replacement="child")
  cleanish <- gsub(x=cleanish, pattern=" kid ", replacement=" child ")

  ##_ instructor ----
  ##__ pronoun -->> instructor ----
  cleanish <- gsub(x=cleanish, pattern=" he ", replacement=" instructor ") ## pronoun -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" her ", replacement=" instructor ") ## pronoun -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" him ", replacement=" instructor ") ## pronoun -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" his ", replacement=" instructor ") ## pronoun -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" she ", replacement=" instructor ") ## pronoun -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" their ", replacement=" instructor ") ## pronoun -->> instructor
  ##__ dr, mr, mrs, ms,  -->> instructor ----
  cleanish <- gsub(x=cleanish, pattern="^dr\\.", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="^dr ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" dr\\.", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" dr ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="^miss\\. ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="^miss ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" miss\\. ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" miss ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="^mr\\. ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="^mr ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" mr\\. ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" mr ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="^mrs\\. ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="^mrs ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" mrs\\. ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" mrs ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="^ms\\. ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="^ms ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" ms\\. ", replacement=" instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern=" ms ", replacement=" instructor ") ## specific -->> instructor
  ##__ lecturer, faculty, professor, teacher -->> instructor ----
  cleanish <- gsub(x=cleanish, pattern="faculty", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="lecturer\\'s", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="lecturers", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="lecturer", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="professor\\'s", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="professors", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="professor", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="prof ", replacement="instructor ") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="teacher\\'s", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="teachers", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="teacher", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="instructor\\'s", replacement="instructor") ## specific -->> instructor
  cleanish <- gsub(x=cleanish, pattern="instructors", replacement="instructor") ## specific -->> instructor
  ##__ teaching assistants -->> TA ----
  cleanish <- gsub(x=cleanish, pattern="teaching assistant\\'s", replacement="teachAssist")
  cleanish <- gsub(x=cleanish, pattern="teaching assistants", replacement="teachAssist")
  cleanish <- gsub(x=cleanish, pattern="teaching assistant", replacement="teachAssist")
  cleanish <- gsub(x=cleanish, pattern="teaching assist", replacement="teachAssist")
  cleanish <- gsub(x=cleanish, pattern=" ta ", replacement=" teachAssist ")
  cleanish <- gsub(x=cleanish, pattern=" tas ", replacement=" teachAssist ")
  cleanish <- gsub(x=cleanish, pattern=" ta\\'s ", replacement=" teachAssist ")
  cleanish <- gsub(x=cleanish, pattern=" t\\.a ", replacement=" teachAssist ")
  cleanish <- gsub(x=cleanish, pattern=" t\\.a\\'s ", replacement=" teachAssist ")

  ## convert multiple-spaces to single-spaces ----
  cleanish <- stringr::str_replace_all(string=cleanish,
                                       pattern="[[:space:]]{2,}",
                                       replacement=" ")

  ## return the cleaned comment ----
  return(cleanish)
}




#' @title Construct Keyword Similarity Plots
#'
#' @description Construct the pairwise heatmap data for keyword area overlap
#'
#' @param keywords.similarities results of [compare.dataset()].
#'
#' @return `tibble` with the data to construct a heatmap
#' @export
#' @importFrom dplyr full_join mutate
#' @importFrom tibble as_tibble add_column
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' keywords.comp.sim <- compare.dataset(data)
#'
#' keywords.heatmap.data <- keywords.plot.data(keywords.similarities=keywords.comp.sim)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
keywords.plot.data <- function(keywords.similarities) {

  ## get the column names ----
  col.names <- colnames(keywords.similarities$Tanimoto.Indices)

  ## calculate the Tanimoto/Jaccard indices ----
  tani.tb <- tibble::as_tibble(keywords.similarities$Tanimoto.Indices)
  tani.tb <- tibble::add_column(tani.tb, "area.x"=col.names)
  tani.long <- tidyr::pivot_longer(data=tani.tb, cols=-"area.x", names_to="area.y", values_to="Tanimoto")

  ## count the number of overlapping other areas (topics) ----
  count.tb <- tibble::as_tibble(keywords.similarities$Overlap.Counts)
  count.tb <- tibble::add_column(count.tb, "area.x"=col.names)
  count.long <- tidyr::pivot_longer(data=count.tb, cols=-"area.x", names_to="area.y", values_to="count")

  ## ratio of the overlapping other areas (topics) ----
  ratio.tb <- tibble::as_tibble(keywords.similarities$Overlap.Ratios)
  ratio.tb <- tibble::add_column(ratio.tb, "area.x"=col.names)
  ratio.long <- tidyr::pivot_longer(data=ratio.tb, cols=-"area.x", names_to="area.y", values_to="ratio")
  ratio.long <- dplyr::mutate(ratio.long, pct=.data$ratio*100)

  ## construct the plot data (long) ----
  plot.data <- dplyr::full_join(x=tani.long, y=count.long)
  plot.data <- dplyr::full_join(x=plot.data, y=ratio.long)

  ## return the plot data (long) ----
  return(plot.data)
}



#' @title Find n-grams with Keywords
#'
#' @description When making network map/plot of connected terms, the need to remove
#'   cluster of terms with three or two terms is often desired. This function
#'   identifies all n-grams connected to the user provided keyword(s).
#'
#' @param data `data.frame` or `tibble` with an n-gram to search.
#' @param col.oi string with the column name of to search.
#' @param search.terms string with the keyword(s) of interest. Can be a single word
#'   (_e.g._, "cat") or multiple words (_e.g._, `c("dog", "cat")`).
#'
#' @return `data.frame` or `tibble` (depending on what is provided) with the n-gram
#'   phrases connected to the provided `search.terms` (aka keyword(s)).
#' @export
#'
#' @examples
#' \dontrun{
#' popular.DATA <- get.bigrams.oi(data=bigrams.n3,
#'                                col.oi="gram.2",
#'                                search.terms=c("development", "learning", "health", "research"))
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
get.bigrams.oi <- function(data, col.oi, search.terms) {

  string.oi <- data[[col.oi]]
  search.string.initial <- create.search.string(search.terms=search.terms)
  unique.terms <- get.unique.terms(string.oi=string.oi, search.terms=search.string.initial)
  search.string.new <- create.search.string(search.terms=unique.terms)

  n.terms.start <- n.terms.orig <- length(unique.terms)
  n.terms.total <- n.loop <- 0
  new.uniques <- NULL

  while (n.terms.start != n.terms.total) {
    n.terms.start <- length(unique.terms)
    # print(paste("start", n.terms.start))
    new.unique.terms <- get.unique.terms(string.oi=string.oi, search.terms=search.string.new)
    n.terms.new <- length(new.unique.terms)
    # print(paste("new", n.terms.new))
    # print(new.unique.terms)

    unique.terms <- sort(unique(c(unique.terms, new.unique.terms)))
    search.string.new <- create.search.string(search.terms=unique.terms)
    # print(paste("search terms", length(new.uniques)))

    n.terms.total <- length(unique.terms)
    # print(paste("total", n.terms.total))

    n.loop <- n.loop + 1
    # print(paste("loop", n.loop))

    if (n.loop == 20) { stop() }
  }

  rows.oi <- str_detect(string=string.oi, pattern=search.string.new)
  data.oi <- data[rows.oi, ]

  return(data.oi)
}


#' @title Get Unique Terms
#'
#' @description Using the provided search terms, identify the phrases containing
#'   the search terms, and return all the related terms.
#'
#' @param string.oi vector of strings to search.
#' @param search.terms vector of strings to use as the query (aka "pattern") string.
#'
#' @return string with the terms related to the query
#'
#' @examples
#' \dontrun{
#' keywords.comp.sim <- compare.dataset(data)
#'
#' keywords.heatmap.data <- keywords.plot.data(keywords.similarities=keywords.comp.sim)
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
get.unique.terms <- function(string.oi, search.terms) {

  ## create the search/query string ----
  search.string <- create.search.string(search.terms)

  ## find terms connected to the search terms ----
  related.terms <- string.oi[str_detect(string=string.oi, pattern=search.string)]
  related.terms <- sort(unique(unlist(strsplit(x=related.terms, split=" "))))

  return(related.terms)
}



#' @title Construct the Search String
#'
#' @description Construct the search string (aka "pattern") based on provided
#'   search terms. The [get.unique.terms()] and [get.all.nodes()] functions use this
#'   function. The function is a simple wrapper for the [paste()] function that
#'   surrounds each search term with `\\b` and separates multiple terms with
#'   pipes `|`.
#'
#' @param search.terms vector of strings to use as the query (aka "pattern") string.
#'
#' @return string with the terms to search the text string of interest, _e.g._,
#'   `"\\bTERM1\\b|\\bTERM2\\b"`.
#'
#' @examples
#' \dontrun{
#' search.terms <- c("dog", "cat", "fish")
#' create.search.string(search.terms)
#' "\\bdog\\b|\\bcat\\b|\\bfish\\b"
#' }
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
create.search.string <- function(search.terms) {

  search.string <- paste("\\b", search.terms, "\\b", collapse="|", sep="")

  return(search.string)
}





## /\/\/\/\ FUNCTIONS ----
## \/\/\/\/ PROTOCOLS ----

# library(tidyverse)
# library(tidytext)
# library(ggwordcloud)
# library(tm)
# library(corpus)
# library(ggraph)
# library(igraph)
# library(cowplot)
#
# ## load data and source functions & constants ----
# load(file="./data-raw/fs20_SIRS-Survey_DATA.RData")
# source('~/OneDrive - Michigan State University/theHUB/R/utilities.R')
#
# ## construct the keywords table ----
# keywords <- readr::read_csv(file="./data-raw/keywords_areas.csv", trim_ws=FALSE)
# keywords.tb <- group_by(keywords, area) |> summarise(query=paste(keyword, collapse="|"))
#
#
# ## construct dataset for testing ----
# comment.general <- select(fs20.general.data, responder.dID, Q.most.challenging.text) |>
#   rename(comment=Q.most.challenging.text)
# comment.course <- select(fs20.course.data, responder.dID, Q.overall.text) |>
#   rename(comment=Q.overall.text)
# comment.data <- bind_rows(comment.general, comment.course)
#
# ## clean dataset ----
# comment.data <- mutate(comment.data,
#                        comment.clean=clean.comment(comment),
#                        comment.unified=unified.terms(comment.clean),
#                        comment.uniStem=stemDocument(comment.unified))
#
# comment.data <- has.keywords(data=comment.data, comment.col="comment.clean", keywords.tb=keywords.tb)
#
# comment.data.summary <- comment.summary(data=comment.data, comment.col="comment.clean")
#
# comment.data <- filter(comment.data.summary, comment.present)
#
# ##_ make some plots ----
# plot.data <- select(comment.data, starts_with("n")) |> pivot_longer(cols=c("n.words", "n.areas"), names_to="names", values_to="count")
#
# areaVSwords.bin2d <- ggplot(data=comment.data, aes(x=n.words, y=factor(n.areas), fill=..count..)) +
#   stat_bin2d(aes(fill = after_stat(count)), binwidth = c(5,1)) +
#   scale_fill_gradient(low="#e7eceb", high=msu.darkGreen, space="Lab") +
#   labs(x="Number of Words in Comment",
#        y="Number of Areas Covered in Comment") +
#   theme_cowplot()
#
# areaVSwords.bin2d.inset <- ggdraw(areaVSwords.bin2d + coord_cartesian(xlim=c(0,100))) +
#   draw_plot(areaVSwords.bin2d + labs(x=NULL, y=NULL) + theme(legend.position="none"), .05, .75, .25, .25)
#
# cowplot::ggsave2(filename="~/OneDrive - Michigan State University/projects/theHUBtesting/keywordAreas_density_plot.png",
#                  areaVSwords.bin2d.inset,
#                  # width=25, height=11,
#                  width=10, height=5,
#                  units="in", dpi=300)



# map_int(comment.data$comment.unified, .f=word.count(.))

# survey.data <- select(fs20.general.data, responder.dID, Q.most.challenging.text) |>
#   mutate(clean.txt=tolower(convert.toASCII(Q.most.challenging.text))) |>
#   filter(!grepl(x=trimws(clean.txt), pattern="none") & !is.na(clean.txt))



## print stop words ----
# stop_words.df <- arrange(stop_words, word) |>
#   distinct(word, .keep_all=TRUE) |>
#   as.data.frame()
# sink(file="stopwords.txt", split=TRUE)
# stop_words.df[1:500, ]
# stop_words.df[501:728, ]
# sink()
# system("enscript -6rG -p stopwords.ps -c stopwords.txt")




## term frequency ----
# blah.singles <- unnest_tokens(comment.data, output=word, input=comment.uniStem) |>
#   count(word) |>
#   arrange(word)
#
# blah.singles.count.all.print <- filter(blah.singles, n>=5) |>
#   arrange(word) |>
#   select(word, n) |>
#   as.data.frame()
#
# sink(file="comment_1grams-uniStem_counts.txt", split=TRUE)
# blah.singles.count.all.print[1:500, ]
# blah.singles.count.all.print[501:1000, ]
# blah.singles.count.all.print[1001:1306, ]
# sink()
# system("enscript -4rG -p comment_1grams-uniStem_counts.ps -c comment_1grams-uniStem_counts.txt")
#
# ##_ ngram == 2 ----
# blah.2grams <- tidytext::unnest_tokens(comment.data, output=gram.n2, input=comment.uniStem,
#                                        token="ngrams", format="text", n=2,
#                                        to_lower=TRUE, drop=TRUE, collapse=NULL)
#
# blah.2grams.united <- tidyr::separate(blah.2grams, col="gram.n2", c("word1", "word2"), sep=" ") |>
#   # filter(!word1 %in% stop_words$word,
#   #        !word2 %in% stop_words$word) |>
#   tidyr::unite(multiGram, word1, word2, sep=" ") |>
#   select(responder.dID, multiGram) |>
#   add_column(n.words=2) |>
#   filter(multiGram != paste(rep_len("NA", length.out=2), collapse=" "))
#
# blah.2grams.count.all <- blah.2grams.united |>
#   count(multiGram, sort=TRUE) |>
#   add_column(n.words=2)
#
# blah.2grams.count.all.print <- filter(blah.2grams.count.all, n>=5) |>
#   arrange(multiGram) |>
#   select(multiGram, n) |>
#   as.data.frame()
#
# sink(file="comment_2grams-uniStem_counts.txt", split=TRUE)
# blah.2grams.count.all.print[1:500, ]
# blah.2grams.count.all.print[501:1000, ]
# blah.2grams.count.all.print[1001:1500, ]
# blah.2grams.count.all.print[1501:2000, ]
# blah.2grams.count.all.print[2001:2500, ]
# blah.2grams.count.all.print[2501:3000, ]
# blah.2grams.count.all.print[3001:3107, ]
# sink()
# system("enscript -4rG -p comment_2grams-uniStem_counts.ps -c comment_2grams-uniStem_counts.txt")
#
# ##_ ngram == 3 ----
# blah.3grams <- tidytext::unnest_tokens(comment.data, output=gram.n3, input=comment.uniStem,
#                                        token="ngrams", format="text", n=3,
#                                        to_lower=TRUE, drop=TRUE, collapse=NULL)
#
# blah.3grams.united <- tidyr::separate(blah.3grams, col="gram.n3", c("word1", "word2", "word3"), sep=" ") |>
#   # filter(!word1 %in% stop_words$word,
#   #        !word2 %in% stop_words$word) |>
#   tidyr::unite(multiGram, word1, word2, word3, sep=" ") |>
#   select(responder.dID, multiGram) |>
#   add_column(n.words=3) |>
#   filter(multiGram != paste(rep_len("NA", length.out=3), collapse=" "))
#
# blah.3grams.count.all <- blah.3grams.united |>
#   count(multiGram, sort=TRUE) |>
#   add_column(n.words=3)
#
# blah.3grams.count.all.print <- filter(blah.3grams.count.all, n>=5) |>
#   arrange(multiGram) |>
#   select(multiGram, n) |>
#   as.data.frame()
#
# sink(file="comment_3grams-uniStem_counts.txt", split=TRUE)
# blah.3grams.count.all.print[1:500, ]
# blah.3grams.count.all.print[501:1000, ]
# blah.3grams.count.all.print[1001:1400, ]
# sink()
# system("enscript -4rG -p comment_3grams-uniStem_counts.ps -c comment_3grams-uniStem_counts.txt")
#
# comments.ngram.2 <- unnest_tokens(tbl=comment.data, output=ngram.2, input=comment, token="ngrams", n=2) |>
#   filter(!is.na(ngram.2)) |>
#   separate(col=ngram.2, into=c("word.1", "word.2"), sep=" ", remove=FALSE) |>
#   # filter(!word.1 %in% stop_words$word) |>
#   # filter(!word.2 %in% stop_words$word) |>
#   count(ngram.2, sort=TRUE) |>
#   filter(n>=10)
# # enscript -3rG --line-numbers -p JCIM_2grams_counts.ps -c JCIM_2grams_counts.txt
#
# unnest_tokens(tbl=comment.data, output=ngram.3, input=comment, token="ngrams", n=3) |>
#   filter(!is.na(ngram.3)) |>
#   separate(col=ngram.3, into=c("word.1", "word.2", "word.3"), sep=" ", remove=FALSE) |>
#   # filter(!word.1 %in% stop_words$word) |>
#   # filter(!word.2 %in% stop_words$word) |>
#   count(ngram.3, sort=TRUE) |>
#   filter(n>=10)
#   # pull(n) |> table()



## 1. TERM FREQUENCY ----
##_ keyword analysis ----
##__ keyword buckets ----
# comment.data <- has.keywords(data=comment.data, comment.col="comment.clean", keywords.tb=keywords.tb)
#
# comment.summary(data=comment.data, comment.col="comment.clean")
#
# ##__ keyword similarities ----
# keyword.data <- select(comment.data, ends_with("tf"))
# keywords.similarities <- compare.dataset(keyword.data)
# keyword.plot.data <- keywords.plot.data(keywords.similarities)
# keyword.plot.data <- mutate(keyword.plot.data, pct.int=round(pct, digits=0))
#
# ##__ keyword heatmap using Tanimoto coefficients ----
# heatmap.text <- colorRampPalette(colors=c("black", "white"), bias = 1, space="rgb", interpolate="linear", alpha=FALSE)
# heatmap.text.100 <- tibble::tibble(hex=heatmap.text(101),
#                                    pct=seq(from=0, to=100, by=1))
# keyword.plot.data <- left_join(keyword.plot.data, heatmap.text.100, by=c("pct.int"="pct")) |>
#   mutate(area.x=gsub(pattern=".tf", replacement="", x=area.x),
#          area.y=gsub(pattern=".tf", replacement="", x=area.y))
#
# keyword.plot.data$area.x <- factor(x=keyword.plot.data$area.x,
#                              levels=unique(keyword.plot.data$area.x))
# keyword.plot.data$area.y <- factor(x=keyword.plot.data$area.y,
#                              levels=rev(unique(keyword.plot.data$area.y)))
#
#
#
# keyword.similarity.heatmap <- ggplot(keyword.plot.data, aes(x=area.x, y=area.y, fill=Tanimoto)) +
#   geom_raster() +
#   scale_fill_gradient(low="#FFFFFF", high="#18453B", space="rgb", aesthetics="fill") +
#   # geom_text(aes(label=count), colour="#fdae6b", fontface="bold", family="Georgia", size=5, hjust="middle", vjust="middle") +
#   # geom_label(aes(label=count), fill="#8ba29d", colour="white", family="Georgia", size=5, hjust="middle", vjust="middle") +
#   geom_label(aes(label=count), fill="white", colour="#18453B", family="Georgia", size=5, hjust="middle", vjust="middle") +
#   # scale_colour_manual(values=keyword.plot.data$hex) +
#   labs(x=NULL, y=NULL, title="Similarity Between Keyword Areas", subtitle="Number of comments from one area that mentions another area") +
#   theme_cowplot()
# # https://stackoverflow.com/questions/41541708/how-to-change-font-color-in-geom-text-in-ggplot2-in-r
#
# cowplot::ggsave2(filename="~/OneDrive - Michigan State University/projects/theHUBtesting/keywordAreas_similarity_heatmap.png",
#                  keyword.similarity.heatmap + labs(title=NULL, subtitle=NULL),
#                  # width=25, height=11,
#                  width=18, height=8,
#                  units="in", dpi=300)



##_ bigram (ngram == 2) connections ----
# comment.data <- mutate(comment.data,
#                        comment.clean=clean.comment(comment),
#                        comment.unified=unified.terms(comment.clean),
#                        comment.uniStem=stemDocument(comment.unified))
#
# blah.2grams <- tidytext::unnest_tokens(comment.data, output=gram.n2, input=comment.clean,
#                                        token="ngrams", format="text", n=2,
#                                        to_lower=TRUE, drop=TRUE, collapse=NULL)
#
# blah.2grams.counts <- tidyr::separate(blah.2grams, col="gram.n2", c("word1", "word2"), sep=" ") |>
#   filter(!word1 %in% stop_words$word) |>
#   filter(!word2 %in% stop_words$word) |>
#   count(word1, word2, sort=TRUE) |>
#   filter(!is.na(word1) | !is.na(word2))
#
# blah.2gram.graph <- blah.2grams.counts |>
#   filter(n >= 5) |>
#   graph_from_data_frame()
#
# # ggraph(blah.2gram.graph, layout="fr") +
# #   geom_edge_link() +
# #   geom_node_point() +
# #   geom_node_text(aes(label=name), vjust=1, hjust=1)
#
# # a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
# bigram.connections <- ggraph(blah.2gram.graph, layout = "fr") +
#   geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
#                  arrow = a, end_cap = circle(.07, 'inches')) +
#   geom_node_point(color = "lightblue", size = 5) +
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
#   theme_void()
# bigram.connections
#
# ggsave2(filename="bigram-uniStem_connection_n.TESTING.pdf", plot=bigram.connections, height=11, width=17, units="in")

##_ identify Unified Terms ----

## 2. CONNECTION BETWEEN TERMS/WORDS ----



## 3. CONNECTION BETWEEN COMMENTS ----
