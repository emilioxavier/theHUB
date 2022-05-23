
#' @title  Subject-Course Codes for Each College Since Fall 2020
#'
#' @description A dataset of all offered courses at Michigan State University during the
#'   fall 2020 (FS20) semester. The available courses will be updated every semester.
#'
#' @format A tibble with 986 rows and 16 columns:
#'  - coll_code: Two-digit code for the college hosting the course; _e.g._, "02".
#'     Leading zeros (`0`) are requried for single-digit college codes.
#'  - short_name: Abbreviated or short-hand for the hosting college; _e.g._,
#'     "AG NAT RES". Presented in all capital letters.
#'  - full_name: Official name of the hosting college; _e.g._,
#'     "Agriculture and Natural Resources"
#'  - line_adr1: Street address of course, often a building on campus; _e.g._,
#'     "Morrill Hall of Agriculture"
#'  - city_name: City of course location, commonly "East Lansing"; _e.g._, "East Lansing"
#'  - state_code: State abbreviation of course location, commonly "MI" ; _e.g._, "MI"
#'  - zip_code: Zip code of course location, commonly "48823"; _e.g._, "48823"
#'  - cntry_code: Country abbreviation of course location, commonly "US" ; _e.g._, "US"
#'  - start_term_code: Unknown origin and purpose
#'  - end_term_code: Unknown origin and purpose
#'  - start_term_seq_id: Unknown origin and purpose
#'  - end_term_seq_id: Unknown origin and purpose
#'  - subj_code: Subject code; _e.g._, "ACC"
#'  - crse_code: Course code; _e.g._, "250"
#'  - index: Indexed value assigned to each subject-course based on the alphabetic
#'     order of the `subject_code` and the order of the `crse_code`.
#'  - course: The subject code and the course number; _e.g._, "ACC 250"
#'
#' @aliases SubjectCourse.codes
#'
#' @source [MSU Institutional Research](https://opb.msu.edu/functions/institution/index.html)
"SubjectCourse.codes.FS20"


#' @title Keywords and Areas of Interest
#'
#' @description A collection of keywords (206 unique keywords) and associated
#'   areas (16)
#'
#' @format A tibble with 206 rows and 2 columns:
#'  - keyword: Word
#'  - area:
#'
#' @aliases keyword.areas
#'
#' @source List of keywords and associated areas as determined by Emilio. Please
#'   feel free to offer suggestions to expand keywords and areas covered.
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
"keywords"


#' @title Calculate Latitude-Longitude Distance Constants
#'
#' @description Constants needed to calculate the distance between two latitude-
#'   longitude points on a globe, aka earth.
#'
#'   - `deg2rad` | Degrees to Radians: `0.0174532925199433` for the
#'   conversion of degrees (specifically latitude and longitude values) to
#'   radians. Needed for calculating the distance between two locations on the
#'   globe using latitude-longitude values.
#'
#'  \loadmathjax
#'  \mjdeqn{ deg2rad = \frac{\pi}{180} }{deg2rad = pi/180}
#'
#'  - `earth.radius.miles` | Earth's Radius: `3963.17` miles.
#'  - `earth.radius.km` | Earth's Radius: `6373` kilometers.
#'
#' @format numeric (double)
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
"deg2rad"

#' @describeIn deg2rad | Earth's Radius
"earth.radius.miles"

#' @describeIn deg2rad | Earth's Radius
"earth.radius.km"


#' @title Term-Code Translation Table
#'
#' @description Term-code translation table used to convert four-digit term-code
#'   into term and four-digit year. The `term.translation` constant contains
#'   the following information. Used in the function [convert.termCode()].
#'
#'   | **Term abbreviation** | **Full Term Name** | **Short Term Name** |
#'   |:-------------------:|:-------------:|:--------------:|
#'   |   1  | Winter Quarter (WinterQ) |  WQ  |
#'   |   2  | Spring | SS |
#'   |   3  | Spring Quarter (SpringQ) | SpQ  |
#'   |   5  | Summer | SU |
#'   |   6  | Summer Quarter | SuQ |
#'   |   8  | Fall   | FS |
#'   |   9  | Fall Quarter (FallQ) | FQ |
#'
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
"term.translation"
