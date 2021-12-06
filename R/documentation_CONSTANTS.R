
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


#' @title MSU Colour Palette
#'
#' @description The MSU colour palette to easily construct plots. There are three
#'   colour palettes based on the colour information found at
#'   [https://brand.msu.edu/design-visual/index.html#color](https://brand.msu.edu/design-visual/index.html#color).
#'   All colours within [theHUB] are defined using hex-codes (also known as "web")
#'   values.
#'   - msu.darkGreen (Primary Colour) Hex: ["#18453b"](https://www.color-hex.com/color/18453b)
#'   - msu.darkGreenTints: Ten tints of msu.darkGreen including "#18453b" from
#'     [https://color-hex.com/color/18453b](https://www.color-hex.com/color/18453b)
#'   - msu.heatmap.20: Twenty tints of MSU Dark Green to the lightest tint in msu.darkGreenTints
#'   - msu.heatmap.100: One-hundred tints of MSU Dark Green to the lightest tint in msu.darkGreenTints
#'     [https://www.color-hex.com/color/18453b](https://www.color-hex.com/color/18453b)
#'   - msu.palette: The thirteen colours of MSU's color palette
#'     - msu green ["#18453b"](https://www.color-hex.com/color/18453b)
#'     - kelly green ["#0db14b"](https://www.color-hex.com/color/0db14b)
#'     - grey ["#97a2a2"](https://www.color-hex.com/color/97a2a2)
#'     - orange ["#f08521"](https://www.color-hex.com/color/f08521)
#'     - teal ["#008183"](https://www.color-hex.com/color/008183)
#'     - blue-grey ["#909ab7"](https://www.color-hex.com/color/909ab7)
#'     - dark grey ["#535054"](https://www.color-hex.com/color/535054)
#'     - yellow-green ["#d1de3f"](https://www.color-hex.com/color/d1de3f)
#'     - cream ["#e8d9b5"](https://www.color-hex.com/color/e8d9b5)
#'     - texas-brown ["#c89a58"](https://www.color-hex.com/color/c89a58)
#'     - split pea soup green ["#94ae4a"](https://www.color-hex.com/color/94ae4a)
#'     - eggplant ["#6e005f"](https://www.color-hex.com/color/6e005f)
#'     - sienna ["#cb5a28"](https://www.color-hex.com/color/cb5a28)
#'
#' @format A vector with the colours of interest.
#'
#' @source MSU's Brand website:
#'   [https://brand.msu.edu/design-visual/index.html#color](https://brand.msu.edu/design-visual/index.html#color)
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
"msu.darkGreen"

#' @describeIn msu.darkGreen Ten tints of msu.darkGreen including "#18453b"
"msu.darkGreenTints"

#' @describeIn msu.darkGreen The thirteen colours of MSU's color palette
"msu.palette"

#' @describeIn msu.darkGreen Twenty tints of MSU Dark Green to the lightest tint in msu.darkGreenTints
"msu.heatmap.20"

#' @describeIn msu.darkGreen One-hundred tints of MSU Dark Green to the lightest tint in msu.darkGreenTints
"msu.heatmap.100"



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
