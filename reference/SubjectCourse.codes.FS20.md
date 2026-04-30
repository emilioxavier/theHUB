# Subject-Course Codes for Each College Since Fall 2020

A dataset of all offered courses at Michigan State University during the
fall 2020 (FS20) semester. The available courses will be updated every
semester.

## Usage

``` r
SubjectCourse.codes.FS20
```

## Format

A tibble with 986 rows and 16 columns:

- coll_code: Two-digit code for the college hosting the course; *e.g.*,
  "02". Leading zeros (`0`) are requried for single-digit college codes.

- short_name: Abbreviated or short-hand for the hosting college; *e.g.*,
  "AG NAT RES". Presented in all capital letters.

- full_name: Official name of the hosting college; *e.g.*, "Agriculture
  and Natural Resources"

- line_adr1: Street address of course, often a building on campus;
  *e.g.*, "Morrill Hall of Agriculture"

- city_name: City of course location, commonly "East Lansing"; *e.g.*,
  "East Lansing"

- state_code: State abbreviation of course location, commonly "MI" ;
  *e.g.*, "MI"

- zip_code: Zip code of course location, commonly "48823"; *e.g.*,
  "48823"

- cntry_code: Country abbreviation of course location, commonly "US" ;
  *e.g.*, "US"

- start_term_code: Unknown origin and purpose

- end_term_code: Unknown origin and purpose

- start_term_seq_id: Unknown origin and purpose

- end_term_seq_id: Unknown origin and purpose

- subj_code: Subject code; *e.g.*, "ACC"

- crse_code: Course code; *e.g.*, "250"

- index: Indexed value assigned to each subject-course based on the
  alphabetic order of the `subject_code` and the order of the
  `crse_code`.

- course: The subject code and the course number; *e.g.*, "ACC 250"

## Source

[MSU Institutional
Research](https://opb.msu.edu/functions/institution/index.html)
