
<!-- README.md is generated from README.Rmd. Please edit README.Rmd and knit. -->
<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.1-4baaaa.svg)](code_of_conduct.md)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/theHUB)](https://cran.r-project.org/package=theHUB)
<!-- badges: end -->

# theHUB: the Haabefuld Utility Box. <a href="https://theHopefulBox.com"><img src="man/figures/logo.png" align="right" height="139" /></a>

***the Haabefuld[1],[2] Utility Box*** (***theHUB***), a collection of
analysis tools composed of functions, scripts, electronic notebooks, and
worked examples. *theHUB* is how we will share these tools with the MSU
community to improve the analyses of student outcomes, student success,
and survey responses. A standard set of processes and methods along with
a defined lexicon enables reproducibility, reduces the time needed to
complete analysis-based projects, and allows the quick addition of new
methods while encouraging data exploration. Our desire is for theHUB’s
functionality to grow over time, and for others at MSU to contribute new
features and methods for the MSU community.

Developed and implemented in R – an open-source application with a
robust community (on campus and around the world) – *theHUB* removes the
need for a software license while providing access to a myriad of
statistical analysis and graphing options. *theHUB’s* documentation
allows anyone to use and understand the tools. Initially, *theHUB* would
contain three Learning Analytics v2.0 projects: the survey analysis
package presented at the 2021 Spring Conference, *Survey Connections*,
along with the *in Semester Analysis Aiding/Assisting Students* and
*Unified Student Success and Outcomes Analysis Reports* applications.
Currently, the *in Semester Analysis Aiding/Assisting Students* and
*Unified Student Success and Outcomes Analysis Reports* applications are
on hiatus due to a pivot in *theHUB*’s focus. We expect *theHUB* to grow
as we add new, yet to be conceived, ideas and analysis methods from the
literature, our conversations with others around campus, and conferences
and seminars.

***[Advanced Candidate
Evaluation](vignettes/AdvancedCandidateEvaluation.html)*** (***ACE***):
*Analyze and summarize evaluations of candidates for positions.* A
standardized interview candidate evaluation instrument containing a
[collection of
questions](vignettes/AdvancedCandidateEvaluation_questions.html) that
can be augmented to include position-specific questions. The result is a
set of bespoke reports; an individual report for each candidate along
with a comparative executive summary comparing the candidates. The
option to include a breakdown of the evaluators’ MAUs and job titles is
being developed. *NB: All comments are confidential and the search
committee never has access to the original Qualtrics data.*

***Survey Connections***: *Analyze survey responses to gain a better
understanding of the relationships between responses and respondents.*
Traditionally, survey analysis returns a collection of response counts
and comment summaries without considering important and insightful
relationships between questions and responses. The *Survey Connections*
package provides workflows for cleaning survey data and performing
Likert and Likert-like, multi-response, and comment (text) analyses. The
package’s goal is to easily allow the discovery of relationships between
responses and the creation of cohorts for further exploration and
analysis. See the [2021 Spring Conference
presentation](https://docs.google.com/presentation/d/1w8LXtFxP5LtnXbGBH-ZROPJrU7pvXdxYwAk1nTHJgpM/edit?usp=sharing)
demonstrating these abilities.

<!-- _**in Semester Analysis Aiding/Assisting Students**_ (_**iSAAS**_): _Using a student’s in-semester performance and course participation data provides educators a method to identify those needing academic support._ Identifying the area(s) within a course that a student needs assistance during the semester significantly improves the student’s chance of a favorable outcome. Currently, educators identify the area(s) of needed improvement via MSU’s Enhancing Academic Success Early (EASE) reports. Automating the identification of students during the semester using gradebook data allows educators and academic units to provide students with the required resources to improve their course standing, thus leading to better student outcomes and success. However, we understand that constructing detailed EASE reports can be time-consuming, especially for educators asked to do more with fewer resources. _iSAAS_ builds on the ideas and goals of the current EASE reporting system and is an automated system to construct detailed reports using D2L course gradebook data, reducing educator load. In addition, the automated nature of iSAAS allows educators to provide earlier and more frequent reports to the student. Initially, _iSAAS_ will focus on large-enrollment, first and second-year gateway courses. -->
<!-- _**Unified Student Success and Outcomes Analysis Reports**_ (_**USOAR**_): _Provide academic units with unit-level learning analytics reports while allowing them to explore student success analysis specific to their needs and interests._ Academic units (College or Department) have a collection of student-centric analyses they perform to understand their students, the student’s success and outcomes, and evaluate the unit’s academic endeavours. One expects overlaps between the units’ analyses, yet the breadth and depth of the analysis likely differ, creating an analytics gap. The analytics gap turns into an analytics haves and have-nots situation because few units on campus have full-time analytics groups. The analysis-chasm between units will continue to expand without a resource to provide student success and outcomes analysis. A centralized resource will also reduce the significant logistical burden for units by leveraging their collective knowledge into a common analytics framework that benefits all academic units. A unified learning analytics platform allows new analytics methods to quickly propagate to the MSU community while reducing the analytics gap for those without dedicated student success and outcomes analysis groups. The _USOAR_ system is based on the belief in an egalitarian analytics ecosystem where everyone (students and academic units) benefits. -->

## Installing `theHUB`

`theHUB` is *only* available on
[GitHub](https://github.com/emilioxavier/theHUB/). To install it:

``` r
# Install from GitHub:
install.packages("devtools")
devtools::install_github("emilioxavier/theHUB")
```

## How to use `theHUB`

``` r
library(theHUB)
```

## Have a suggestion? Need help? Found a bug?

-   Contact [Emilio](https://github.com/emilioxavier/) at
    <emilio@msu.edu> OR <emilio.esposito@gmail.com>
-   Submit a [request or issue via
    GitHub](https://github.com/emilioxavier/theHUB/issues/)

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/emilioxavier/theHUB/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.

## License

MIT © [Emilio Xavier Esposito](https://github.com/emilioxavier/)

## Footnotes

[1] The word haabefuld is Danish for hopeful. The **aa** is really an
**å**, and thus, haabefuld is really håbefuld. The use of **aa** makes
typing ([Emilio](https://github.com/emilioxavier/) can never remember
the correct ASCII code for the å) and web searches easier.

[2] The Haabefuld Utility Box was originally *the Hub Utility Box*,
*theHUB* for short. The original name – a [recursive
backronym](https://en.wikipedia.org/wiki/Recursive_acronym) – was a way
to include The Hub for Innovation in Learning and Technology where this
package’s development was started. Now, *theHUB* is used to pay homage
to the Hub.
