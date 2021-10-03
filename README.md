
<!-- README.md is generated from README.Rmd. Please edit README.Rmd. -->
<!-- badges: start -->

[![Project Status: Active - The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/)
<!-- badges: end -->

# theHUB: the Hub Utility Box.

***the Hub Utility Box*** (***theHUB***), a collection of analysis tools
composed of functions, scripts, electronic notebooks, and worked
examples. *theHUB* is how we will share these tools with the MSU
community to improve the analyses of student outcomes, student success,
and survey responses. A standard set of processes and methods along with
a defined lexicon enables reproducibility, reduces the time needed to
complete analysis-based projects, and allows the quick addition of new
methods while encouraging data exploration. Our desire is for theHUB’s
functionality to grow over time, and for others at MSU to contribute new
features and methods for the MSU community. Developed and implemented in
R – an open-source application with a robust community (on campus and
around the world) – *theHUB* removes the need for a software license
while providing access to a myriad of statistical analysis and graphing
options. *theHUB’s* documentation allows anyone to use and understand
the tools. Initially, *theHUB* will contain three Learning Analytics
v2.0 projects: the survey analysis package presented at the 2021 Spring
Conference, *Survey Connections*, along with the *in Semester Analysis
Aiding/Assisting Students*, and *Unified Student Success and Outcomes
Analysis Reports* packages; each package is briefly described below. We
expect *theHUB* to grow as we add new, yet to be conceived, ideas and
analysis methods from the literature, our conversations with others
around campus, and conferences and seminars.

***Survey Connections***: *Analyze survey responses to gain a better
understanding of the relationships between responses and respondents.*
Traditionally, survey analysis returns a collection of response counts
and comment summaries without considering important and insightful
relationships between questions and responses. The *Survey Connections*
package provides workflows for cleaning survey data and performing
Likert and Likert-like, multi-response, and comment (text) analyses. The
package’s goal is to easily allow the discovery of relationships between
responses and the creation of cohorts for further exploration and
analysis.

***in Semester Analysis Aiding/Assisting Students*** (***iSAAS***):
*Using a student’s in-semester performance and course participation data
provides educators a method to identify those needing academic support.*
Identifying the area(s) within a course that a student needs assistance
during the semester significantly improves the student’s chance of a
favorable outcome. Currently, educators identify the area(s) of needed
improvement via MSU’s Enhancing Academic Success Early (EASE) reports.
Automating the identification of students during the semester using
gradebook data allows educators and academic units to provide students
with the required resources to improve their course standing, thus
leading to better student outcomes and success. However, we understand
that constructing detailed EASE reports can be time-consuming,
especially for educators asked to do more with fewer resources. *iSAAS*
builds on the ideas and goals of the current EASE reporting system and
is an automated system to construct detailed reports using D2L course
gradebook data, reducing educator load. In addition, the automated
nature of iSAAS allows educators to provide earlier and more frequent
reports to the student. Initially, *iSAAS* will focus on
large-enrollment, first and second-year gateway courses.

***Unified Student Success and Outcomes Analysis Reports***
(***USOAR***): *Provide academic units with unit-level learning
analytics reports while allowing them to explore student success
analysis specific to their needs and interests.* Academic units (College
or Department) have a collection of student-centric analyses they
perform to understand their students, the student’s success and
outcomes, and evaluate the unit’s academic endeavours. One expects
overlaps between the units’ analyses, yet the breadth and depth of the
analysis likely differ, creating an analytics gap. The analytics gap
turns into an analytics haves and have-nots situation because few units
on campus have full-time analytics groups. The analysis-chasm between
units will continue to expand without a resource to provide student
success and outcomes analysis. A centralized resource will also reduce
the significant logistical burden for units by leveraging their
collective knowledge into a common analytics framework that benefits all
academic units. A unified learning analytics platform allows new
analytics methods to quickly propagate to the MSU community while
reducing the analytics gap for those without dedicated student success
and outcomes analysis groups. The *USOAR* system is based on the belief
in an egalitarian analytics ecosystem where everyone (students and
academic units) benefits.

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
-   Submit an [issue via
    GitHub](https://github.com/emilioxavier/theHUB/issues/)

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/emilioxavier/sageparenting/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.

## License

MIT © [Emilio Xavier Esposito](https://github.com/emilioxavier/)
