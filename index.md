# theHUB: the Haabefuld Utility Box.

***the Haabefuld[^1],[^2] Utility Box*** (***theHUB***), a collection of
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
Evaluation](https://emilioxavier.github.io/theHUB/vignettes/AdvancedCandidateEvaluation.md)***
(***ACE***): *Analyze and summarize evaluations of candidates for
positions.* A standardized interview candidate evaluation instrument
containing a [collection of
questions](https://emilioxavier.github.io/theHUB/vignettes/AdvancedCandidateEvaluation_questions.md)
that can be augmented to include position-specific questions. The result
is a set of bespoke reports; an individual report for each candidate
along with a comparative executive summary comparing the candidates. The
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

## Installing `theHUB`

`theHUB` is *only* available on
[GitHub](https://github.com/emilioxavier/theHUB/). To install it:

``` r

# Install from GitHub:
install.packages("devtools")
install.packages(c("tidyr","tidyselect","tibble","lubridate","dplyr","ggplot2","purrr","stringr","ggrepel","cowplot","rlang","sf","digest","WriteXLS"))
devtools::install_github("emilioxavier/theHUB", force=TRUE)
```

- You will need [`devtools`](https://devtools.r-lib.org/) or
  [`remote`](https://remotes.r-lib.org/) or another package that allows
  installation from remote sources, such as
  [GitHub](https://github.com), [GitLab](https://about.gitlab.com/), or
  [BitBucket](https://bitbucket.org/), to install `theHUB` from
  [GitHub](https://github.com/emilioxavier/theHUB/).
- We have found that it is beneficial to uninstall (aka remove) the
  previous `theHUB` installation ***before*** installing an update.
- For some reason you need to ensure the **imported** packages are
  installed.

## How to use `theHUB`

``` r

library(theHUB)
```

## Have a suggestion? Need help? Found a bug?

- Contact [Emilio](https://github.com/emilioxavier/) at <emilio@msu.edu>
  OR <emilio.esposito@gmail.com>
- Submit a [request or issue via
  GitHub](https://github.com/emilioxavier/theHUB/issues/)

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://github.com/emilioxavier/theHUB/blob/master/CONDUCT.md).
By participating in this project you agree to abide by its terms.

## License

MIT © [Emilio Xavier Esposito](https://github.com/emilioxavier/)

## Footnotes

[^1]: The word haabefuld is Danish for hopeful. The **aa** is really an
    **å**, and thus, haabefuld is really håbefuld. The use of **aa**
    makes typing ([Emilio](https://github.com/emilioxavier/) can never
    remember the correct ASCII code for the å) and web searches easier.

[^2]: The Haabefuld Utility Box was originally *the Hub Utility Box*,
    *theHUB* for short. The original name – a [recursive
    backronym](https://en.wikipedia.org/wiki/Recursive_acronym) – was a
    way to include The Hub for Innovation in Learning and Technology
    where this package’s development was started. Now, *theHUB* is used
    to pay homage to the Hub.
