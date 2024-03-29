% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation_DATASETS.R
\docType{data}
\name{country.currency}
\alias{country.currency}
\title{Country and Currency Dataset}
\format{
A tibble of countries and their associated currency.
}
\source{
Data sources:
\itemize{
\item List of countries (via \code{ISOcodes::ISO_3166_1}) from ISOcodes (\href{https://cran.r-project.org/web/packages/ISOcodes/}{CRAN})
\item List of currencies (via \code{priceR::currencies()}) from priceR (\href{https://cran.r-project.org/package=priceR}{CRAN} & \href{https://github.com/stevecondylios/priceR}{GitHub})
\item List of countries (via \code{ggplot2::map_data()})
\item List of countries (via \href{https://gadm.org/index.html}{GADM maps and data version 3.6})
\item Wikipedia's \href{https://en.wikipedia.org/wiki/List_of_circulating_currencies}{List of circulating currencies}
\item Individual currency \href{https://en.wikipedia.org/}{Wikipedia} pages (too many to list)
\item Individual country \href{https://en.wikipedia.org/}{Wikipedia} pages (also, too many to list)
}
}
\usage{
country.currency
}
\description{
The 257 International Organization for Standardization (ISO)
recognized countries and the currencies they use.
}
\seealso{
Other Datasets: 
\code{\link{ACT.2.SAT}},
\code{\link{MSUpeers}}
}
\author{
Emilio Xavier Esposito \email{emilio.esposito@gmail.com}
(\url{https://github.com/emilioxavier})
}
\concept{Datasets}
\keyword{datasets}
