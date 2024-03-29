% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/documentation_DATASETS.R
\docType{data}
\name{MSUpeers}
\alias{MSUpeers}
\alias{WMUpeers}
\title{Michigan State University & Western Michigan University Peer Institutions Datasets}
\format{
A tibble of institutions.

An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 16 rows and 13 columns.
}
\source{
Data sources:
\itemize{
\item Landgrant institutions (based on \href{https://en.wikipedia.org/wiki/List_of_land-grant_universities}{List of land-grant universities} Wikipedia entry)
\item Association of American Universities (based on \href{https://www.aau.edu/who-we-are/our-members}{AAU provided list})
\item \href{https://wmich.edu/institutionalresearch/peers}{WMU's Institutional Research's Peer Institutions}
\item \href{https://carnegieclassifications.acenet.edu/}{Carnegie Classification of Institutions of Higher Education}
}
}
\usage{
MSUpeers

WMUpeers
}
\description{
The Michigan State University (MSU) dataset contains institutions
within the BigTen, the PAC-12, are Landgrant institutions, are public Association
of American Universities (AAU), \href{https://getsomemaction.com/}{Mid-American Conference}
(aka the MAC), institutions of note within the state of Michigan, and other
institutions of interest.

The Western Michigan University (WMU) dataset is the
\href{https://wmich.edu/institutionalresearch/peers}{WMU's Institutional Research's Peer Institutions}
list. The WMU Peer Institutions are based on several key parameters from the
\href{https://carnegieclassifications.acenet.edu/}{Carnegie Classification of Institutions of Higher Education}.

The information common to each dataset:
\itemize{
\item INSTNM: Institution name
\item name.short: Short name for plots and reports
\item UNITID: IPEDS unit ID
\item OPEID:
\item OPEID6:
\item CITY: City
\item STABBR: State
\item ZIP: Zip code
\item LAT: Latitude coordinate
\item LON: Longitude coordinate
\item index: Index to aid reordering
\item colour.primary: Institution's primary colour
\item colour.secondary: Institution's secondary colour
}

The institution's primary and secondary colours were, in most cases, from
the institution's branding page/website.

The MSU peers dataset also includes the following columns to aid the construction
of subsets
\itemize{
\item BigTen.tf: BigTen institution
\item PAC12.tf: PAC-12 institution
\item MAC.tf: MAC institution
\item Public.tf: Public institution
\item Landgrant.tf: Landgrant institution (based on \href{https://en.wikipedia.org/wiki/List_of_land-grant_universities}{List of land-grant universities} Wikipedia entry)
\item AAU.tf: Association of American Universities (based on \href{https://www.aau.edu/who-we-are/our-members}{AAU provided list})
\item MIinsts.tf: Institutions within the state of Michigan
\item OIOI.tf: Other institutions of interest
\item MSUpeer.tf: MSU peer institution
}

The selection of the WMU peer institutions used the 2015 Carnegie Classification categories:
\itemize{
\item Control: Public
\item Level: Four-year or above
\item Basic Classification: Doctoral universities, higher research activity
\item Undergraduate Instructional Program: Professions plus arts & sciences, high graduate coexistence
\item Graduate Instructional Program: Research doctoral, comprehensive programs, no medical/veterinary school
\item Enrollment Profile: High undergraduate
\item Undergraduate Profile: Four-year, full-time, selective, higher transfer-in
\item Size and Setting: Four-year, large, primarily residential
\item Total enrollment; total research expenditures; and total doctoral degrees awarded
}

The WMU peer instititions:
\itemize{
\item \href{http://cms.bsu.edu/}{Ball State University (Muncie, IN)}
\item \href{https://www.ecu.edu/}{East Carolina University (Greenville, NC)}
\item \href{http://www.kent.edu/}{Kent State University at Kent (Kent, OH)}
\item \href{http://nau.edu/}{Northern Arizona University (Flagstaff, AZ)}
\item \href{http://www.niu.edu/index.shtml}{Northern Illinois University (Dekalb, IL)}
\item \href{https://www.ohio.edu/}{Ohio University - Main Campus (Athens, OH)}
\item \href{http://go.okstate.edu/}{Oklahoma State University - Main Campus (Stillwater, OK)}
\item \href{http://www.pdx.edu/}{Portland State University (Portland, OR)}
\item \href{http://www.binghamton.edu/}{SUNY at Binghamton (Vestal, NY)}
\item \href{https://www.ua.edu/}{The University of Alabama (Tuscaloosa, AL)}
\item \href{http://www.memphis.edu/}{University of Memphis (Memphis, TN)}
\item \href{http://www.unlv.edu/}{University of Nevada - Las Vegas (Las Vegas, NV)}
\item \href{http://www.uncg.edu/}{University of North Carolina at Greensboro (Greensboro, NC)}
\item \href{http://und.edu/}{University of North Dakota (Grand Forks, ND)}
\item \href{https://www.usm.edu/}{University of Southern Mississippi (Hattiesburg, MS)}
}
}
\section{Functions}{
\itemize{
\item \code{WMUpeers}: Western Michigan Peer Institutions dataset

}}
\seealso{
Other Datasets: 
\code{\link{ACT.2.SAT}},
\code{\link{country.currency}}
}
\author{
Emilio Xavier Esposito \email{emilio.esposito@gmail.com}
(\url{https://github.com/emilioxavier})
}
\concept{Datasets}
\keyword{datasets}
