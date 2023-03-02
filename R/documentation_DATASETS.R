#' @title Michigan State University & Western Michigan University Peer Institutions Datasets
#'
#' @description The Michigan State University (MSU) dataset contains institutions
#'   within the BigTen, the PAC-12, are Landgrant institutions, are public Association
#'   of American Universities (AAU), [Mid-American Conference](https://getsomemaction.com/)
#'   (aka the MAC), institutions of note within the state of Michigan, and other
#'   institutions of interest.
#'
#'   The Western Michigan University (WMU) dataset is the
#'   [WMU's Institutional Research's Peer Institutions](https://wmich.edu/institutionalresearch/peers)
#'   list. The WMU Peer Institutions are based on several key parameters from the
#'   [Carnegie Classification of Institutions of Higher Education](https://carnegieclassifications.acenet.edu/).
#'
#'   The information common to each dataset:
#'   - INSTNM: Institution name
#'   - name.short: Short name for plots and reports
#'   - UNITID: IPEDS unit ID
#'   - OPEID:
#'   - OPEID6:
#'   - CITY: City
#'   - STABBR: State
#'   - ZIP: Zip code
#'   - LAT: Latitude coordinate
#'   - LON: Longitude coordinate
#'   - index: Index to aid reordering
#'   - colour.primary: Institution's primary colour
#'   - colour.secondary: Institution's secondary colour
#'
#'   The institution's primary and secondary colours were, in most cases, from
#'   the institution's branding page/website.
#'
#'   The MSU peers dataset also includes the following columns to aid the construction
#'   of subsets
#'   - BigTen.tf: BigTen institution
#'   - PAC12.tf: PAC-12 institution
#'   - MAC.tf: MAC institution
#'   - Public.tf: Public institution
#'   - Landgrant.tf: Landgrant institution (based on [List of land-grant universities](https://en.wikipedia.org/wiki/List_of_land-grant_universities) Wikipedia entry)
#'   - AAU.tf: Association of American Universities (based on [AAU provided list](https://www.aau.edu/who-we-are/our-members))
#'   - MIinsts.tf: Institutions within the state of Michigan
#'   - OIOI.tf: Other institutions of interest
#'   - MSUpeer.tf: MSU peer institution
#'
#'   The selection of the WMU peer institutions used the 2015 Carnegie Classification categories:
#'   - Control: Public
#'   - Level: Four-year or above
#'   - Basic Classification: Doctoral universities, higher research activity
#'   - Undergraduate Instructional Program: Professions plus arts & sciences, high graduate coexistence
#'   - Graduate Instructional Program: Research doctoral, comprehensive programs, no medical/veterinary school
#'   - Enrollment Profile: High undergraduate
#'   - Undergraduate Profile: Four-year, full-time, selective, higher transfer-in
#'   - Size and Setting: Four-year, large, primarily residential
#'   - Total enrollment; total research expenditures; and total doctoral degrees awarded
#'
#'   The WMU peer instititions:
#'   - [Ball State University (Muncie, IN)](http://cms.bsu.edu/)
#'   - [East Carolina University (Greenville, NC)](https://www.ecu.edu/)
#'   - [Kent State University at Kent (Kent, OH)](http://www.kent.edu/)
#'   - [Northern Arizona University (Flagstaff, AZ)](http://nau.edu/)
#'   - [Northern Illinois University (Dekalb, IL)](http://www.niu.edu/index.shtml)
#'   - [Ohio University - Main Campus (Athens, OH)](https://www.ohio.edu/)
#'   - [Oklahoma State University - Main Campus (Stillwater, OK)](http://go.okstate.edu/)
#'   - [Portland State University (Portland, OR)](http://www.pdx.edu/)
#'   - [SUNY at Binghamton (Vestal, NY)](http://www.binghamton.edu/)
#'   - [The University of Alabama (Tuscaloosa, AL)](https://www.ua.edu/)
#'   - [University of Memphis (Memphis, TN)](http://www.memphis.edu/)
#'   - [University of Nevada - Las Vegas (Las Vegas, NV)](http://www.unlv.edu/)
#'   - [University of North Carolina at Greensboro (Greensboro, NC)](http://www.uncg.edu/)
#'   - [University of North Dakota (Grand Forks, ND)](http://und.edu/)
#'   - [University of Southern Mississippi (Hattiesburg, MS)](https://www.usm.edu/)
#'
#' @format A tibble of institutions.
#'
#' @source Data sources:
#'   - Landgrant institutions (based on [List of land-grant universities](https://en.wikipedia.org/wiki/List_of_land-grant_universities) Wikipedia entry)
#'   - Association of American Universities (based on [AAU provided list](https://www.aau.edu/who-we-are/our-members))
#'   - [WMU's Institutional Research's Peer Institutions](https://wmich.edu/institutionalresearch/peers)
#'   - [Carnegie Classification of Institutions of Higher Education](https://carnegieclassifications.acenet.edu/)
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
"MSUpeers"

#' @describeIn MSUpeers Western Michigan Peer Institutions dataset
"WMUpeers"


#' @title Country and Currency Dataset
#'
#' @description The 257 International Organization for Standardization (ISO)
#'   recognized countries and the currencies they use.
#'
#' @format A tibble of countries and their associated currency.
#'
#' @source Data sources:
#'   - List of countries (via `ISOcodes::ISO_3166_1`) from ISOcodes ([CRAN](https://cran.r-project.org/web/packages/ISOcodes/))
#'   - List of currencies (via `priceR::currencies()`) from priceR ([CRAN](https://cran.r-project.org/package=priceR) & [GitHub](https://github.com/stevecondylios/priceR))
#'   - Wikipedia's [List of circulating currencies](https://en.wikipedia.org/wiki/List_of_circulating_currencies)
#'   - Individual currency [Wikipedia](https://en.wikipedia.org/) pages (too many to list)
#'   - Individaul country [Wikipedia](https://en.wikipedia.org/) pages (also, too many to list)
#'
#' @author Emilio Xavier Esposito \email{emilio@@msu.edu}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
"country.currency"
