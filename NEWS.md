
# theHUB 2025.09

* Updated the [Code of Conduct](https://github.com/emilioxavier/theHUB/blob/master/CONDUCT.md) to version 3.0
* Updated the resources of the [PAG AI](https://thehopefulbox.com/articles/web_only/PAG_AI.html) page


# theHUB 2025.08

* Added [PAG AI](https://thehopefulbox.com/articles/web_only/PAG_AI.html) page
* Update the [PAGroup Meeting](https://thehopefulbox.com/articles/web_only/PAG_GroupMeetings.html) page
* Update the [PAGroup About](https://thehopefulbox.com/articles/web_only/PAG_About.html) page
* Added PCA biplot functions [`extract_VarExplain()`](https://thehopefulbox.com/reference/extract_VarExplain.html), [`loading_impact()`](https://thehopefulbox.com/reference/loading_impact.html), [`negMin_posMax_range()`](https://thehopefulbox.com/reference/negMin_posMax_range.html), and [`make_PCAplot_DATA()`](https://thehopefulbox.com/reference/make_PCAplot_DATA.html). Only the [`extract_VarExplain()`](https://thehopefulbox.com/reference/extract_VarExplain.html) and [`make_PCAplot_DATA()`](https://thehopefulbox.com/reference/make_PCAplot_DATA.html) are needed by the user. Need to add a biplot function
* Updated [`framework.project()`](https://thehopefulbox.com/reference/framework.project.html) to include the creation of `REPORTS` and `RESULTS` folders


# theHUB 2025.01.15

* Changed version numbering to a date format
* Added [PAG Lexicon](https://thehopefulbox.com/articles/web_only/PAG_Lexicon.html) page
* Update the [PAGroup Meeting](https://thehopefulbox.com/articles/web_only/PAG_GroupMeetings.html) page


# theHUB 1.2.9

* Rectified
  - Issue #23, #24, #25, #26, #29, #31, #32, #39, #40
* Added [`replace.colNames()`](https://thehopefulbox.com/reference/replace.colNames.html)
* Added EDW descriptions to [EDW: dBeaver Setup](https://thehopefulbox.com/articles/EDW_dBeaverSetup.html)
* Added [`geom_split_violin`](https://thehopefulbox.com/reference/geom_split_violin.html); Issue #31
* Added [`classification.check`](https://thehopefulbox.com/reference/classification.check.html)


# theHUB 1.1.0

 * Added various versions of country names to the `country.currency` [dataset](https://thehopefulbox.com/reference/country.currency.html).


# theHUB 1.0.0

* Rectified
  - Issue #2, #5, #14, #15, #17, #19, #20, #21, #22, #23
* Added [`ID.convert()`](https://thehopefulbox.com/reference/ID.convert.html) and [`ID.random()`](https://thehopefulbox.com/reference/ID.random.html) along with documentation.
* Added [Country and Currency Dataset](https://thehopefulbox.com/articles/Datasets_CountryCurrency) article and [dataset](https://thehopefulbox.com/reference/country.currency.html).
* Added [SAT to ACT Conversion Dataset](https://thehopefulbox.com/reference/ACT.2.SAT.html) and conversion functions [`convert.ACT2SAT()`](https://thehopefulbox.com/reference/convert.ACT2SAT.html) and [`convert.SAT2ACT()`](https://thehopefulbox.com/reference/convert.SAT2ACT.html). Issue #23
* The Big Ten color (or colour) palette (issue #5) is available through the [`MSUpeers` dataset](https://thehopefulbox.com/reference/MSUpeers.html) with the following command:
  > `filter(MSUpeers, BigTen.tf) |> select(name.short, starts_with("colour")) |> distinct()`
* Corrected many, many, many notes, warnings, and errors from a `R CMD check --as-cran` check.


# theHUB 0.3.0

* Rectified
  - Issue #6, #7, #8, #14
  - Issue #6 "add the PAC-12 institutions"
  - Issue #7 "add the Western Michigan Peer Institution dataset"
  - Issue #8 "create an institutions of interest dataset"
  - Issue #14 "`make.donut.data()` faceting option (via `facetBy`) is broken" (partial; step 1 of many)
* Added [PAG Group Meetings](./vignettes/web_only/PAG_GroupMeetings.html) page
* Added [Culture of Gratitude](http://thehopefulbox.com/articles/web_only/PAG_GuidelinesAndPolicies.html#culture-of-gratitude) section to [PAG Guidelines & Policies](http://thehopefulbox.com/articles/web_only/PAG_GuidelinesAndPolicies.html)
* Added all combinations of a pairwise t-test for all columns within a `data.frame` or `tibble`. This function was taken from the [exeResearch](http://exeResearch.com) package `sygKommunikation` within the private code collection of [exeResearch](https://github.com/exeResearch).
* Added [R-SQL Setup](./vignettes/web_only/EDW_R-SQLsetup.html) article


# theHUB 0.2.6

* Rectified 
  - Issue #12
* Added [PAG Onboarding](./vignettes/web_only/PAG_Onboarding.html) description
* Added [PAG About](./vignettes/web_only/PAG_About.html) description
* Added [GeoCoding: Downloading Shape Files](./vignettes/GeoCoding_DownloadShapeFiles.html)
* Added the [`convert.YN2TF()`](https://thehopefulbox.com/reference/convert.YN2TF.html) and [`clean.DATE()`](https://thehopefulbox.com/reference/clean.DATE.html) functions


# theHUB 0.2.4

* Added Predictive Analytics Group (PAG) documentation.
* Added [`dataset.summary()`](https://thehopefulbox.com/reference/dataset.summary.html) and associated functions.
* Added logo and favicon! Check out the [website](https://thehopefulbox.com)!


# theHUB 0.2.0

* Added "Western Michigan Peer Institutions" dataset.
* Added "Institutions of Interest" dataset (includes BigTen, PAC-12, MAC, Landgrant, public AAU institutions, Michigan institutions, and other institutions of interest).
* Added a `NEWS.md` file to track changes to the package.
