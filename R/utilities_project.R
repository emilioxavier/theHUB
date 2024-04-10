#' @title Create Project Framework
#'
#' @description The creation of the base setup for a project. The function checks
#'   to see if a file with `readme` in its name and if any directories (aka folders)
#'   with the words data, document, image, and presentation are present within the
#'   current working directory. The `README` and the folders noted below are only
#'   created if the not already present. The script indicates what is and is not
#'   being created.
#'
#'   - `00_README.md`: Provide general information about the project, what is
#'     being worked on, and what is completed.
#'   - `./DATA`: Where files containing the data for the study reside.
#'   - `./DOCUMENTS`: A location for reports, articles, and presentations
#'     related to the project and are often provided by colleagues.
#'   - `./IMAGES`: Where all the images created within the scripts are written.
#'   - `./PRESENTATIONS`: Where the presentations and other reports created for the
#'     project from the performed analyses.
#'   - `./REFERENCES`: A location for reports, articles, and presentations
#'     related to the project and are often provided by colleagues.
#'
#' The `README` file can work as an extension of your notebook by containing links
#' to websites and code snippets (short pieces of code to perform specific tasks).
#' The `README` file is constructed as a Markdown file and allows you to provide
#' simple text formatting, the potential to be converted into a rendered version
#' (PDF, webpage aka HTML, and Word document), and is human readable and easily
#' edited. The leading double zeros (`00`) ensure the `README` file is always at
#' the top of the directory when the files are ordered by name in descending order.
#'
#' This function should be run after creating the R project.
#'
#' @export
#' @return Nothing is returned to the R session. The following _**might**_ be created
#'   in the current working directory:
#'   - `00_README.md`
#'   - `./DATA`
#'   - `./DOCUMENTS`
#'   - `./IMAGES`
#'   - `./PRESENTATIONS`
#'   - `./REFERENCES`
#'
#' @author Emilio Xavier Esposito \email{emilio.esposito@@gmail.com}
#'   ([https://github.com/emilioxavier](https://github.com/emilioxavier))
#'
framework.project <- function() {

  ## current directory information ----
  ##_ files ----
  curr.files <- list.files()
  n.files <- length(curr.files)
  ##_ directories/folders ----
  curr.dirs <- list.dirs() |>
    substr(start=3, stop=1000)
  curr.dirs <- curr.dirs[nchar(curr.dirs)>0]
  n.dirs <- length(curr.dirs)
  ##_ current directory/folder ----
  curr.path <- getwd()

  ## message to user ----
  curr.path.mess <- paste0("Working within the ", curr.path, " directory (aka folder)!\n")
  message(curr.path.mess)

  ## check for README files ----
  readme.files <- grep(pattern="readme", ignore.case=TRUE, x=curr.files, value=TRUE)
  ##_ README-like exists ----
  if ( length(readme.files) > 0 ) {
    readme.files.mess <- paste(readme.files, collapse=", ")
    readme.mess <- paste0("The following README file(s) exist: ", readme.files.mess, "\n  -->> No 00_README.md file created\n")
    message(readme.mess)
  ##_ create 00_README.md
  } else {
    file.create("00_README.md")
    readme.mess <- "NO README files exist!!\n  -->> Created 00_README.md"
    message(readme.mess)
  }

  ## check for DATA folder ----
  data.dirs <- grep(pattern="data", ignore.case=TRUE, x=curr.dirs, value=TRUE)
  ##_ DATA-like folders exists ----
  if ( length(data.dirs) > 0 ) {
    data.dirs.mess <- paste(data.dirs, collapse=", ")
    data.mess <- paste0("The following DATA folder(s) exist: ", data.dirs.mess, "\n  -->> No DATA folder created\n")
    message(data.mess)
  ##_ create DATA folder
  } else {
    dir.create("DATA")
    data.mess <- "NO DATA folders exist!!\n  -->> Created ./DATA"
    message(data.mess)
  }

  ## check for DOCUMENTS folder ----
  documents.dirs <- grep(pattern="document", ignore.case=TRUE, x=curr.dirs, value=TRUE)
  ##_ DOCUMENTS-like folders exists ----
  if ( length(documents.dirs) > 0 ) {
    documents.dirs.mess <- paste(documents.dirs, collapse=", ")
    documents.mess <- paste0("The following DOCUMENT folder(s) exist: ", documents.dirs.mess, "\n  -->> No DOCUMENTS folder created\n")
    message(documents.mess)
  ##_ create DOCUMENTS folder
  } else {
    dir.create("DOCUMENTS")
    documents.mess <- "NO DOCUMENTS folder exist!!\n  -->> Created ./DOCUMENTS"
    message(documents.mess)
  }

  ## check for IMAGES folder ----
  images.dirs <- grep(pattern="image", ignore.case=TRUE, x=curr.dirs, value=TRUE)
  ##_ IMAGES-like folders exists ----
  if ( length(images.dirs) > 0 ) {
    images.dirs.mess <- paste(images.dirs, collapse=", ")
    images.mess <- paste0("The following IMAGE folder(s) exist: ", images.dirs.mess, "\n  -->> No IMAGES folder created\n")
    message(images.mess)
  ##_ create IMAGES folder
  } else {
    dir.create("IMAGES")
    images.mess <- "NO IMAGES folder exist!!\n  -->> Created ./IMAGES"
    message(images.mess)
  }

  ## check for PRESENTATIONS folder ----
  presentations.dirs <- grep(pattern="presentation", ignore.case=TRUE, x=curr.dirs, value=TRUE)
  ##_ PRESENTATIONS-like folders exists ----
  if ( length(presentations.dirs) > 0 ) {
    presentations.dirs.mess <- paste(presentations.dirs, collapse=", ")
    presentations.mess <- paste0("The following PRESENTATION folder(s) exist: ", presentations.dirs.mess, "\n  -->> No PRESENTATIONS folder created\n")
    message(presentations.mess)
  ##_ create PRESENTATIONS folder
  } else {
    dir.create("PRESENTATIONS")
    presentations.mess <- "NO PRESENTATIONS folder exist!!\n  -->> Created ./PRESENTATIONS"
    message(presentations.mess)
  }

  ## check for REFERENCES folder ----
  references.dirs <- grep(pattern="reference", ignore.case=TRUE, x=curr.dirs, value=TRUE)
  ##_ REFERENCES-like folders exists ----
  if ( length(references.dirs) > 0 ) {
    references.dirs.mess <- paste(references.dirs, collapse=", ")
    references.mess <- paste0("The following REFERENCE folder(s) exist: ", references.dirs.mess, "\n  -->> No REFERENCES folder created\n")
    message(references.mess)
  ##_ create REFERENCES folder
  } else {
    dir.create("REFERENCES")
    references.mess <- "NO REFERENCES folder exist!!\n  -->> Created ./REFERENCES"
    message(references.mess)
  }




}
