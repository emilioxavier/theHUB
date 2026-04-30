# Create Project Framework

The creation of the base setup for a project. The function checks to see
if a file with `readme` in its name and if any directories (aka folders)
with the words data, document, image, and presentation are present
within the current working directory. The `README` and the folders noted
below are only created if the not already present. The script indicates
what is and is not being created.

- `00_README.md`: Provide general information about the project, what is
  being worked on, and what is completed.

- `./DATA`: Where files containing the data for the study reside.

- `./DOCUMENTS`: A location for reports, articles, and presentations
  related to the project and are often provided by colleagues.

- `./IMAGES`: Where all the images created within the scripts are
  written.

- `./PRESENTATIONS`: Where the presentations and other reports created
  for the project from the performed analyses.

- `./REFERENCES`: A location for reports, articles, and presentations
  related to the project and are often provided by colleagues.

- `./REPORTS`: Where the presentations and other reports created for the
  project from the performed analyses.

- `./RESULTS`: Where the presentations and other reports created for the
  project from the performed analyses.

The `README` file can work as an extension of your notebook by
containing links to websites and code snippets (short pieces of code to
perform specific tasks). The `README` file is constructed as a Markdown
file and allows you to provide simple text formatting, the potential to
be converted into a rendered version (PDF, webpage aka HTML, and Word
document), and is human readable and easily edited. The leading double
zeros (`00`) ensure the `README` file is always at the top of the
directory when the files are ordered by name in descending order.

This function should be run after creating the R project.

## Usage

``` r
framework.project()
```

## Value

Nothing is returned to the R session. The following ***might*** be
created in the current working directory:

- `00_README.md`

- `./DATA`

- `./DOCUMENTS`

- `./IMAGES`

- `./PRESENTATIONS`

- `./REFERENCES`

- `./REPORTS`

- `./RESULTS`

## Author

Emilio Xavier Esposito <emilio.esposito@gmail.com>
(<https://github.com/emilioxavier>)
