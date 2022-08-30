## Resubmission
This is a re-submission of the brisk package (first time CRAN package).

In this version I have:

* Edited the description field of DESCRIPTION so it does not start with "Provides functions".
* Added a reference in the description field of DESCRIPTION.
* Added a \value field to the sim_weights.Rd file.
* Use tempdir() in tests to avoid writing to a relative file path in
  the package structure.
* Added inst/WORDLIST

# R CMD check results
There were no ERRORs, WARNINGs or NOTEs on the following platforms:

* MacOS 11.6.8, R 4.2.1
* Ubuntu 20.04.4, R 4.2.1

Results can be viewed at:
*  https://github.com/rich-payne/brisk/actions/workflows/R-CMD-check.yaml

There were no ERRORs or WARNINGs, with one NOTE for Windows
(x86_64-w64-mingw32) with R Under development (unstable):

NOTE
Maintainer: 'Richard Payne <paynestatistics@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  MCDA (18:25, 22:60)
  Mussen (18:38)
  Salek (18:46)
  univariate (16:30)

# Downstream dependencies
There are no downstream dependencies.
