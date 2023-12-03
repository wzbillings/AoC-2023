# Advent of Code 2023

These are my solutions for [Advent of Code 2023](https://adventofcode.com/2023).

## Contents

* `R`: contains the solution scripts for each problem.
* `data`: contains the input files for each problem.
* All the other files are some type of documentation or housekeeping.

## Getting started

To reproduce my results, you should:

1. Clone or download the git repository.
1. Open the `.Rproj` file in RStudio (if you don't use RStudio, you have
to set the working directory correctly yourself, although I do use
`here::here()` to construct filepaths within the project).
1. Run `renv::restore()` to install the necessary dependencies at the correct
versions. (Note that if you use a different version of `R` than what is
specified by `renv`, something might break at this step. If you are using a
different version, `renv` should warn you.)
1. You can then run whichever `R` script you want. Of course, I recommend
using a clean `R` session whenever you switch between scripts.

<!-- END OF FILE -->
