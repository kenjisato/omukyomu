
# omukyomu

<!-- badges: start -->
<!-- badges: end -->

The goal of omukyomu is to help useR who wants to write R scripts that are meant
to be run on other person's computers. `omukyomu::ask()` command launches an 
interactive wizard that lets the latter user to easily choose which script to
`source()`. 

## Installation

You can install the development version of omukyomu like so:

``` r
# install.packages("remotes")
remotes::install_github("kenjisato/omukyomu")
```

## Preparetion

Your scripts must be in a single directory, typically, R.

Each script should have a short explanation of the command in the following form.

```
#| This is the explanation. 
#| Lines will be collapsed into one. 
```
## Example

Ask

``` r
library(omukyomu)
ask()
```

