# HMCBLR

Code to sample from a logistic regression posterior with Gaussian prior distributions on the parameters. Please see function documentation and vignettes for example usage.

## Installation

Simply run: `devtools::install_github('rchan26/HMCBLR')`.

Note that there are vignettes available, but by default `devtools::install_github()` does not build them since they are time consuming. To force building, use `devtools::install_github('rchan26/HMCBLR', build_vignettes = TRUE)`.

## Development workflow

If any code has been modified, call:

```
Rcpp::compileAttributes()
pkgbuild::compile_dll()
devtools::document()
devtools::install()
```

To build vignettes use:
```
devtools::build_vignettes()
```

Or, you can use
```
devtools::build()
```
to create a package bundle with the vignettes included.

## License

Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg
