# `ngtr` package

Various R functions most focused around data science

## Install

```
remotes::install_github('stackcon/rngt')
```


## Develop

```
# Clone the repository
git clone https://github.com/stackcon/rngt.git

# ... <make changes to code> ...

# Rebuild and check package:
# ... (CLI, from project root directory) 
Rscript --vanilla -e 'roxygen2::roxygenise(clean=TRUE)'
R CMD check rngt

# ... (In R) 
roxygen2::roxygenise(clean=TRUE)
devtools::check()

```


