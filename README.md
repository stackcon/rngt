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

# Rebuild documentation and package metadata (from root directory)
Rscript --vanilla -e 'roxygen2::roxygenise(clean=TRUE)'

# Check package (from parent directory)
R CMD check rngt
```


