# `netwrite` has `gridGraphics` as an implict dependency.
# However, when this package is listed under `Imports` in the `DESCRIPTION`
# file, the R CMD check will display the following note:

# * checking dependencies in R code ... NOTE
# Namespace in Imports field not imported from: ‘aaapkg’
# All declared Imports should be used.

# To avoid this, Hadley Wickham and Jennifer Bryan (https://r-pkgs.org/)
# recommend adding the below function to this package.

ignore_unused_imports <- function() {
  gridGraphics::echoGrob
}
