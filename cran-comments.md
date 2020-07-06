## Release Overview

# Update to remove dependency on S4Vectors package, which is a Bioconductor package. The dependency was causing issues based on which versions were used. Also removed an another dependency that was used just for a utility function, and incoporates a bug fix.

Tested on Windows, Mac, and Linux via Travis, Appveyor, and/or Rhub.

## R CMD check results

0 errors, 0 warnings, 0 notes

## Reverse dependencies

No reverse dependencies on CRAN, checked reverse dependency on Bioconductor (lipidr). 
---