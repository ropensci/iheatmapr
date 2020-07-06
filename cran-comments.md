## Release Overview

# Update to remove dependency on S4Vectors package, which is a Bioconductor package. The dependency was causing issues based on which versions were used. Also removed an another dependency that was used just for a utility function, and incoporates a bug fix.

Tested on Windows, Mac, and Linux via Travis, Appveyor, and/or Rhub.

## R CMD check results

0 errors, 0 warnings, 2 notes

There is a note about authors with non-standard roles, 'rev'.  
These authors were reviewers for the rOpensci review process 
(see https://github.com/ropensci/onboarding/issues/107).   

There is also a note about "Unknown, possibly mis-spelled, fields" in 
DESCRIPTION. These are metadata for an ROpensci code metadata project.

## Reverse dependencies

No reverse dependencies.
---