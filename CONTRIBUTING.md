# Contributing to iheatmapr

## Opening Issues

Please report bugs and contribute feature requests using the the Github Issues page. For reporting bugs, please provide a reproducible example. For feature requests, please give an example use case.

## Development guidelines

New features or improvements to the existing codebase or documentation are welcomed. Please use GitHub's pull request feature for proposing and submitting changes.  

New features or changes to existing features should mimic the style used for existing features. For example, if adding a new modular component that can be added onto a complex heatmap, the name of the new function should start with "add_". Additionally, iheatmapr uses the S4 OOP system, so additional functions should generally be writted as S4 methods. 

If adding a new feature, a test should be added for that new feature, as well as an update to the vignette to document the new feature.  The website documentation should also be updated using pkgdown.  

Testing is done using the testthat package. A `expect_iheatmap` function is included in helper_expectation.R in the tests/testthat directory. This function wraps a few expectations.  In particular, the expectation will create a saved version of the data that is used to create the plotly graphic.  The view_reference.Rmd function will create the plots based on all the saved data created by the `expect_iheatmap` function.  Running view_reference.Rmd can be used to visually expect the expected result of each test.

## Scope

iheatmapr is intended to be a general purpose package for creating complex, interactive heatmaps. The modular building blocks can be adapted to make domain-specific types of heatmaps, but such specialized adaptations are a better fit for complementary, add-on packages that build upon iheatmapr rather than as components of iheatmapr itself. 

## Questions about these guidelines?

Please use the Issues page for questions about these guidelines. You can also submit a preliminary pull request with questions. 

## Code of Conduct

When contributing to iheatmapr, you are expected to follow the [code of conduct](https://github.com/AliciaSchep/iheatmapr/blob/master/CONDUCT.md).