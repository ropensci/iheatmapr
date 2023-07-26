# iheatmapr 0.7.0 (CRAN Release)

* Change maintainership from Alicia Schlep to Alan O'Callaghan.

# iheatmapr 0.5.1 (CRAN Release)

* Bug fix for column annotation labels
* Bug fix for issues with merging colorbars

# iheatmapr 0.5.0 (CRAN Release)

* Remove dependencies on S4Vectors (problematic because it is a Bioconductor package) and plyr 
* Bug fix for single row or column inputs for main heatmap
 
# iheatmapr 0.4.12 (CRAN Release)

* Adjust tests to be compatible with newer version of scales package.

# iheatmapr 0.4.11

* Fix issue for some subplots where an NA value prevents any plotting.

# iheatmapr 0.4.10

* Exports to_plotly_list and adds new to_plotly_json to make it easier to convert to plotly spec

# iheatmapr 0.4.9

* Adds an option to main_heatmap to show colorbar or not, by [@fboehm](https://github.com/fboehm)

# iheatmapr 0.4.8

* Update to fix error in tests due to change in R-devel random sample
* Fix issue where add_col_groups uses continuous scale if all groups are equal
* Fix for iheatmapr_event not working on relayout 
* Fix issue where show_colorbar was always set to be TRUE
* Fix example for save_iheatmap

# iheatmapr 0.4.3

* Minor bug fix for issue on R-devel.

# iheatmapr 0.4.2

* Update tests for compatibility with new version of testthat

# iheatmapr 0.4.1

* Bug fix -- actually show titles for add_row_groups, add_col_groups, etc.

# iheatmapr 0.4.0

* Added option to customize text in tooltips for heatmaps. By default, only show
three sig figs for values.  This change will break rendering of Iheatmap objects
created via older versions of iheatmapr.

# iheatmapr 0.3.0

* Removed plotly r package dependency.  iheatmapr now directly interfaces with plotlyjs instead. While this may mean having two copies of the plotly.js library, the benefit is that the htmlwidget for iheatmapr can be customized for plots created by iheatmapr and that iheatmapr now has more control over when the js component gets updated.

# iheatmapr 0.2.1

* Added a `NEWS.md` file to track changes to the package.



