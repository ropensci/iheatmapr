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



