---
  title: 'iheatmapr: Interactive complex heatmaps in R'
  tags:
    - visualization
    - R
    - heatmap
  authors:
   - name: Alicia N Schep
     orcid: 0000-0002-3915-0618
     affiliation: 1,2
   - name: Sarah K Kummerfeld
     orcid: 0000-0002-0089-2358
     affiliation: 2
  affiliations:
   - name: Stanford University
     index: 1
   - name: Genentech
     index: 2
  date: 2 March 2017
  bibliography: paper.bib
---

# Summary

The iheatmapr package is an R package [@team2000r] for creating complex, interactive heatmaps. Heatmaps are commonly used to visualize patterns in high-dimensional data, particularly in genomics research. Adding annotations and summary plots along the rows or columns of a heatmap can enhance a heatmap visualization. Pairing several heatmaps horizontally can enable association of data across multiple assays. Adding interactive features like tooltips and zooming can further enhance these complex heatmaps, enabling information-rich visualizations linking diverse, high-dimensional data sets. 

There are great tools in R for creating simple interactive heatmaps [@heatmaply, @d3heatmap] or creating static complex heatmaps [@ComplexHeatmap]. However, there are no tools facilitating the creation of complex, interactive heatmaps. The iheatmapr package fills this gap, enabling creation of highly customizable, interactive, complex heatmaps using the plotly library [@plotly]. The resulting interactive visualizations can easily be incorporated into reproducible R Markdown reports for sharing with collaborators. 
  
# References