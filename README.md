# mvarVis

`mvarVis` is an R package for making interactive versions of the exploratory multivariate statistics plots. 
Our approach is to leverage existing packages -- currently [`FactoMineR`](factominer.free.fr), 
[`ade4`](http://pbil.univ-lyon1.fr/ade4/home.php?lang=eng), and [`vegan`](vegan.r-forge.r-project.org) -- to 
perform the actual dimension reduction, and simply add a new layer for visualizing the results of these methods.
For example, `convert_to_mvar()` converts the objects from these packages into a new S4 class, which can be directly
plotted using `plot_mvar()` or `plot_mvar_d3()` (for static and interactive graphics, respectively). Alternatively, 
numerous methods from these packages are wrapped in a new `ordi_wrapper()` function.

Example output using this package can be seen [here](http://statweb.stanford.edu/~kriss1/mvarVis_d3_examples.html) --
this example is also a package vignette. Note that these plots can be embedded in `rmarkdown` (this is possible because 
we are using [`htmlwidgets`](http://www.htmlwidgets.org/)).

The main motivation for this package was to generalize the 
[`plot_ordination()`](http://joey711.github.io/phyloseq/plot_ordination-examples) function in the 
[`phyloseq`](https://joey711.github.io/phyloseq/) package.
