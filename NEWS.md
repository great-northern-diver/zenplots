# zenplots 1.0.7

* fixed link to `graphNEL` from `graph` package in documentation of `graph_pairs`.

* added `mvtnorm`, `crop` to suggests in DESCRIPTION for demo; also made explicit calls to 
  `crop::dev.off.crop()` in `SP500` demo.
  
# zenplots 1.0.6

* moved package loon to the "Suggests" list in the DESCRIPTON to accommodate non-GI R use. 

# zenplots 1.0.5

* zenplots package documentation file 

  - roxygen2's @doctype package stopped adding -package alias (see roxygen2 issue 1491)

# zenplots 1.0.4

* zenplot now (invisibly) returns a list with two extra classes
  
  - `zenplot` to identify that the structure is a zenplot
  - one of `zengraphics`, `zenGrid`, or `zenLoon` to indicate which graphics
    package was used.
  - allows `loon` zenplots to be turned into ggplots via package `loon.ggplot`

  documentation updated accordingly.

* updated documentation on `zenpath` to mention that `pairs` must describe
  a connected graph.

# zenplots 1.0.3

- CRAN release 