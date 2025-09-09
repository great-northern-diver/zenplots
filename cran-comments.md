## Test environments

- Local macOS Sequoia 15.6.1, R 4.4.3
- Local macOS Sequoia 15.6.1, R Under development (2025-09-04 r88794)
- win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* NOTE (HTML manual):  
  `adjust_bb.html`, `arrow_1d_graphics.html`, etc. report `<main>` not recognized and similar HTML5 validation messages.  
  These are produced by RÕs Rd ? HTML generator. They are harmless and do not indicate an error in the package sources.


## Additional comments

* Suggested packages (`qqtest`, `loon`, `Rgraphviz`, `scagnostics`, `ggplot2`, `grid`, `lattice`, `gridExtra`) are only used conditionally in examples and vignettes.  
  All such usage is now guarded with `if (requireNamespace(..., quietly=TRUE))`.  
  If a suggested package is not available, examples/vignettes fall back to base equivalents or skip gracefully.

* Checked with `_R_CHECK_FORCE_SUGGESTS_=false` and `_R_CHECK_DEPENDS_ONLY_=true` to verify the package builds, installs, and checks cleanly when **no suggested packages** are available.

* All URLs were checked with `urlchecker::url_check()`:

  - URLs in documentation and vignettes have been updated to valid DOI or canonical forms (verified with `urlchecker`).
  - Replaced outdated ScienceDirect links with DOIs.
  - Updated World Happiness Report links to current domain.

* Package size, compilation, and vignettes all check cleanly.  

* Fixed DESCRIPTION and NAMESPACE 
    
