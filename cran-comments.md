## Test environments
* Local macOS Sequoia 15.6.1, R 4.4.3 (release)
* win-builder (devel and release, Windows Server 2022)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Notes on this release
* Updated vignettes and examples so that suggested packages
(`qqtest`, `scagnostics`, `Rgraphviz`, `loon`, `ggplot2`)
are used conditionally.  
If not available, code paths fall back to base R equivalents or
display a message, so checks without Suggests succeed.
* Fixed DESCRIPTION and NAMESPACE so `graph` is imported only
when actually required, with guarded `requireNamespace()` checks.
* All URLs were checked with `urlchecker::url_check()`:
    - Replaced outdated ScienceDirect links with DOIs.
- Updated World Happiness Report links to current domain.
