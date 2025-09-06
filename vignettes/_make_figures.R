# vignettes/_make_figures.R
set.seed(20250905)

ensure <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
        stop(sprintf("Package '%s' is required to (re)build figures. Install via:\n  if (!requireNamespace('BiocManager', quietly=TRUE)) install.packages('BiocManager')\n  BiocManager::install('%s')\n", pkg, pkg), call. = FALSE)
    }
}

ensure("graph"); ensure("Rgraphviz")

dir.create("vignettes/figures", showWarnings = FALSE, recursive = TRUE)

# intro.Rmd figure -----------------------------------------------------------
data("attenu")
library(PairViz)
g1 <- PairViz::mk_complete_graph(names(attenu))
ag1 <- Rgraphviz::agopen(g1, "intro_complete_graph")
png("vignettes/figures/intro_complete_graph.png", width = 800, height = 600, res = 120)
Rgraphviz::plot(ag1, "circo")
dev.off()

# selected_features.Rmd figure ----------------------------------------------

## Get olive data without geographic info
data("olive", package = "zenplots")
oliveAcids <- olive[, !names(olive) %in% c("Area", "Region")] # acids only

## Find the "convexity" scagnostic for each pair of olive acids
library(scagnostics)
Y <- scagnostics(oliveAcids) # compute scagnostics (scatter-plot diagonstics)
X <- Y["Convex",] # pick out component 'convex'
d <- ncol(oliveAcids)
M <- matrix(NA, nrow = d, ncol = d) # matrix with all 'convex' scagnostics
M[upper.tri(M)] <- X # (i,j)th entry = scagnostic of column pair (i,j) of oliveAcids
M[lower.tri(M)] <- t(M)[lower.tri(M)] # symmetrize

## Show the six pairs with largest "convexity" scagnostic:
zpath <- zenpath(M, method = "strictly.weighted") # list of ordered pairs
## Extract the corresponding pairs:
ezpath <- extract_pairs(zpath, n = c(6, 0)) # extract the first six pairs
## Reproducing Figure 7 (visualizing the pairs):
g2 <- graph_pairs(ezpath)
ag2 <- Rgraphviz::agopen(g2, "features_convexity_pairs")
png("vignettes/figures/selected_features_convexity_pairs.png", width = 900, height = 600, res = 120)
Rgraphviz::plot(ag2)
dev.off()

message("Figures written under vignettes/figures/.")
