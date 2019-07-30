## ----setup, message = FALSE----------------------------------------------
# attaching required packages
library(PairViz)
library(MASS)
library(zenplots)

## ---- message = FALSE----------------------------------------------------
data(olive, package = "zenplots")

## ---- fig.align = "center", fig.width = 6, fig.height = 8----------------
zenplot(olive)

## ---- fig.align = "center", fig.width = 6, fig.height = 8----------------
zenplot(olive, plot1d = "layout", plot2d = "layout")

## ---- eval = FALSE-------------------------------------------------------
#  str(zenplot)

## ---- eval = FALSE-------------------------------------------------------
#  function (x, turns = NULL, first1d = TRUE, last1d = TRUE,
#            n2dcols = c("letter", "square", "A4", "golden", "legal"),
#            n2dplots = NULL,
#            plot1d = c("label", "points", "jitter", "density", "boxplot",
#                       "hist", "rug", "arrow", "rect", "lines", "layout"),
#            plot2d = c("points", "density", "axes", "label", "arrow",
#                       "rect", "layout"),
#            zargs = c(x = TRUE, turns = TRUE, orientations = TRUE,
#                      vars = TRUE, num = TRUE, lim = TRUE, labs = TRUE,
#                      width1d = TRUE, width2d = TRUE,
#                      ispace = match.arg(pkg) != "graphics"),
#            lim = c("individual", "groupwise", "global"),
#            labs = list(group = "G", var = "V", sep = ", ", group2d = FALSE),
#            pkg = c("graphics", "grid", "loon"),
#            method = c("tidy", "double.zigzag", "single.zigzag"),
#            width1d = if (is.null(plot1d)) 0.5 else 1,
#            width2d = 10,
#            ospace = if (pkg == "loon") 0 else 0.02,
#            ispace = if (pkg == "graphics") 0 else 0.037, draw = TRUE, ...)

## ------------------------------------------------------------------------
olive2 <- cbind(olive, olive) # just for this illustration

## ---- fig.align = "center", fig.width = 8, fig.height = 13.3-------------
zenplot(olive2, n2dcols = 6, plot1d = "layout", plot2d = "layout",
        method = "single.zigzag")

## ---- fig.align = "center", fig.width = 8, fig.height = 8----------------
zenplot(olive2, n2dcols = 6, plot1d = "layout", plot2d = "layout",
        method = "double.zigzag")

## ---- fig.align = "center", fig.width = 8, fig.height = 6.6--------------
zenplot(olive2, n2dcols = 6, plot1d = "layout", plot2d = "layout",
        method = "tidy")

## ---- fig.align = "center", fig.width = 8, fig.height = 5.4--------------
zenplot(olive2, n2dcols = 6, plot1d = "arrow", plot2d = "layout",
        method = "rectangular")

## ---- fig.align = "center", fig.width = 6, fig.height = 10---------------
zenplot(olive, plot1d = "layout", plot2d = "layout", method = "double.zigzag",
        last1d = FALSE, ispace = 0.1)

## ---- fig.align = "center", fig.width = 6, fig.height = 7----------------
zenplot(olive, plot1d = "layout", plot2d = "layout", n2dcol = 4, n2dplots = 8,
        width1d = 2, width2d = 4)

## ------------------------------------------------------------------------
(path <- 1:5)

## ------------------------------------------------------------------------
(path <- zenpath(5))

## ---- eval = FALSE-------------------------------------------------------
#  zenplot(x = dataMat[,path])

## ---- eval = FALSE-------------------------------------------------------
#  str(zenpath)

## ---- eval = FALSE-------------------------------------------------------
#  function (x, pairs = NULL,
#            method = c("front.loaded", "back.loaded", "balanced",
#                       "eulerian.cross", "greedy.weighted", "strictly.weighted"),
#            decreasing = TRUE)

## ------------------------------------------------------------------------
zenpath(5, method = "front.loaded")
zenpath(5, method = "back.loaded")
zenpath(5, method = "balanced")

## ------------------------------------------------------------------------
zenpath(c(3,5), method = "eulerian.cross")

## ---- fig.align = "center", fig.width = 6, fig.height = 9----------------
oliveAcids <- olive[, !names(olive) %in% c("area", "region")] # acids only
zpath <- zenpath(ncol(oliveAcids)) # all pairs
zenplot(oliveAcids[, zpath], plot1d = "hist", plot2d = "density")

## ---- fig.align = "center", fig.width = 8, fig.height = 7.2, eval = FALSE----
#  path <- c(1,2,3,1,4,2,5,1,6,2,7,1,8,2,3,4,5,3,6,4,7,3,8,4,5,6,7,5,8,6,7,8)
#  turns <- c("l",
#             "d","d","r","r","d","d","r","r","u","u","r","r","u","u","r","r",
#             "u","u","l","l","u","u","l","l","u","u","l","l","d","d","l","l",
#             "u","u","l","l","d","d","l","l","d","d","l","l","d","d","r","r",
#             "d","d","r","r","d","d","r","r","d","d","r","r","d","d")
#  
#  library(ggplot2) # for ggplot2-based 2d plots
#  stopifnot(packageVersion("ggplot2") >= "2.2.1") # need 2.2.1 or higher
#  ggplot2d <- function(zargs) {
#    r <- extract_2d(zargs)
#    num2d <- zargs$num/2
#    df <- data.frame(x = unlist(r$x), y = unlist(r$y))
#    p <- ggplot() +
#      geom_point(data = df, aes(x = x, y = y), cex = 0.1) +
#      theme(axis.line = element_blank(),
#            axis.ticks = element_blank(),
#            axis.text.x = element_blank(),
#            axis.text.y = element_blank(),
#            axis.title.x = element_blank(),
#            axis.title.y = element_blank())
#    if(num2d == 1) p <- p +
#      theme(panel.background = element_rect(fill = 'royalblue3'))
#    if(num2d == (length(zargs$turns)-1)/2) p <- p +
#      theme(panel.background = element_rect(fill = 'maroon3'))
#    ggplot_gtable(ggplot_build(p))
#  }
#  
#  zenplot(as.matrix(oliveAcids)[,path], turns = turns, pkg = "grid",
#          plot2d = function(zargs) ggplot2d(zargs))

## ------------------------------------------------------------------------
oliveAcids.by.area <- split(oliveAcids, f = olive$area)
# Replace the "." by " " in third group's name
names(oliveAcids.by.area)[3] <- gsub("\\.", " ", names(oliveAcids.by.area)[3])
names(oliveAcids.by.area)

## ---- fig.align = "center", fig.width = 6, fig.height = 8----------------
zenplot(oliveAcids.by.area, labs = list(group = NULL))

## ---- fig.align = "center", fig.width = 6, fig.height = 8----------------
zenplot(oliveAcids.by.area, lim = "groupwise", labs = list(sep = " - "),
        plot1d = function(zargs) label_1d_graphics(zargs, cex = 0.8),
        plot2d = function(zargs)
            points_2d_graphics(zargs, group... = list(sep = "\n - \n")))

## ---- message = FALSE----------------------------------------------------
library(scagnostics)
Y <- scagnostics(oliveAcids) # compute scagnostics (scatter-plot diagonstics)
X <- Y["Convex",] # pick out component 'convex'
d <- ncol(oliveAcids)
M <- matrix(, nrow = d, ncol = d) # matrix with all 'convex' scagnostics
M[upper.tri(M)] <- X # (i,j)th entry = scagnostic of column pair (i,j) of oliveAcids
M[lower.tri(M)] <- t(M)[lower.tri(M)] # symmetrize
round(M, 5)

## ------------------------------------------------------------------------
zpath <- zenpath(M, method = "strictly.weighted") # list of ordered pairs
head(M[do.call(rbind, zpath)]) # show the largest six 'convexity' measures

## ------------------------------------------------------------------------
(ezpath <- extract_pairs(zpath, n = c(6, 0))) # extract the first six pairs

## ---- message = FALSE, fig.align = "center", fig.width = 6, fig.height = 6----
library(graph)
plot(graph_pairs(ezpath)) # depict the six most convex pairs (edge = pair)

## ------------------------------------------------------------------------
(cezpath <- connect_pairs(ezpath)) # keep the same order but connect the pairs

## ------------------------------------------------------------------------
oliveAcids.grouped <- groupData(oliveAcids, indices = cezpath) # group data for (zen)plotting

## ---- fig.align = "center", fig.width = 6, fig.height = 8----------------
zenplot(oliveAcids.grouped)

## ------------------------------------------------------------------------
res <- zenplot(olive, plot1d = "layout", plot2d = "layout", draw = FALSE)
str(res)

## ------------------------------------------------------------------------
res[["path"]][["occupancy"]]

## ------------------------------------------------------------------------
head(res[["path"]][["positions"]])

## ------------------------------------------------------------------------
points_2d_graphics

## ------------------------------------------------------------------------
plot_region

## ------------------------------------------------------------------------
plot_indices

## ------------------------------------------------------------------------
n2dcols <- ncol(olive) - 1 # number of faces of the hypercube
stopifnot(identical(res, unfold(nfaces = n2dcols)))

