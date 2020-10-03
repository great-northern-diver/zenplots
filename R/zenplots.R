#' zenplots: Zigzag Expanded Navigation Plots
#'
#' Zenplots, like pairs plots (scatterplot matrices), lay out a large
#' number of one- and two-dimensional plots in an organized way.
#' 
#' Unlike pairs plots, zenplots can lay out a much larger number of
#' plots by pursuing a zigzagging layout (following a zenpath) of 
#' alternating one- and two-dimensional plots. 
#' 
#' The plots can be created by R's base graphics package, by the grid
#' graphics package, or even made interactive (brushing, etc.) by using
#' using the loon package.
#' 
#'
#' @docType package
#' @name zenplots
#' @useDynLib zenplots, .registration=TRUE
#' 
#' @import graphics
#' @import grid
#' 
#' @importFrom MASS kde2d
#' @importFrom tcltk tktoplevel tktitle<- tkgrid tkconfigure tkgrid.rowconfigure tkgrid.columnconfigure tkframe tkpack
#' @importFrom graph ftM2graphNEL 
#' @importFrom PairViz eseq hpaths eulerian bipartite_graph
#' @importFrom grDevices colorRampPalette contourLines hcl xy.coords
#' @importFrom stats density qnorm quantile runif na.omit as.dist median qqplot approx
#' @importFrom methods hasArg
#' @importFrom utils head tail
#' 
#' 
NULL
#> NULL