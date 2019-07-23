## By Marius Hofert

## This script investigates tail dependence in constituent data of the S&P 500.
## It takes 3h to run on standard hardware (2016; can differ substantially).


### Setup ######################################################################

library(rugarch)
library(ADGofTest)
library(qqtest)
library(zoo)
library(Matrix)
library(pcaPP)
library(copula)
library(mvtnorm)
library(zenplots)
library(lattice)
library(qrmdata)
library(qrmtools)
doPNG <- require(crop)

## Set working directory (to find existing objects!)
## usr <- Sys.getenv("USER")
## if(usr == "mhofert") setwd("../../misc") else if (usr == "rwoldford")
##            setwd("/Users/rwoldford/Documents/Software/zenplots/pkg/misc")


### 0 Auxiliary functions ######################################################

##' @title Panel Function to Display Sectors and Subsectors
##' @param x matrix
##' @param sectors sector division points
##' @param subsectors subsector division points
##' @return (panel) function
##' @author Marius Hofert
sector_panel_function <- function(x, sectors, subsectors,
                                  colsec="black", colsubs="black",
                                  lwdsec=1, lwdsubs=0.2) {
    function(...){
        panel.levelplot(...)
        ## Sectors
        for(s in sectors) {
            panel.lines(x = c(s, s), y = c(s, nrow(x)), col = colsec, lwd=lwdsec, lty = 2)
            panel.lines(x = c(1, s), y = c(s, s), col = colsec, lwd=lwdsec, lty = 2)
        }
        ## Subsectors
        for(s in subsectors) {
            panel.lines(x = c(s, s), y = c(s, nrow(x)), col = colsubs, lwd = lwdsubs)
            panel.lines(x = c(1, s), y = c(s, s), col = colsubs, lwd = lwdsubs)
        }
    }
}

##' @title Compute Pairwise Rosenblatt Transformed Data Based on a Fitted Full
##'        or Bivariate t Copulas and Compute Pairwise p-values via an AD Test
##'        after Mapping to a K_2
##' @param U pseudo-observations
##' @param P correlation matrix of the fitted (full or pairwise) t copula(s)
##' @param nu matrix of degrees of freedom of the fitted (full or pairwise) t copula(s)
##' @return list with
##'         1) an (i,j,n,2)-array containing the row index i, col index j,
##'            U[,i] and C(U[,j] | U[,i])
##'         2) an (i,j,.)-array containing the corresponding p-values
##'            (of the AD tests after mapping the Rosenblatt transformed data
##'             to a K_2 distribution)
##'         3) a (d,d)-matrix containing the p-values
##' @author Marius Hofert
##' Note: We should actually work with gofCopula(, method = "Sn"), but see the
##'       the note in Section 7.2 below. Also, gofCopula() is probably much slower
pw_test <- function(U, P, Nu)
{
    stopifnot(is.matrix(U))
    n <- nrow(U)
    d <- ncol(U)
    stopifnot(is.matrix(P),  dim(P)  == c(d, d),
              is.matrix(Nu), dim(Nu) == c(d, d))

    ## Setup
    nms <- colnames(U)
    URosen <- array(, dim = c(d*(d-1), n, 2)) # (i, j), U_i, C(U_j|U_i)
    FUN <- function(q) pchisq(2*q*q, df = 2) # df of a K_2 distribution
    pvals <- matrix(, nrow = d*(d-1), ncol = 3) # i, j, p-value
    pb <- txtProgressBar(max = d*(d-1), style = if(isatty(stdout())) 3 else 1) # setup progress bar
    on.exit(close(pb)) # on exit, close progress bar
    k <- 0 # counter over all pairs

    ## Compute pairwise Rosenblatt transformed data and p-values
    for(i in 1:d) { # row index
        for(j in 1:d) { # column index
            if(i != j) {
                k <- k + 1
                ## Test
                u <- U[, c(i,j)]
                tc <- tCopula(P[i,j], df = Nu[i,j])
                U. <- cCopula(u, copula = tc) # Rosenblatt transformed data
                URosen[k,,] <- U. # save as result
                pvals[k,] <- c(i, j, ad.test(sqrt(rowMeans(qnorm(U.)^2)), # (*)
                                             distr.fun = FUN)$p.value) # compute p-values; equivalent ordering results when 'statistic' is used
                ## (*): Careful: Can be Inf multiple times (in which case p-values of
                ##               7.94702e-07 result) for pairwise Rosenblatt transformed
                ##               data based on the pairwise fitted models
                ## Update progress bar
                setTxtProgressBar(pb, k) # update progress bar
            }
        }
    }

    ## Build matrix of p-values
    pvalsMat <- matrix(, ncol = d, nrow = d)
    k <- 0
    for(i in 1:d) {
        for(j in 1:d) {
            if(i != j) {
                k <- k + 1
                pvalsMat[i,j] <- pvals[k,3]
            }
        }
    }

    ## Return
    list(URosen = URosen, pvals = pvals, pvalsMat = pvalsMat)
}

##' @title Maximal p-value Over Ljung--Box Tests for Various Lags
##' @param x univariate time series
##' @param lag.max maximal lag
##' @param p fitted AR order
##' @param q fitted MA order
##' @return maximal p-value
##' @author Marius Hofert
##' @note The order of the pairs may depend on whether p-values or the
##'       Ljung--Box test statistics are used as the latter depends on
##'       the (thirty considered) lag(s).
LB_test <- function(x, lag.max = 30, p = 1, q = 1) {
    lag <- seq_len(lag.max)
    df <- rep(0, lag.max)
    df[lag > p + q] <- p + q
    min(vapply(lag, FUN = function(l)
        Box.test(x, lag = l, type = "Ljung-Box", fitdf = df[l])$p.value,
        FUN.VALUE = NA_real_))
}


### 1 Basic data manipulation and building risk-factor changes #################

## Read the S&P 500 constituent data (composition of the S&P 500 as of 2016-01-03)
data("SP500_const") # load the constituents data from qrmdata
time <- c("2007-01-03", "2009-12-31") # time period
x <- SP500_const[paste0(time, collapse = "/"),] # data
sectors <- SP500_const_info$Sector # sectors
ssectors <- SP500_const_info$Subsector # subsectors

## Detecting NAs
if(doPNG)
png(file = (file <- paste0("fig_SP500_NA.png")),
    width = 7.5, height = 6, units = "in", res = 300, bg = "transparent")
NA_plot(x)
if(doPNG) dev.off.crop(file)

## Keep the time series with at most 20% missing data and fill NAs
keep <- apply(x, 2, function(x.) mean(is.na(x.)) <= 0.2) # keep those with <= 20% NA
x <- x[, keep] # data we keep
percentNA <- round(100 * apply(x, 2, function(x.) mean(is.na(x.)))) # % of NAs for those with <= 20% NA
percentNA[percentNA > 0] # => DAL (11%), DFS (15%), TEL (15%), TWC (1%)
sectors <- sectors[keep] # corresponding sectors
ssectors <- ssectors[keep] # corresponding subsectors
if(doPNG)
png(file = (file <- paste0("fig_SP500_NA<=0.2.png")),
    width = 7.5, height = 6, units = "in", res = 300, bg = "transparent")
NA_plot(x) # => only very little NA (yet still introduces interesting shape in some pobs below)
if(doPNG) dev.off.crop(file)
colnames(x)[apply(x, 2, function(x) any(is.na(x)))] # components with <= 20% NA
x <- na.fill(x, fill = "extend") # fill NAs
stopifnot(all(!is.na(x)))


### 2 Fitting marginal ARMA(1,1)-GARCH(1,1) models #############################

## We first fit ARMA(1,1)-GARCH(1,1) time series models to each margin and extract
## the standardized residuals which can then be investigated
## for cross-sectional dependence (de-GARCHing). Since the marginal fitting of so
## many time series models can fail, we use the fail-safe implementation
## fit_ARMA_GARCH() in qrmtools. The appearing warnings for six time series can
## be ignored (they come from finding initial values).
file <- paste0("SP500_",paste0(time, collapse = "--"),"_X_Z_nu_sectors_ssectors.rda")
if(file.exists(file)) {
    load(file)
} else {
    X <- -returns(x) # -log-returns
    uspec <- rep(list(ugarchspec(distribution.model = "std")), ncol(X))
    system.time(fit.ARMA.GARCH <- fit_ARMA_GARCH(X, ugarchspec.list = uspec)) # ~ 2.5min
    ## Note: Without the default 'solver = "hybrid"', fitting for component
    ##       297 throws the warning...
    ## Warning message:
    ## In .sgarchfit(spec = spec, data = data, out.sample = out.sample,  :
    ## ugarchfit-->warning: solver failer to converge.
    ## ... and extracting the standardized residuals below fails!
    stopifnot(sapply(fit.ARMA.GARCH$error, is.null)) # NULL = no error
    ## Display warnings
    fit.ARMA.GARCH$warning[!sapply(fit.ARMA.GARCH$warning, is.null)]
    ## => 6x warnings:
    ##    Warning message:
    ##    In arima(data, order = c(modelinc[2], 0, modelinc[3]), include.mean = modelinc[1],  :
    ##      possible convergence problem: optim gave code = 1
    ## => Comes from finding initial values and can be ignored here.
    fits <- fit.ARMA.GARCH$fit # fitted models
    resi <- lapply(fits, residuals, standardize = TRUE) # grab out standardized residuals
    Z <- as.matrix(do.call(merge, resi)) # standardized residuals
    stopifnot(is.matrix(Z), nrow(Z) == 755, ncol(Z) == 465) # fail-safe programming
    colnames(Z) <- colnames(x)
    nu.margins <- vapply(fits, function(x) x@fit$coef[["shape"]], NA_real_) # vector of estimated df
    save(X, Z, nu.margins, sectors, ssectors, file = file, compress = "xz") # save computed objects
}
n <- nrow(X) # 755
d <- ncol(X) # 465
stopifnot(n == 755, d == 465) # fail-safe programming


### 3 Pseudo-observations ######################################################

## Order the data according to business sectors (only sectors; not subsectors;
## this order gives an easier way to describe zenplots in the paper)
ord. <- order(sectors) # order according to sectors
sec. <- sectors[ord.] # sectors in sector order
ssec. <- ssectors[ord.] # subsectors in sector order
usec. <- unique(sec.) # unique sectors
nsec. <- sapply(usec., function(s) sum(sec. == s)) # sector sizes
dsec. <- head(cumsum(nsec.), n = -1) # sector division points
## Compute pseudo-observations sorted according to sectors
U. <- pobs(Z[,ord.])

## A few scatterplots to start with
if(doPNG)
    png(file = (file <- paste0("fig_SP500_Unif_U12_U23_U34.png")),
        width = 16, height = 4.35, units = "in", res = 200, bg = "transparent")
opar <- par(no.readonly = TRUE)
layout(matrix(1:4, nrow = 1), widths = rep(1, 4), heights = 1)
set.seed(271)
plot(matrix(runif(n*2), ncol = 2), pch = 19, xlab = "U(0,1)", ylab = "U(0,1)",
     col = adjustcolor("black", alpha.f = 0.4))
plot(U.[,1:2], pch = 19, col = adjustcolor("black", alpha.f = 0.4))
plot(U.[,2:3], pch = 19, col = adjustcolor("black", alpha.f = 0.4))
plot(U.[,3:4], pch = 19, col = adjustcolor("black", alpha.f = 0.4))
par(opar)
if(doPNG) dev.off.crop(file)

## In more than two or three dimensions, one typically uses a scatterplot matrix
## to visualize the pseudo-observations. As the following plot shows, as the dimensions increase
## (here about 5% of 465), the pairs plot becomes crowded with superfluous
## information; it would be even harder to see anything interesting without alpha
## blending and, especially, in higher dimensions.
## A scatterplot matrix of the first 22 components only
if(doPNG)
    png(file = (file <- paste0("fig_SP500_pobs_splom.png")),
        width = 10, height = 10, units = "in", res = 600, bg = "transparent")
pairs(U.[,1:22], gap = 0, cex = 0.1, cex.labels = 0.9, oma = rep(0.2, 4),
      col = adjustcolor("black", alpha.f = 0.5), xaxt="n", yaxt="n")
if(doPNG) dev.off.crop(file)

## Note that we have far too many pairs to be visible in a scatterplot matrix!
## A zenplot, visualizing some of the pairs of variables, makes more sense here.
## These pairs are, by default, $(1,2), (2,3), (3,4), ..., (d-1,d)$ (with
## $d$ denoting the dimension) but will be chosen in a more meaningful way below.
if(doPNG)
    png(file = (file <- paste0("fig_SP500_pobs_zenplot.png")),
        width = 10, height = 10, units = "in", res = 600, bg = "transparent")
zenplot(U., n2dcols = 23, ospace = 0,
        plot1d = function(zargs) label_1d_graphics(zargs, cex = 0.5),
        plot2d = function(zargs) points_2d_graphics(zargs, cex = 0.1,
                                                    col = adjustcolor("black", alpha.f = 0.5)))
if(doPNG) dev.off.crop(file)

## From a risk management perspective, for example, the pairs we are most
## interested in are those with the strongest tail dependence, and possibly
## also those with the weakest tail dependence (to get a feeling for the range
## of tail dependence found in the data). Our goal is thus to find and display
## (just) these pairs.

## Order the data according to business sectors *and* subsectors
## In what follows, we exclusively work with these fully ordered components
ord <- order(sectors, ssectors) # order according to sectors and subsectors
sec <- sectors[ord] # sectors in sector and subsector order
ssec <- ssectors[ord] # subsectors in sector and subsector order
## Sector information
usec <- unique(sec) # unique sectors
nsec <- sapply(usec, function(s) sum(sec == s)) # sector sizes
dsec <- head(cumsum(nsec), n = -1) # sector division points
## Subsector information
ussec <- unique(ssec) # unique subsectors
nssec <- sapply(ussec, function(s) sum(ssec == s)) # subsector sizes
dssec <- head(cumsum(nssec), n = -1) # subsector division points

## Compute pseudo-observations sorted according to sectors and subsectors
U <- pobs(Z[,ord])

## Plot several/many/all pairs of pseudo-observations (164 zenplots!)
if(FALSE) {
    zpath.all <- zenpath(465) # all pairs
    l <- length(zpath.all)
    n2dcol <- 23
    n2drow <- 30
    nplots <- ceiling(l/((n2dcol-1)*n2drow)) # number of plots (note: each row has one plot less)
    gr <- rep(1:nplots, each = (n2dcol-1)*n2drow)[1:l]
    zpath.all.splt <- split(zpath.all, f = gr)
    U.lst <- group(U, indices = zpath.all.splt)
    max.nplots <- nplots # adjust here
    stopifnot(1 <= max.nplots, max.nplots <= nplots)
    ## Loop (~ 30s/plot, 38MB each) => 82min, 6.2GB
    for(i in 1:max.nplots) {
        if(doPNG)
            png(file = (file <- paste0("fig_SP500_pobs_zenplot_all_",i,".png")),
                width = 10, height = 13, units = "in", res = 600, bg = "transparent")
        zenplot(U.lst[[i]], n2dcols = 23, ospace = 0,
                plot1d = function(zargs) label_1d_graphics(zargs, cex = 0.5),
                plot2d = function(zargs) points_2d_graphics(zargs, cex = 0.1,
                                                            col = adjustcolor("black", alpha.f = 0.5)))
        if(doPNG) dev.off() # omit cropping here to save time
    }
}


### 4 Computing pairwise estimators of the tail-dependence matrix ##############

### 4.1 Non-parametric estimators (conditional Spearman's rho) #################

## Non-parametric estimator based on conditional Spearman's rho
file <- paste0("SP500_",paste0(time, collapse = "--"),"_LamPWnp.rda")
if(file.exists(file)) {
    load(file)
} else {
    ## Pairwise nonparametric estimating of the upper tail-dependence coefficient
    system.time(LamPWnp <- fitLambda(U, p = 0.1, lower.tail = FALSE, verbose = TRUE)) # ~ 2min
    diag(LamPWnp) <- NA

    ## Save object
    save(LamPWnp, file = file, compress = "xz")
}

## Plot of Lambda in 'sectorial' order with sectors and subsectors
if(doPNG)
    png(file = (file <- paste0("fig_SP500_Lambda_pw_nonparametric.png")),
        width = 7.5, height = 6, units = "in", res = 600, bg = "transparent")
matrix_plot(LamPWnp, at = seq(0, 1, length.out = 200),
            col.regions = grey(c(seq(1, 0, length.out = 200))),
            panel = sector_panel_function(LamPWnp, sectors = dsec, subsectors = dssec))
if(doPNG) dev.off.crop(file)

## Density of all pairwise lambdas
if(doPNG)
    png(file = (file <- paste0("fig_SP500_lambdas_pw_nonparametric.png")),
        width = 7.5, height = 6, units = "in", res = 200, bg = "transparent")
matrix_density_plot(LamPWnp, xlim = 0:1,
                    xlab = expression("Pairwise"~lambda[U]~"(conditional Spearman's rhos)"))
if(doPNG) dev.off.crop(file)


### 4.2 Parametric estimators (bivariate t copulas) ############################

## Fit t copulas to all pairs of columns of pseudo-observations with the
## approach of Mashal, Zeevi (2002)
file <- paste0("SP500_",paste0(time, collapse = "--"),"_PPW_NuPW_LamPW.rda")
if(file.exists(file)) {
    load(file)
} else {
    ## Pairwise fitting of t copulas
    system.time(res <- fitLambda(U, method = "t", lower.tail = FALSE, verbose = TRUE)) # ~ 1.5h

    ## Symmetrize matrices (diagonals remain NA)
    LamPW <- res$Lam
    diag(LamPW) <- NA
    PPW <- res$P
    diag(PPW) <- NA
    NuPW <- res$Nu

    ## Save objects
    save(PPW, NuPW, LamPW, file = file, compress = "xz")
}

## Plot of Lambda in 'sectorial' order with sectors and subsectors
if(doPNG)
    png(file = (file <- paste0("fig_SP500_Lambda_pw.png")),
        width = 7.5, height = 6, units = "in", res = 600, bg = "transparent")
matrix_plot(LamPW, at = seq(0, 1, length.out = 200),
            col.regions = grey(c(seq(1, 0, length.out = 200))),
            panel = sector_panel_function(LamPWnp, sectors = dsec, subsectors = dssec,
                                          colsec="black", lwdsec=0.8, lwdsubs=0.2, colsubs="black"))
if(doPNG) dev.off.crop(file)

## Density of all pairwise lambdas
if(doPNG)
    png(file = (file <- paste0("fig_SP500_lambdas_pw.png")),
        width = 7.5, height = 6, units = "in", res = 200, bg = "transparent")
matrix_density_plot(LamPW, xlim = 0:1,
                    xlab = expression("Pairwise"~lambda[U]~"(bivariate t copulas)"))
if(doPNG) dev.off.crop(file)

## Density of all degrees of freedom
if(doPNG)
    png(file = (file <- paste0("fig_SP500_nus_pw.png")),
        width = 7.5, height = 6, units = "in", res = 200, bg = "transparent")
matrix_density_plot(NuPW, log = "x",
                    xlab = "Pairwise degrees of freedom (bivariate t copulas)")
abline(v = 12.98722, lty = 2) # d.o.f. of the fitted full model (see below)
legend("topright", bty = "n", lty = 1:2,
       legend = c("Density",
                  as.expression(substitute("Joint t copula"~hat(nu)==nu.,
                                           list(nu. = 12.98)))))
if(doPNG) dev.off.crop(file)
whch <- NuPW > 999
sum(whch, na.rm = TRUE) / choose(465, 2) * 100 # % pairs with estimated d.o.f. > 999 (1.57%)
summary(LamPW[whch]) # => they are (numerically correctly treated as) 0

if(FALSE) {
    ## The following is known/clear, but still quite impressive to see
    ## Plot of Lambda in the original (unsorted) order of the components
    num <- (1:ncol(x))[ord] # column numbers ordered according to sectors and subsectors
    oord <- order(num) # indices to go back to original order
    stopifnot(colnames(U[, oord]) == colnames(SP500_const[, keep])) # basic sanity check
    LamPWunsort <- LamPW[oord, oord] # LamPW in the original, unsorted order
    matrix_plot(LamPWunsort, at = seq(0, 1, length.out = 200),
                col.regions = grey(c(seq(1, 0, length.out = 200))))
}


### 5 Extreme pairs ############################################################

### 5.1 Largest and smallest among all pairs ###################################

## Build a list containing the k most and least extreme lambdas
## Note: zenpath also accepts other objects, like
##       as.vector(LamPW[lower.tri(LamPW)]) for which then, internally,
##       the default argument 'pairs' is automatically built correctly.
k <- 10 # bottom and top number of pairs (k most extreme pairs)
zpath <- zenpath(LamPW, method = "strictly.weighted") # zenpath with decreasing weights over all pairs
czpath <- connect_pairs(zpath) # connected zpath
zp <- extract_pairs(czpath, n = k) # extract top/bottom k pairs

## Zenplot of pseudo-observations of these k most and least extreme pairs
U.groups <- group(U, indices = zp)
if(doPNG)
    png(file = (file <- paste0("fig_SP500_pobs_largest_smallest_lambdas_pw.png")),
        width = 7.5, height = 12, units = "in", res = 600, bg = "transparent")
zenplot(U.groups, n2dcols = 5, ospace = 0, ispace = 0.01,
        plot2d = function(zargs) points_2d_graphics(zargs, box = TRUE,
                                                    col = adjustcolor("black", alpha.f = 0.4)),
        labs = list(group = "Path ", var = "V", sep = ", "))
if(doPNG) dev.off.crop(file)


### 5.2 Largest among all (cross-)pairs (= pairs whose components belong to
###     different groups) ######################################################

## Idea: We use the zenpath over all pairs, extract those pairs lying in
##       different groups and extract the most extremes among them.

## Build a matrix indicating whether a pair (i,j) lies in the different sectors
diffGrp <- matrix(TRUE, nrow = d, ncol = d)
sec.ind <- c(0, dsec, d) # 0, 78, 111,..., 436, 465
for(i in 1:(length(sec.ind)-1)) {
    ii <- (sec.ind[i]+1) : sec.ind[i+1]
    if(FALSE)
        print(c(head(colnames(U[,ii]), n = 1), tail(colnames(U[,ii]), n = 1)))
    diffGrp[ii, ii] <- FALSE
}

## Pick out those pairs along the zenpath which lie in different sectors
l <- 0
zp <- zpath
for(i in 1:length(zpath)) {
    pair <- zpath[[i]]
    if(diffGrp[pair[1], pair[2]]) {
        l <- l+1
        zp[[l]] <- zpath[[i]]
    }
}
zp <- zp[1:l]

## Pick out largest k pairs and (try to) connect them
zp <- connect_pairs(extract_pairs(zp, n = c(k, 0)))

## Zenplot of pseudo-observations of these k most and least extreme pairs
U.groups <- group(U, indices = zp)
if(doPNG)
    png(file = (file <- paste0("fig_SP500_pobs_largest_lambdas_pw_cross_sectors.png")),
        width = 7.5, height = 9, units = "in", res = 600, bg = "transparent")
zenplot(U.groups, n2dcols = 5, ospace = 0, ispace = 0.01,
        plot2d = function(zargs) points_2d_graphics(zargs, box = TRUE,
                                                    col = adjustcolor("black", alpha.f = 0.4)),
        labs = list(group = "Path ", var = "V", sep = ", "))
if(doPNG) dev.off.crop(file)


### 5.3 Among all pairs within a group (for all groups) ########################

## We can also look at the pseudo-observations of the pairs with largest
## pairwise tail dependencies *within each* of the 10 sectors. If the corresponding
## pairs of variables are connected, draw all connected pairs; otherwise, only draw
## the pairs corresponding to largest tail dependence within that group.

## Build a list containing the most extreme lambdas per sector (sectors are
## ordered in their original order, so lexicographically)
LamPWlst <- lapply(usec, function(s) LamPW[sec == s, sec == s]) # correct
zpath <- vector("list", length = length(usec))
for(s in seq_len(length(usec))) {
    zp <- connect_pairs(zenpath(LamPWlst[[s]], method = "strictly.weighted"))[[1]] # grab out most extreme 'sub-path'
    zpath[[s]] <- zp + if(s >= 2) dsec[s-1] else 0 # shift by 'previous sectors' to grab out the right variables
    ## Note: We could have used zenpath's extract feature to extract more
    ##       extreme pairs but then we would have obtained a list of lists
    ##       which cannot be dealt with easily by zenplot().
}

## Zenplot of pseudo-observations of these extreme pairs per sector
U.groups <- group(U, indices = zpath)
if(doPNG)
    png(file = (file <- paste0("fig_SP500_pobs_largest_lambdas_pw_per_sector.png")),
        width = 7.5, height = 12, units = "in", res = 600, bg = "transparent")
zenplot(U.groups, n2dcols = 5, ospace = 0, ispace = 0.01,
        plot2d = function(zargs) points_2d_graphics(zargs, box = TRUE,
                                                    col = adjustcolor("black", alpha.f = 0.4)),
        labs = list(group = "GICS ", var = "V", sep = ", "))
if(doPNG) dev.off.crop(file)


### 6 Fitting a proper multivariate t copula ###################################

## Fit a d-dimensional t copula to the pseudo-observations with the approach of
## Mashal, Zeevi (2002)
file <- paste0("SP500_",paste0(time, collapse = "--"),"_t_cop.rda")
if(file.exists(file)) {
    load(file)
} else {
    tc <- tCopula(dim = d, dispstr = "un")
    system.time(cop <- fitCopula(tc, data = U, method = "itau.mpl")) # ~ 50s
    save(cop, file = file, compress = "xz")
}
nu <- tail(cop@estimate, n = 1) # estimated degrees of freedom nu
Nu <- p2P(nu, d = d) # corresponding matrix
p <- head(cop@estimate, n = -1) # estimated correlation coefficients
P <- p2P(p, d = d) # estimated correlation matrix
tc <- tCopula(p, dim = d, dispstr = "un", df = nu) # fitted t copula
Lam <- p2P(lambda(tc)[1:(d*(d-1)/2)], d = d) # 2 * pt(- sqrt((nu + 1) * (1 - P) / (1 + P)), df=nu + 1)
diag(Lam) <- NA # to be comparable to the other matrices

## Check that the estimated correlation matrices are close
## Note: They are not the same because the full model nearPD() (via fitCopula())
PPW. <- PPW
diag(PPW.) <- 1
stopifnot(all.equal(P, PPW., tol = 0.0003))
if(FALSE)
    matrix_plot(P-PPW.)

## Plot of Lambda (in 'sectorial' order as above)
if(doPNG)
    png(file = (file <- paste0("fig_SP500_Lambda_full.png")),
        width = 7.5, height = 6, units = "in", res = 600, bg = "transparent")
matrix_plot(Lam, at = seq(0, 1, length.out = 200),
            col.regions = grey(c(seq(1, 0, length.out = 200))),
            panel = sector_panel_function(LamPWnp, sectors = dsec, subsectors = dssec,
                                          colsec="black", lwdsec=0.8, lwdsubs=0.2, colsubs="black"))
if(doPNG) dev.off.crop(file)

## Compare Lam with LamPW in a scatter plot
lamPW <- LamPW[lower.tri(LamPW)]
lam  <- Lam[lower.tri(Lam)]
if(doPNG)
    png(file = (file <- paste0("fig_SP500_Lambda_full_vs_Lambda_pw.png")),
        width = 6, height = 6, units = "in", res = 200, bg = "transparent")
par(pty = "s")
plot(lamPW, lam, xlim = 0:1, ylim = 0:1,
     xlab = expression("Pairwise"~lambda[U]~"(bivariate t copulas)"),
     ylab = expression("Pairwise"~lambda[U]~"(t copula)"),
     pch = 19, cex=0.5,col = adjustcolor("black", 0.5)
     # pch = ".", col = "black"  # An alternative to the above
     )
abline(b = 1, a = 0, lwd = 0.5)
if(doPNG) dev.off.crop(file)

## Compare pobs of Lam and lamPW
if(doPNG)
    png(file = (file <- paste0("fig_SP500_Lambda_full_vs_Lambda_pw_pobs.png")),
        width = 6, height = 6, units = "in", res = 200, bg = "transparent")
par(pty = "s")
plot(pobs(cbind(lamPW, lam)), xlim = 0:1, ylim = 0:1,
     xlab = expression("Pseudo-observations of pairwise"~lambda[U]~"(bivariate t copulas)"),
     ylab = expression("Pseudo-observations of pairwise"~lambda[U]~"(t copula)"),
     pch = ".", col = adjustcolor("black", alpha.f = 0.4))
if(doPNG) dev.off.crop(file)

## Compare Lam with LamPW in a density plot
dists <- LamPW-Lam # in [-1,1]
madists <- max(abs(dists), na.rm = TRUE)
xran <- c(-madists, madists)
if(doPNG)
    png(file = (file <- paste0("fig_SP500_lambdas_abs_differences_full_vs_pw.png")),
        width = 7.5, height = 6, units = "in", res = 200, bg = "transparent")
matrix_density_plot(dists, xlim = xran,
                    xlab = expression("Pairwise"~lambda[U]~"differences (bivariate t copulas minus full t copula)"))
if(doPNG) dev.off.crop(file)

## Density of all pairwise implied lambdas
if(doPNG)
    png(file = (file <- paste0("fig_SP500_lambdas_full.png")),
        width = 7.5, height = 6, units = "in", res = 200, bg = "transparent")
matrix_density_plot(Lam, xlim = 0:1,
                    xlab = expression("Pairwise"~lambda[U]~"(t copula)"))
if(doPNG) dev.off.crop(file)


### 7 Goodness-of-fit ##########################################################

### 7.1 Marginal ARMA(1,1)-GARCH(1,1) models ###################################

### 7.1.1 Marginally test for autocorrelation of the standardized residuals Z ##

## Compute p-values for the Ljung--Box test and order Z accordingly
doSquared <- FALSE # TRUE
Z. <- if(doSquared) Z^2 else Z
pvals.margins.LB <- sapply(1:d, function(j) LB_test(Z.[,j])) # compute p-values
ord <- order(pvals.margins.LB) # order
Z.ord <- Z.[, ord] # order standardized residuals according to p-values (names also ordered)
nu.margins.ord <- nu.margins[ord] # sort degrees of freedom accordingly
pvals.margins.ord <- pvals.margins.LB[ord] # sort p-values accordingly

## Auxiliary function for zenplot() below
acf_2d <- function(zargs, pvalues, nus, lag.max = 30, ...)
{
    ## Extract information
    x <- zargs$x
    turns <- zargs$turns
    num <- zargs$num/2
    z <- x[, num, drop = FALSE]
    lab <- colnames(z)
    nu <- nus[num]
    pval <- pvalues[num]

    ## ACF plot
    acf(z, lag.max = lag.max, main = "", xlab = "", ylab = "", ...)

    ## Labels
    opar <- par(usr = c(0, 1, 0, 1)) # to get coordinates in [0,1]
    text(x = 0.95, y = 0.9, labels = lab, adj = 1)
    ## text(x = 0.95, y = 0.9, labels = substitute(hat(nu) == df, list(df = round(nu, 2))),
    ##      adj = 1)
    ## pval.r <- round(pval, 4)
    ## text(x = 0.4, y = 0.9,
    ##      labels = if (pval.r == 0) paste0("SL < 0.0001") else paste0("SL = ", pval.r))
    par(opar)
    invisible()
}

## Zenplot of the ACFs for those margins with smallest LB test p-values
n.col <- 4 # number of columns in the layout
n.row <- 4 # number of rows in the layout
fname <- if(doSquared) "fig_SP500_gof_margins_LB_squared.png" else "fig_SP500_gof_margins_LB.png"
if(doPNG)
    png(file = (file <- paste0(fname)),
        width = 8.4, height = 8.4, units = "in", res = 200, bg = "transparent")
zenplot(Z.ord[,1:(n.col*n.row+1)], # fill it completely
        n2dcols = n.col, method = "rectangular",
        last1d = FALSE, ospace = 0.03, ispace = c(0.17, 0.17, 0, 0), cex = 0.1,
        plot1d = "arrow", plot2d = function(zargs, ...) {
            acf_2d(zargs, pvalues = pvals.margins.ord, nus = nu.margins.ord, ...)
})
if(doPNG) dev.off.crop(file)


## Checking the time series which had NA

## Specify the fitted model
uspec <- ugarchspec(distribution.model = "std")

## Components with NA in (0%, 20%)
percentNA[percentNA > 0]

## DAL
x. <- x[,"DAL"]
plot(x., type = "l") # => fine
x.. <- -returns(x.)
plot(x.., type = "l") # => fine
fit.. <- ugarchfit(uspec, data = x.., solver = "hybrid")
Z.. <- as.numeric(residuals(fit.., standardize = TRUE))
plot(Z.., type = "l") # ... suddenly a peak
day <- index(x..)[which(Z.. < -1e5)]
x..[c(day-2, day-1, day)] # ... at the first trading day
acf(Z..) # ... but ACF not affected
## Check out lag plot
if(doPNG)
    png(file = (file <- paste0("fig_SP500_ACF_problem_DAL_lag_plot.png")),
        width = 6, height = 6, units = "in", res = 300, bg = "transparent")
lZ.. <- length(Z..)
opar <- par(pty = "s")
plot(Z..[2:lZ..], Z..[1:(lZ..-1)], xlab = expression(Z[t+1]), ylab = expression(Z[t]))
abline(a = 0, b = 1, lty = 2, col = "gray60")
par(opar)
if(doPNG) dev.off.crop(file)

## DFS
x. <- x[,"DFS"]
plot(x., type = "l") # => fine
x.. <- -returns(x.)
plot(x.., type = "l") # => fine
fit.. <- ugarchfit(uspec, data = x.., solver = "hybrid")
Z.. <- as.numeric(residuals(fit.., standardize = TRUE))
plot(Z.., type = "l") # ... suddenly a peak
day <- index(x..)[which(Z.. < -1e5)]
x..[c(day-2, day-1, day)] # ... at the first trading day
acf(Z..) # ... but ACF not affected
## Check out lag plot
if(doPNG)
    png(file = (file <- paste0("fig_SP500_ACF_problem_DFS_lag_plot.png")),
        width = 6, height = 6, units = "in", res = 300, bg = "transparent")
lZ.. <- length(Z..)
opar <- par(pty = "s")
plot(Z..[2:lZ..], Z..[1:(lZ..-1)], xlab = expression(Z[t+1]), ylab = expression(Z[t]))
abline(a = 0, b = 1, lty = 2, col = "gray60")
par(opar)
if(doPNG) dev.off.crop(file)

## TWC
x. <- x[,"TWC"]
plot(x., type = "l") # => fine
x.. <- -returns(x.)
plot(x.., type = "l") # => fine
fit.. <- ugarchfit(uspec, data = x.., solver = "hybrid")
Z.. <- as.numeric(residuals(fit.., standardize = TRUE))
plot(Z.., type = "l") # => fine
acf(Z..) # => fine

## TEL
x. <- x[,"TEL"]
plot(x., type = "l") # => fine
x.. <- -returns(x.)
plot(x.., type = "l") # => fine
fit.. <- ugarchfit(uspec, data = x.., solver = "hybrid")
Z.. <- as.numeric(residuals(fit.., standardize = TRUE))
if(doPNG)
    png(file = (file <- paste0("fig_SP500_ACF_problem_TEL_Z.png")),
        width = 6, height = 6, units = "in", res = 300, bg = "transparent")
opar <- par(pty = "s")
plot(index(x..), Z.., type = "l", xlab = "t",
     ylab = expression("Standardized residuals"~~Z[t])) # ... suddenly a peak
par(opar)
if(doPNG) dev.off.crop(file)
day <- index(x..)[which(Z.. > 10)]
x..[c(day-2, day-1, day)] # ... at the first *two* trading days
acf(Z..) # ... ACF *is* affected
## Check out lag plot
if(doPNG)
    png(file = (file <- paste0("fig_SP500_ACF_problem_TEL_lag_plot.png")),
        width = 6, height = 6, units = "in", res = 300, bg = "transparent")
lZ.. <- length(Z..)
opar <- par(pty = "s")
plot(Z..[2:lZ..], Z..[1:(lZ..-1)], xlab = expression(Z[t+1]), ylab = expression(Z[t]))
abline(a = 0, b = 1, lty = 2, col = "gray60")
par(opar)
if(doPNG) dev.off.crop(file)

## How about just using an ARMA(1,1)?
uspec <- ugarchspec(variance.model = list(garchOrder = c(0, 0)), distribution.model = "std")
fit.. <- ugarchfit(uspec, data = x.., solver = "hybrid")
Z.. <- as.numeric(residuals(fit.., standardize = TRUE))
plot(Z.., type = "l") # => fine
acf(Z..) # => fine
## Check out lag plot (=> fine)
if(doPNG)
    png(file = (file <- paste0("fig_SP500_ACF_problem_TEL_lag_plot_pure_arma.png")),
        width = 6, height = 6, units = "in", res = 300, bg = "transparent")
lZ.. <- length(Z..)
opar <- par(pty = "s")
plot(Z..[2:lZ..], Z..[1:(lZ..-1)], xlab = expression(Z[t+1]), ylab = expression(Z[t])) # => fine
abline(a = 0, b = 1, lty = 2, col = "gray60")
par(opar)
if(doPNG) dev.off.crop(file)

## Note: When only using a GARCH(1,1), there is only one spike (much larger)
##       which does not affect the ACF.


### 7.1.2 Marginally test the assumption of Z being t ##########################

## Compute p-values for the AD test and order Z accordingly
pvals.margins.AD <- sapply(1:d, function(j)
    ad.test(Z[,j], distr.fun = function(q)
        pt(sqrt(nu.margins[j]/(nu.margins[j]-2)) * q, df = nu.margins[j]))$p.value) # note: equivalent ordering results when 'statistic' is used (with order(, decreasing = TRUE) below)
ord <- order(pvals.margins.AD) # order
Z.ord <- Z[, ord] # order standardized residuals according to p-values (names also ordered)
nu.margins.ord <- nu.margins[ord] # sort degrees of freedom accordingly
pvals.margins.ord <- pvals.margins.AD[ord] # sort p-values accordingly

## Auxiliary function for zenplot() below
qqtest_t_2d <- function(zargs, pvalues, nus, nreps, ...)
{
    ## Extract information
    x <- zargs$x
    turns <- zargs$turns
    num <- zargs$num/2
    z <- x[, num, drop = FALSE]
    lab <- colnames(z)
    nu <- nus[num]
    pval <- pvalues[num]

    ## Q-Q plot
    qqtest(z, qfunction = function(p) sqrt((nu-2)/nu) * qt(p, df = nu), # theoretical quantiles
           legend = FALSE, nreps = nreps, col = "black",
           ann = FALSE, axes = FALSE, frame.plot = TRUE, ...) # x-axis = theoretical quantiles; y-axis = sample quantiles

    ## Labels
    opar <- par(usr = c(0, 1, 0, 1)) # to get coordinates in [0,1]
    text(x = 0.92, y = 0.1, labels = substitute(hat(nu) == df~~~~~~L,
                                                list(df = round(nu, 2), L = lab)),
         adj = 1)
    ## pval.r <- round(pval, 4)
    ## text(x = 0.4, y = 0.07,
    ##      labels = if (pval.r == 0) paste0("SL < 0.0001") else paste0("SL = ", pval.r))
    par(opar)
    invisible()
}

## Zenplot of the Q-Q plots for those margins with smallest AD test p-values
n.col <- 4 # number of columns in the layout
n.row <- 4 # number of rows in the layout
set.seed(271) # set a seed (for qqtest())
if(doPNG)
    png(file = (file <- paste0("fig_SP500_gof_margins_AD.png")),
        width = 7, height = 7, units = "in", res = 200, bg = "transparent")
zenplot(Z.ord[,1:(n.col*n.row+1)], # fill it completely
        n2dcols = n.col, method = "rectangular",
        last1d = FALSE, ospace = 0, cex = 0.1,
        plot1d = "arrow", plot2d = function(zargs, ...) {
            qqtest_t_2d(zargs, pvalues = pvals.margins.ord, nus = nu.margins.ord, nreps = 1000, ...)
            abline(0, 1, col = adjustcolor("black", 0.5), lwd = 0.5)
})
if(doPNG) dev.off.crop(file)


### 7.2 Single test: Full model ################################################

## Note:
## 1) We should actually work with gofCopula(tc, x = U, method = "Sn"), but:
##    Error in .gofPB(copula, x, N = N, method = method, estim.method = estim.method,  :
##      Param. boostrap with 'method'="Sn" not available for t copulas as pCopula() cannot be computed for non-integer degrees of freedom yet.
## 2) The next best would be gofCopula(tc, x = U, method = "SnB", trafo.method = "cCopula") but:
##    0% => Error in optim(start, loglikCopula, lower = lower, upper = upper, method = optim.method,  :
##    non-finite finite-difference value [1]
## => We stick to a ad.test() based on the Rosenblatt-transformed
##    pseudo-observations here

## Single AD test based on full (d-dimensional) model
URosenFull <- cCopula(U, copula = tc) # Rosenblatt transformation; ~ 1.5min
stopifnot(0 <= URosenFull, URosenFull <= 1) # sanity check
## Problem: URosenFull can be 0 (14x) or 1 (3732x) => use pobs() to avoid
URosenFull. <- pobs(URosenFull)
stopifnot(0 < URosenFull., URosenFull. < 1) # sanity check
URosenFull.K <- sqrt(rowMeans(qnorm(URosenFull.)^2)) # map to a K_d
pK <- function(q, d) pchisq(d*q*q, df = d) # df of a K_d distribution
(pvalFull <- ad.test(URosenFull.K, distr.fun = pK, d = d)$p.value) # compute p-value; equivalent ordering results when 'statistic' is used
## Note: - ~ 8 * 10^{-7} => rejection!
##       - The formal test based on the K_d distribution is actually the same
##         as the test based on the chi_d^2 distribution

## Plot
if(doPNG)
    png(file = (file <- paste0("fig_SP500_gof_full.png")),
        width = 6, height = 6, units = "in", res = 200, bg = "transparent")
qqtest(URosenFull.K, dist = "kay", df = d, nreps = 1000, pch = 1,
       col = adjustcolor("black", alpha.f = 0.5), main = "", cex = 0.3,
       xlab = substitute(K[dof]~"quantiles", list(dof = d))) # envelope = FALSE, nexemplars = 5,
if(doPNG) dev.off.crop(file)

## Do the same for random permutation of the order of the variables
set.seed(271)
URosenFull. <- cCopula(U[,sample(1:ncol(U))], copula = tc) # Rosenblatt transformation; ~ 1.5min
stopifnot(0 <= URosenFull., URosenFull. <= 1) # sanity check
## Problem: URosenFull. can be 0 (15x) or 1 (4073x) => use pobs() to avoid
URosenFull.. <- pobs(URosenFull.)
stopifnot(0 < URosenFull.., URosenFull.. < 1) # sanity check
URosenFull.K. <- sqrt(rowMeans(qnorm(URosenFull..)^2)) # map to a K_d
(pvalFull. <- ad.test(URosenFull.K., distr.fun = pK, d = d)$p.value) # compute p-value; equivalent ordering results when 'statistic' is used
## Note: - ~ 8 * 10^{-7} => rejection, too

## Plot
if(doPNG)
    png(file = (file <- paste0("fig_SP500_gof_full_randomized.png")),
        width = 6, height = 6, units = "in", res = 200, bg = "transparent")
qqtest(URosenFull.K., dist = "kay", df = d, nreps = 1000, pch = 1,
       col = adjustcolor("black", alpha.f = 0.5), main = "", cex = 0.3,
       xlab = substitute(K[dof]~"quantiles", list(dof = d)))
if(doPNG) dev.off.crop(file)


### 7.3 Pairwise tests: With full ("wrong dof?") and pairwise fitted models
###     ("is the data not even any t?") ########################################

### 7.3.1 Compute the objects ##################################################

## Compute pairwise Rosenblatt transformed data based on the *full* fitted model
file <- paste0("SP500_",paste0(time, collapse = "--"),"_URosenPWFull_pvalsPWFull_pvalsMatPWFull.rda")
if(file.exists(file)) {
    load(file) # ~ 30s
} else {
    system.time(res <- pw_test(U = U, P = P, Nu = Nu)) # ~ 30min
    URosenPWFull <- res$URosen # object.size(URosenPWFull) ~= 2.6GB
    pvalsPWFull <- res$pvals
    pvalsMatPWFull <- res$pvalsMat
    save(URosenPWFull, pvalsPWFull, pvalsMatPWFull, file = file, compress = "xz") # takes ~ 15min; 1.1GB
}

## Compute pairwise Rosenblatt transformed data based on the pairwise fitted models
file <- paste0("SP500_",paste0(time, collapse = "--"),"_URosenPWPW_pvalsPWPW_pvalsMatPWPW.rda")
if(file.exists(file)) {
    load(file)
} else {
    system.time(res <- pw_test(U = U, P = PPW, Nu = NuPW)) # ~ 30min
    URosenPWPW <- res$URosen
    pvalsPWPW <- res$pvals
    pvalsMatPWPW <- res$pvalsMat
    save(URosenPWPW, pvalsPWPW, pvalsMatPWPW, file = file, compress = "xz") # takes ~ 15min; 1.1GB
}


### 7.3.2 Tests/analysis #######################################################

if(FALSE) {
    ## Plot the matrix of p-values for the pairwise tests based on the full/pairwise model(s)
    matrix_plot(pvalsMatPWFull, at = c(0, 0.05, 1), col.regions = c("black", "white")) # full model
    matrix_plot(pvalsMatPWPW,   at = c(0, 0.05, 1), col.regions = c("black", "white")) # pairwise models
}

## Check whether they lead to the same rejections at 5%
rejPWFull <- pvalsPWFull
rejPWFull[,3] <- pvalsPWFull[,3] < 0.05
rejPWPW <- pvalsPWPW
rejPWPW[,3] <- pvalsPWPW[,3] < 0.05
same <- rejPWFull[,3] == rejPWPW[,3] # length d*(d-1)
mean(same) * 100 # ~ 99.99397% => almost always the same decision

## Do all rejections of bivariate t models also lead to rejections of the full model?
stopifnot(! any(rejPWPW[,3] & !rejPWFull[,3]))
## => ok, all rejections of bivariate t models also lead to rejections of the full model

## For which pairs is the full model rejected but not the bivariate?
ii <- which(rejPWFull[,3] & !rejPWPW[,3])
prs <- rejPWFull[ii,1:2]
t(apply(prs, 1, function(pair) colnames(U)[pair]))
## => 13 pairs, 12 are 'symmetric', only c("WMB", "VLO") not


### 7.3.3 Matrices of p-values #################################################

## Build matrix of rejections (at 5% level)
rej <- pvalsMatPWFull # diag() is NA
rej[pvalsMatPWFull >= 0.05] <- 0 # non-rejections
rej[pvalsMatPWFull < 0.05] <- 1 # rejections of full model
rej[pvalsMatPWPW   < 0.05] <- 2 # rejections of bivariate t models

## Plot
if(doPNG)
    png(file = (file <- paste0("fig_SP500_gof_pw_matrix.png")),
        width = 7.5, height = 6, units = "in", res = 600, bg = "transparent")
matrix_plot(rej, at = c(0, 2/3, 4/3, 2), col.regions = c("white", "black", "maroon3"),
            colorkey = list(tck = 0,
                            labels = list(at = c(1/3-0.17, 1-0.29, 10/6-0.32), rot = 90,
                                          cex = 0.7, labels = c("p-value >= 0.05",
                                                                "Only full t's p-value < 0.05",
                                                                "Both models' p-values < 0.05"))),
            panel = sector_panel_function(rej, sectors = dsec, subsectors = dssec))
if(doPNG) dev.off.crop(file)

## Omit the empty rows/cols in the above matrix plots, only do the one where
## the full model is rejected and indicate those pairs where also the bivariate
## t is rejected
## rej is *almost* symmetric (but not quite, so we have to treat the general case)
vrej <- as.vector(rej)
vtrej <- as.vector(t(rej))
ii <- which(vrej != vtrej)
vrej[ii]
vtrej[ii]
## Determine the rows/cols with at least one rejection
rkeep <- rowSums(rej, na.rm = TRUE) >= 1
ckeep <- colSums(rej, na.rm = TRUE) >= 1
stopifnot(all(rkeep == ckeep)) # => indeed we keep the same rows and cols (although rej is not symmetric)
keep <- rkeep
## Extract smaller matrix
rej. <- rej[keep, keep] # build (smaller) matrix we keep
## Extract sectors
sec.. <- sec[keep] # extract sectors
usec.. <- unique(sec..) # unique sectors
nsec.. <- sapply(usec.., function(s) sum(sec.. == s)) # sector sizes
dsec.. <- head(cumsum(nsec..), n = -1) # sector division points
## Extract subsectors
ssec.. <- ssec[keep] # extract sectors
ussec.. <- unique(ssec..) # unique subsectors
nssec.. <- sapply(ussec.., function(s) sum(ssec.. == s)) # subsector sizes
dssec.. <- head(cumsum(nssec..), n = -1) # subsector division points

## Plot
if(doPNG)
    png(file = (file <- paste0("fig_SP500_gof_pw_matrix_small.png")),
        width = 7.5, height = 6, units = "in", res = 600, bg = "transparent")
matrix_plot(rej., at = c(0, 2/3, 4/3, 2), col.regions = c("white", "black", "maroon3"),
            colorkey = list(tck = 0,
                            labels = list(at = c(1/3-0.17, 1-0.29, 10/6-0.32), rot = 90,
                                          cex = 0.7, labels = c("p-value >= 0.05",
                                                                "Only full t's p-value < 0.05",
                                                                "Both models' p-values < 0.05"))),
            panel = sector_panel_function(rej., sectors = dsec.., subsectors = dssec..,
                                          colsec="black", lwdsec=0.8, lwdsubs=0.5, colsubs="grey90"))

if(doPNG) dev.off.crop(file)

## Plot of p-values
vpvalsMatPWFull <- as.vector(pvalsMatPWFull)
vpvalsMatPWPW   <- as.vector(pvalsMatPWPW)
if(doPNG)
    png(file = (file <- paste0("fig_SP500_gof_pw_pvals_full_vs_pw.png")),
        width = 6, height = 6, units = "in", res = 200, bg = "transparent")
plot(vpvalsMatPWFull, vpvalsMatPWPW, col = adjustcolor("black", alpha.f = 0.08),
     cex = 0.3, xlab = "p-values of pairwise AD tests (t copula)",
     ylab = "p-values of pairwise AD tests (bivariate t copulas)")
if(doPNG) dev.off.crop(file)

## Plot of corresponding pseudo-observations
pobs.vpvals <- pobs(cbind(vpvalsMatPWFull, vpvalsMatPWPW))
if(doPNG)
    png(file = (file <- paste0("fig_SP500_gof_pw_pvals_full_vs_pw_pobs.png")),
        width = 6, height = 6, units = "in", res = 200, bg = "transparent")
plot(pobs.vpvals, col = adjustcolor("black", alpha.f = 0.08),
     cex = 0.3, xlab = "Pseudo-observations of p-values of pairwise AD tests (t copula)",
     ylab = "Pseudo-observations of p-values of pairwise AD tests (bivariate t copulas)")
if(doPNG) dev.off.crop(file)


### 7.3.4 Pobs plots of all pairs with smallest p-value for the bivariate t copulas
###       (most pairs also have the smallest p-value for the bivariate t copulas
###       implied by the full t copula)

## Sort pairs according to increasing p-values for tests of pairwise models and
## pairwise models implied by the full model
pvalsPWPWord <- pvalsPWPW[order(pvalsPWPW[,3]),] # order p-values for test of pairwise models
pvalsPWFullord <- pvalsPWFull[order(pvalsPWFull[,3]),] # order p-values for test of pairwise models implied by the full model

## Same order?
same <- rowSums(pvalsPWPWord[,1:2] == pvalsPWFullord[,1:2]) == 2
if(FALSE)
    plot(same, xlab = "Pair", ylab = "0 = different order, 1 = same order")
which(same)
(mn <- min(which(!same)) - 1) # => the first 336 pairs are ordered the same
colnames(U)[pvalsPWPWord[mn, 1:2]] # corresponding last pair "DAL", "DFS" where they are ordered in the same way
cbind(pvalsPWPWord[mn:350,], pvalsPWFullord[336:350,])
## Note:
## For all those pairs until ("DAL", "DFS"), the order is the same and both the
## pairwise models and the pairwise models implied by the full model lead to
## rejection at (the meaningless) 0.05.

## We depict the pobs of those pairs (duplicates removed) and some more to
## fill the last row of the zenplot. For those we simply use more pairs
## of those with smallest p-value for the bivariate t copulas but those
## will be exactly those for which this (meaningless) p-value is >= 0.05.
## And, one can show, that for those additional pairs, sometimes the full
## model does not lead to rejection at the (meaningless) 0.05 level.

## Pick out the pairs to plot
ii <- max(which(pvalsPWPWord[,3] < 0.1261)) # indicator of all pairs for which the bivariate models have a p-value < such that the last row is filled
## ii <- max(which(pvalsPWFullord[,3] < 0.05)) # ... would be one more plot and would start a new row
prs <- pvalsPWPWord[1:ii, 1:2] # (ii, 2)-matrix containing these pairs
prs.lst.connect <- connect_pairs(prs, duplicate.rm = TRUE) # connect them and remove duplicates

## Extract pobs to be plotted (connected, duplicates removed)
U.lst <- vector("list", length = length(prs.lst.connect))
for(l in 1:length(prs.lst.connect))
    U.lst[[l]] <- U[, prs.lst.connect[[l]]] # matrices (>= 2 columns)

## Plot of the pobs of those pairs for which the bivariate t copula models
## lead to the smallest p-values. Note that all pairs until ("DAL", "DFS")
## are precisely those for which a) the pairwise t copulas are rejected at the
## (meaningless) 0.05 level and b) the order is the same as for the
## p-values for the bivariate t copulas implied by the full t model
if(doPNG)
    png(file = (file <- paste0("fig_SP500_pobs_zenplot_smallest_p-values.png")),
        width = 7, height = 8.65, units = "in", res = 600, bg = "transparent")
zenplot(U.lst, n2dcol = 17, ospace = 0, ispace = 0.01,
        labs = list(group = NULL, var = "V", sep = " "),
        plot1d = function(zargs) label_1d_graphics(zargs, cex = 0.4),
        plot2d = function(zargs) points_2d_graphics(zargs, cex = 0.1,
                                                    col = adjustcolor("black", alpha.f = 0.2)))
if(doPNG) dev.off.crop(file)
