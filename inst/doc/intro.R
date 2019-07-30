## ---- message=FALSE, warning=FALSE---------------------------------------
library(zenplots)

## ---- message=FALSE, warning=FALSE---------------------------------------
library(PairViz)

## ------------------------------------------------------------------------
head(attenu)

## ---- echo=FALSE---------------------------------------------------------
names(attenu)

## ---- message=FALSE, warning=FALSE---------------------------------------
## Since attenu has 5 variates, the complete graph has n=5 nodes
## and an Euler sequence is given as
eseq(5)

## ---- message=FALSE, warning=FALSE---------------------------------------
names(attenu)[eseq(5)]

## ---- echo=FALSE, message=FALSE, warning=FALSE, fig.align="center"-------
library(Rgraphviz)
parOptions <- par(no.readonly = TRUE)
plot(mk_complete_graph(names(attenu)), "circo")
par(parOptions)

## ---- message=FALSE, warning=FALSE, fig.align="center"-------------------
zenpath(5)

## ---- message=FALSE, warning=FALSE, fig.align="center"-------------------
## Back loading ensures all pairs appear latest (back) for
## high values of the indices.
zenpath(5, method = "back.loaded")
## Frot loading ensures all pairs appear earliest (front) for
## low values of the indices.
zenpath(5, method = "front.loaded")
## Balanced loading ensures all pairs appear in groups of all
## indices (Hamiltonian paths -> a Hamiltonian decomposition of the Eulerian)
zenpath(5, method = "balanced")

## ----zenpath, echo= FALSE, include=FALSE, fig.align="center"-------------
parOptions <- par(mfrow=c(1,3), mar=c(4, 2,2,0),oma=rep(0,4))
back <- zenpath(15, method = "back.loaded")
balanced <- zenpath(15, method = "balanced")
front <- zenpath(15, method = "front.loaded")
n <- length(zenpath(15))
plot_chars <- letters[1:15]
plot(back, n - 1:n,type="b", ylab="", xlab="", main="Back loaded",
     pch=plot_chars[back], bty="n", xaxt="n", yaxt="n")
plot(balanced, n - 1:n,type="b", ylab="", xlab="", main="Balanced",
     pch=plot_chars[balanced], bty="n", xaxt="n", yaxt="n")
plot(front, n - 1:n,type="b", ylab="", xlab="", main="Front loaded",
     pch=plot_chars[front], bty="n", xaxt="n", yaxt="n")
par(parOptions)

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=7, fig.height=7----
## We remove the space between plots and suppress the axes
## so as to give maximal space to the individual scatterplots.
## We also choose a different plotting character and reduce
## its size to better distinguish points.
pairs(attenu, oma=rep(0,4), gap=0, xaxt="n", yaxt="n")

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=7, fig.height=7----
## Plotting character and size are chosen to match that
## of the pairs plot.
## zenpath ensures that all pairs of variates appear
## in the zenplot.
## The last argument, n2dcol, is chosen so that the zenplot
## has the same number of plots across the page as does the
## pairs plot.
zenplot(attenu[, zenpath(ncol(attenu))], n2dcol=4)

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=7, fig.height=7----
## Call zenplot exactly as before, except that each scatterplot is replaced
## by an arrow that shows the direction of the layout.
zenplot(attenu[, zenpath(ncol(attenu))], plot2d="arrow", n2dcol=4)

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=10, fig.height=6----
## The default n2dcol is used
zenplot(attenu[, zenpath(ncol(attenu))], n2dcol=5)

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=6, fig.height=10----
## The default n2dcol is used
zenplot(attenu[, zenpath(ncol(attenu))])

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=6, fig.height=10----
## The directions
zenplot(attenu[, zenpath(ncol(attenu))], plot2d="arrow")

## ------------------------------------------------------------------------
## Access the German election data from zenplots package
data(de_elect)

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=7, fig.height=7----
## pairs(de_elect[,1:34], oma=rep(0,4), gap=0, pch=".", xaxt="n", yaxt="n")

## ----message=FALSE, warning=FALSE, fig.align="center", fig.width=7, fig.height=7----
## Try invoking the plot with the following
## zenplot(de_elect[,zenpath(68)], pch=".", n2dcol="square",col=adjustcolor("black",0.5))

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=7, fig.height=7----
## pairs(de_elect, oma=rep(0,4), gap=0, pch=".", xaxt="n", yaxt="n",col=adjustcolor("black",0.5))

## ---- message=FALSE, warning=FALSE---------------------------------------
Education <- c("School.finishers",
               "School.wo.2nd", "School.2nd",
               "School.Real",  "School.UED")
Employment <- c("Employed", "FFF", "Industry",
                "CTT", "OS" )

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=5, fig.height=6----

EducationData <- de_elect[, Education]
EmploymentData <- de_elect[, Employment]

## Plot all pairs within each group
zenplot(list(Educ= EducationData[, zenpath(ncol(EducationData))],
             Empl= EmploymentData[, zenpath(ncol(EmploymentData))]))

## ------------------------------------------------------------------------
## Grouping variates in the German election data
Regions <- c("District", "State", "Density")
PopDist <- c("Men", "Citizens", "Pop.18.25", "Pop.25.35",
             "Pop.35.60", "Pop.g.60")
PopChange <- c("Births", "Deaths", "Move.in", "Move.out", "Increase")
Agriculture <- c("Farms", "Agriculture")
Mining <- c("Mining", "Mining.employees")
Apt <- c("Apt.new", "Apt")
Motorized <- c("Motorized")
Education <- c("School.finishers",
               "School.wo.2nd", "School.2nd",
               "School.Real",  "School.UED")
Unemployment <- c("Unemployment.03", "Unemployment.04")
Employment <- c("Employed", "FFF", "Industry", "CTT", "OS" )
Voting.05 <- c("Voters.05", "Votes.05", "Invalid.05", "Valid.05")
Voting.02 <- c("Voters.02", "Votes.02", "Invalid.02", "Valid.02")
Voting <- c(Voting.02, Voting.05)
VotesByParty.02 <- c("Votes.SPD.02", "Votes.CDU.CSU.02", "Votes.Gruene.02",
                     "Votes.FDP.02", "Votes.Linke.02")
VotesByParty.05 <- c("Votes.SPD.05", "Votes.CDU.CSU.05", "Votes.Gruene.05",
                     "Votes.FDP.05", "Votes.Linke.05")
VotesByParty <- c(VotesByParty.02, VotesByParty.05)
PercentByParty.02 <- c("SPD.02", "CDU.CSU.02", "Gruene.02",
                       "FDP.02", "Linke.02", "Others.02")
PercentByParty.05 <- c("SPD.05", "CDU.CSU.05", "Gruene.05",
                       "FDP.05", "Linke.05", "Others.05")
PercentByParty <- c(PercentByParty.02, PercentByParty.05)

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=11, fig.height=14----
groups <- list(Regions=Regions, Pop=PopDist,
               Change = PopChange, Agric=Agriculture,
               Mining=Mining, Apt=Apt,  Cars=Motorized,
               Educ=Education, Unemployed=Unemployment, Employed=Employment#,
               # Vote02=Voting.02, Vote05=Voting.05,
               # Party02=VotesByParty.02, Party05=VotesByParty.05,
               # Perc02=PercentByParty.02, Perc05=PercentByParty.05
               )

group_paths <- lapply(groups, FUN= function(g) g[zenpath(length(g), method = "front.loaded")] )
x <- groupData(de_elect, indices=group_paths)

zenplot(x, pch = ".", cex=0.7, col = "grey10")

## ----message=FALSE, warning=FALSE, fig.align="center", fig.width=10, fig.height=10----
#
## Grouping variates in the German election data
RegionsShort <- c("ED", "State", "density")
PopDistShort <- c("men", "citizen", "18-25", "25-35", "35-60", "> 60")
PopChangeShort <- c("births", "deaths", "in", "out", "up")
AgricultureShort <- c("farms", "hectares")
MiningShort <- c("firms", "employees")
AptShort <- c("new", "all")
TransportationShort <- c("cars")
EducationShort <- c("finishers", "no.2nd", "2nd", "Real", "UED")
UnemploymentShort<- c("03", "04")
EmploymentShort <- c("employed", "FFF", "Industry", "CTT", "OS" )
Voting.05Short <- c("eligible", "votes", "invalid", "valid")
Voting.02Short <- c("eligible", "votes", "invalid", "valid")
VotesByParty.02Short <- c("SPD", "CDU.CSU", "Gruene", "FDP", "Linke")
VotesByParty.05Short <- c("SPD", "CDU.CSU", "Gruene", "FDP", "Linke")
PercentByParty.02Short <- c("SPD", "CDU.CSU", "Gruene", "FDP", "Linke", "rest")
PercentByParty.05Short <-  c("SPD", "CDU.CSU", "Gruene", "FDP", "Linke", "rest")

shortNames <- list(RegionsShort, PopDistShort, PopChangeShort, AgricultureShort,
                   MiningShort, AptShort, TransportationShort, EducationShort,
                   UnemploymentShort, EmploymentShort, Voting.05Short, Voting.02Short,
                   VotesByParty.02Short, VotesByParty.05Short, PercentByParty.02Short,
                   PercentByParty.05Short)
# Now replace the names in x by these.

nGroups <- length(x)

for (i in 1:nGroups) {
  longNames <- colnames(x[[i]])
  newNames <- shortNames[[i]]
  oldNames <- groups[[i]]
  #print(longNames)
  #print(newNames)
  for (j in 1:length(longNames)) {
    for (k in 1:length(newNames)) {
      if (grepl(oldNames[k], longNames[j])) {
        longNames[longNames == longNames[j]] <- newNames[k]
      }
    }
  }
  colnames(x[[i]]) <- longNames
}

zenplot(x, pch = ".", cex=0.75)


## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=10, fig.height=16----
crossedGroups <- c(Employment, Education)
crossedPaths <- zenpath(c(length(Employment), length(Education)), method="eulerian.cross")
zenplot(de_elect[,crossedGroups][crossedPaths])

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=5, fig.height=2----
earthquakes <- attenu[, c(1,2,4,5)]  # ignore the station id
zenplot(earthquakes,
        plot1d="boxplot", plot2d=NULL,
        width1d=5, width2d=1,
        turns=c("r","r","r","r","r","r","r"))

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=4, fig.height=4----
zenplot(earthquakes,
        plot1d="boxplot", plot2d=NULL,
        width1d=1, width2d=1,  # now widths must be the same
        turns=c("r","d","d","l","l","u","u"))

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=4, fig.height=4----
zenplot(earthquakes,
        plot1d= "arrow", plot2d="arrow",
        width1d=1, width2d=2,
        turns=c("r","d","d","l","l","u","u"))

## ---- message=FALSE, warning=FALSE, fig.align="center", fig.width=4, fig.height=4----
zenplot(earthquakes,
        plot1d = function(zargs, ...) {
          rect_1d_graphics(zargs,  ...)
          arrow_1d_graphics(zargs, col="firebrick", lwd=3, add=TRUE, ...)
        },
        plot2d = function(zargs, ...) {
          rect_2d_graphics(zargs,  ...)
          arrow_2d_graphics(zargs, col="steelblue", lwd=3, lty=2, add=TRUE, ...)
        },
        width1d = 1, width2d = 2,
        turns=c("r","d","d","l","l","u","u"))

## ---- message=FALSE, warning=FALSE, results="hide", fig.align="center", fig.width=6, fig.height=3----
library(qqtest)
zenplot(earthquakes[,zenpath(ncol(earthquakes))],
        width1d = 1, width2d = 2, n2dcol=5,
        plot1d = function(zargs, ...) {
          r <- extract_1d(zargs) # extract arguments for 1d
          col <- adjustcolor(if (r$horizontal) "firebrick" else "steelblue",
                             alpha.f = 0.7)
          hist_1d_graphics(zargs, col=col, ...)
          },
        plot2d=function(zargs, ...) {
          r <- extract_2d(zargs) # extract arguments for 2d
          x <- as.matrix(r$x)
          xlim <- r$xlim
          y <- as.matrix(r$y)
          ylim <- r$ylim
          qqtest(y, dataTest=x,
                 xlim=xlim, ylim=ylim,
                 cex=0.3, col="black",  pch=19,
                 legend=FALSE, main="", axes=FALSE, ...)

          })

