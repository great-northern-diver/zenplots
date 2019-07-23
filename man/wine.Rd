\name{wine}
\alias{wine}
\docType{data}
\title{Wine Data Set}
\description{
  Data set consisting of 178 rows and 27 columns containing data about
  wine from the Piedmont region of Italy.
}
\usage{
data("wine")
}
\format{
  \code{\link{data.frame}()} with 27 columns:
  \describe{
    \item{\code{wine}:}{wine name (categorical variable
      with levels \code{Barbera}, \code{Barolo}, \code{Grignolino}).}
    \item{\code{alcohol}:}{alcohol percentage (numeric).}
    \item{\code{sugar}:}{sugar-free extract (numeric).}
    \item{\code{acidity}:}{fixed acidity (numeric).}
    \item{\code{tartaric}:}{tartaric acid (numeric).}
    \item{\code{malic}:}{malic acid (numeric).}
    \item{\code{uronic}:}{uronic acids (numeric).}
    \item{\code{pH}:}{pH (numeric).}
    \item{\code{ash}:}{ash (numeric).}
    \item{\code{alcal_ash}:}{alcalinity of ash (numeric).}
    \item{\code{potassium}:}{potassium (numeric).}
    \item{\code{calcium}:}{calcium (numeric).}
    \item{\code{magnesium}:}{magnesium (numeric).}
    \item{\code{phosphate}:}{phosphate (numeric).}
    \item{\code{cloride}:}{chloride (numeric).}
    \item{\code{phenols}:}{total phenols (numeric).}
    \item{\code{flavanoids}:}{flavanoids (numeric).}
    \item{\code{nonflavanoids}:}{nonflavanoid phenols (numeric).}
    \item{\code{proanthocyanins}:}{proanthocyanins (numeric).}
    \item{\code{colour}:}{colour intensity (numeric).}
    \item{\code{hue}:}{hue (numeric).}
    \item{\code{OD_dw}:}{\eqn{OD_{280}/OD_{315}}{OD_280/OD_315} of
      diluted wines (numeric).}
    \item{\code{OD_fl}:}{\eqn{OD_{280}/OD_{315}}{OD_280/OD_315} of
      flavanoids (numeric).}
    \item{\code{glycerol}:}{glycerol (numeric).}
    \item{\code{butanediol}:}{2,3-butanediol (numeric).}
    \item{\code{nitrogen}:}{total nitrogen (numeric).}
    \item{\code{proline}:}{proline (numeric).}
    \item{\code{methanol}:}{methanol (numeric).}
  }
}
\source{
  The data set was obtained from the \R\ package \pkg{sn} (for
  convenience). It represent chemical measurements on each of 178 wine specimens
  belonging to three types of wine produced in the Piedmont region of Italy.
  The data set includes all variables listed by Forina \emph{et al.}
  (1986) with the exception of \sQuote{Sulphate}. The first variable is
  categorial, all others are numeric.

  Forina, M., Lanteri, S. Armanino, C., Casolino, C., Casale, M. and Oliveri, P.
  \acronym{V-PARVUS 2008}: an extendible package of programs for esplorative
  data analysis, classification and regression analysis.
  Dip. Chimica e Tecnologie Farmaceutiche ed Alimentari,
  Università di Genova, Italia. Web-site (not accessible as of 2014):
  \samp{http://www.parvus.unige.it}
}
\references{
  Forina M., Armanino C., Castino M. and Ubigli M. (1986).
  Multivariate data analysis as a discriminating method of the origin of wines.
  \emph{Vitis} \bold{25}, 189--201.
}
\examples{
data("wine")
}
\keyword{datasets}