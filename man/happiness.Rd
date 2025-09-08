\name{happiness}
\alias{happiness}
\docType{data}
\title{World Happiness Data Set}
\description{
  Data set consisting of 498 rows and 12 columns containing data from
  the World Happiness Report over three years.
}
\usage{
data("happiness")
}
\format{
  \code{\link{data.frame}()} with 12 columns:
  \describe{
    \item{\code{Time}:}{year of the World Happiness Report.}
    \item{\code{Region}:}{region of the world.}
    \item{\code{Country}:}{country.}
    \item{\code{Happiness}:}{happiness score measured in the respective
      year (see \code{Time}) by asking ``How would you rate your
      happiness on a scale of 0 to 10 where 10 is happiest?''.}
    \item{\code{Rank}:}{rank of the country based on \code{Happiness}.}
    \item{\code{GDP}:}{extent to which the gross domestic product per
      capita contributed to the calculation of \code{Happiness}.}
    \item{\code{Family}:}{extent to which family contributed to the
      calculation of \code{Happiness}.}
    \item{\code{Health}:}{extent to which life expectancy contributed to the
      calculation of \code{Happiness}.}
    \item{\code{Freedom}:}{extent to which freedom contributed to the
      calculation of \code{Happiness}.}
    \item{\code{Corruption}:}{extent to which the perception of corruption
      contributed to the calculation of \code{Happiness}.}
    \item{\code{Generosity}:}{extent to which generosity
      contributed to the calculation of \code{Happiness}.}
    \item{\code{Dystopia}:}{extent to which the dystopia residual
      contributed to the calculation of \code{Happiness}. Dystopia
      is an imaginary country with the world's
      least-happy people (which can act as a benchmark against which all
      countries can be favorably compared).}
  }
}

\details{
  GDP, Family, Health, Freedom, Corruption and Generosity describe the
  extent to which these factors contribute in evaluating the happiness
  in each country. If added together with Dystopia, one receives the
  happiness score.
  
  The data set is based on the World Happiness Report
(\url{https://www.worldhappiness.report/}) and 
derived from the public tables provided with the report and provided by Kaggle on 2018-04-20 (historical
mirrors such as Kaggle may require an account and change over time, so
we refer to the official site above).  Downloaded from Kaggle as three separate .csv files (one for each year), 
joint columns (variables) were then built, rows expanded (to be the
  same for each year) and sorted according to Region and
  Country, and Time was added to obtain a single data set.
}

\references{
Helliwell, Layard, Sachs, et al. World Happiness Report (annual).
Available at \url{https://www.worldhappiness.report/}.
}

\examples{
data("happiness")
stopifnot(all.equal(rowSums(happiness[,c("GDP", "Family", "Health", "Freedom",
                                         "Corruption", "Generosity",
                                         "Dystopia")]),
                    happiness[, "Happiness"], tol = 5e-5))
}
\keyword{datasets}
