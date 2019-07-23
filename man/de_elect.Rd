\name{de_elect}
\alias{de_elect}
\docType{data}
\title{German Election Data from 2002 and 2005}
\description{
  Data set consisting of 68 columns of data about the German elections
  2002 and 2005.
}
\usage{
data("de_elect")
}
\format{
  A \code{\link{data.frame}()} with 68 columns:
  \describe{
    \item{\code{District}:}{electoral district}
    \item{\code{State}:}{federal state (Bundesland)}
    \item{\code{Num.comm}:}{number of communities}
    \item{\code{Area}:}{area 2004-12-31 (in square km)}
    \item{\code{Pop}:}{population 2004-12-31 (in 1000)}
    \item{\code{Men}:}{men (in 1000)}
    \item{\code{Citizens}:}{germans (in 1000)}
    \item{\code{Density}:}{population density 2004-12-31 (in square km)}
    \item{\code{Pop.le.15}:}{population younger than (or equal to) 15 years
      2002-12-31 (in percent)}
    \item{\code{Pop.15.18}:}{population between 15 and 18 years old
      2002-12-31 (in percent)}
    \item{\code{Pop.18.25}:}{population between 18 and 25 years old
      2002-12-31 (in percent)}
    \item{\code{Pop.25.35}:}{population between 25 and 35 years old
      2002-12-31 (in percent)}
    \item{\code{Pop.35.60}:}{population between 35 and 60 years old
      2002-12-31 (in percent)}
    \item{\code{Pop.g.60}:}{population older than 60 years
      2002-12-31 (in percent)}
    \item{\code{Births}:}{live births (per 1000)}
    \item{\code{Deaths}:}{deaths (per 1000)}
    \item{\code{Move.in}:}{moving there in 2003 (per 1000)}
    \item{\code{Move.out}:}{moving away in 2003 (per 1000)}
    \item{\code{Increase}:}{increase in population (per 1000)}
    \item{\code{Farms}:}{number of farms in 2001 (per 1000)}
    \item{\code{Agriculture}:}{agriculturally used land (in ha)}
    \item{\code{Mining}:}{mining companies and processing trade 2002-09-30 (per 1000)}
    \item{\code{Mining.employees}:}{employees in mining and processing trade 2002-09-30 (per 1000)}
    \item{\code{Apt.new}:}{new apartments 2002 (per 1000)}
    \item{\code{Apt}:}{apartments 2002-12-31 (per 1000)}
    \item{\code{Motorized}:}{motor vehicles 2003-01-31 (per 1000)}
    \item{\code{School.finishers}:}{school finishers 2002 (per 1000)}
    \item{\code{School.wo.2nd}:}{without secondary school (ohne
      Hauptschule) 2002 (in percent)}
    \item{\code{School.2nd}:}{with secondary school (Hauptschule) 2002 (in percent)}
    \item{\code{School.Real}:}{with graduation from Realschule 2002 (in percent)}
    \item{\code{School.UED}:}{with university-entrance diploma
      (Gymnasium) 2002 (in percent)}
    \item{\code{Unemployment.03}:}{unemployment 2003-12-31 (in percent)}
    \item{\code{Unemployment.04}:}{unemployment 2004-12-31 (in percent)}
    \item{\code{Employed}:}{employed subject to social insurance contribution (per 1000)}
    \item{\code{FFF}:}{farmers, foresters, fishermen (in percent)}
    \item{\code{Industry}:}{industry employees subject to social insurance
      contribution (in percent)}
    \item{\code{CTT}:}{commerce, transportation and telecommunication employees
      subject to social insurance contribution (in percent)}
    \item{\code{OS}:}{other services (in percent)}
    \item{\code{Voters.05}:}{eligible voters 2005}
    \item{\code{Voters.02}:}{eligible voters 2002}
    \item{\code{Votes.05}:}{number of votes 2005}
    \item{\code{Votes.02}:}{number of votes 2002}
    \item{\code{Invalid.05}:}{invalid votes 2005}
    \item{\code{Invalid.02}:}{invalid votes 2002}
    \item{\code{Valid.05}:}{valid votes 2005}
    \item{\code{Valid.02}:}{valid votes 2002}
    \item{\code{Votes.SPD.05}:}{votes for SPD 2005}
    \item{\code{Votes.SPD.02}:}{votes for SPD 2002}
    \item{\code{Votes.CDU.CSU.05}:}{votes for CDU/CSU 2005}
    \item{\code{Votes.CDU.CSU.02}:}{votes for CDU/CSU 2002}
    \item{\code{Votes.Gruene.05}:}{votes for Gruene 2005}
    \item{\code{Votes.Gruene.02}:}{votes for Gruene 2002}
    \item{\code{Votes.FDP.05}:}{votes for FDP 2005}
    \item{\code{Votes.FDP.02}:}{votes for FDP 2002}
    \item{\code{Votes.Linke.05}:}{votes for Linke 2005}
    \item{\code{Votes.Linke.02}:}{votes for Linke 2002}
    \item{\code{SPD.05}:}{SPD 2005 (as a fraction in [0,1])}
    \item{\code{CDU.CSU.05}:}{CDU/CSU 2005 (as a fraction in [0,1])}
    \item{\code{Gruene.05}:}{Gruene 2005 (as a fraction in [0,1])}
    \item{\code{FDP.05}:}{FDP 2005 (as a fraction in [0,1])}
    \item{\code{Linke.05}:}{Linke 2005 (as a fraction in [0,1])}
    \item{\code{Others.05}:}{Other parties 2005 (as a fraction in [0,1])}
    \item{\code{SPD.02}:}{SPD 2002 (as a fraction in [0,1])}
    \item{\code{CDU.CSU.02}:}{CDU/CSU 2002 (as a fraction in [0,1])}
    \item{\code{Gruene.02}:}{Gruene 2002 (as a fraction in [0,1])}
    \item{\code{FDP.02}:}{FDP 2002 (as a fraction in [0,1])}
    \item{\code{Linke.02}:}{Linke 2002 (as a fraction in [0,1])}
    \item{\code{Others.02}:}{other parties 2002 (as a fraction in [0,1])}
  }
}
\source{
  The data was obtained from http://www.bundeswahlleiter.de but is not
  available under this link anymore. Furthermore, the first column of
  the original data set is ommitted as it only contained the row numbers.
}
\examples{
data("de_elect")
}
\keyword{datasets}