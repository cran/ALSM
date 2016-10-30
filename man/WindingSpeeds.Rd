\name{WindingSpeeds}
\alias{WindingSpeeds}
\docType{data}
\title{Winding Speeds
%%   ~~ data name/kind ... ~~
}
\description{
In a completely randomized design to study the effect of the speed of winding
thread (1: slow; 2: normal; 3: fast; 4: maximum) onto 75-yard spools, 16 runs of 10,000 spools
each were made at each of the four winding speeds. The response variable is the number of
thread breaks during, the production run.

}
\usage{data("WindingSpeeds")}
\format{
  A data frame with 64 observations on the following 3 variables.
  \describe{
    \item{\code{y}}{response variable}
    \item{\code{group}}{factor variable}
    \item{\code{row}}{index corresponding to each response}
  }
}

\examples{
data(WindingSpeeds)
## maybe str(WindingSpeeds) ; plot(WindingSpeeds) ...
}
\keyword{datasets}
