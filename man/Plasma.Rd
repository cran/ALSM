\name{Plasma}
\alias{Plasma}
\docType{data}
\title{ plasma level of a polyamine
%%   ~~ data name/kind ... ~~
}
\description{Data on age (X) and plasma level of a polyamine (Y) for a portion of the 25 healthy children in a study
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("Plasma")}
\format{
  A data frame with 25 observations on the following 3 variables.
  \describe{
    \item{\code{x}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{log.y}}{a numeric vector}
  }
}

\source{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition  .TABLE 3.8 page 133
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\examples{
data(Plasma)
## maybe str(Plasma) ; plot(Plasma) ...
}
\keyword{datasets}
