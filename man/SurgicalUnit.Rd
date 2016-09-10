\name{SurgicalUnit}
\alias{SurgicalUnit}
\docType{data}
\title{ Surgical Unit dataset
%%   ~~ data name/kind ... ~~
}
\description{A hospital surgical unit was interested in predicting survival in patients undergoing a particular of type of  liver operation.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("SurgicalUnit")}
\format{
  A data frame with 54 observations on the following 10 variables.
  \describe{
    \item{\code{x1}}{a numeric vector}
    \item{\code{x2}}{a numeric vector}
    \item{\code{x3}}{a numeric vector}
    \item{\code{x4}}{a numeric vector}
    \item{\code{x5}}{a numeric vector}
    \item{\code{x6}}{a numeric vector}
    \item{\code{x7}}{a numeric vector}
    \item{\code{x8}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
    \item{\code{lny}}{a numeric vector}
  }
}

\source{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition  .page 350
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\examples{
data(SurgicalUnit)
## maybe str(SurgicalUnit) ; plot(SurgicalUnit) ...
}
\keyword{datasets}
