\name{BodyFat}
\alias{BodyFat}
\docType{data}
\title{Body Fat dataset
%%   ~~ data name/kind ... ~~
}
\description{study of the relation of amount of body fat (Y) to several possible predictor variables, based on a sample of 20 healthy females 25-34 years old
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("BodyFat")}
\format{
  A data frame with 20 observations on the following 4 variables.
  \describe{
    \item{\code{x1}}{a numeric vector}
    \item{\code{x2}}{a numeric vector}
    \item{\code{x3}}{a numeric vector}
    \item{\code{y}}{a numeric vector}
  }
}

\source{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition  .TABLE 7.1 page 257
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\examples{
data(BodyFat)
## maybe str(BodyFat) ; plot(BodyFat) ...
}
\keyword{datasets}
