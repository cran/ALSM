\name{JobProficiency}
\alias{JobProficiency}
\docType{data}
\title{Job Proficiency dataset
%%   ~~ data name/kind ... ~~
}
\description{A personnel officer in a governmental agency administered four newly developed aptitude tests to each of 25 applicants for entry-level clerical positions in the agency
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("JobProficiency")}
\format{
  A data frame with 25 observations on the following 5 variables.
  \describe{
    \item{\code{y}}{a numeric vector}
    \item{\code{x1}}{a numeric vector}
    \item{\code{x2}}{a numeric vector}
    \item{\code{x3}}{a numeric vector}
    \item{\code{x4}}{a numeric vector}
  }
}

\source{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition  .problem 9.10 page 377
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\examples{
data(JobProficiency)
## maybe str(JobProficiency) ; plot(JobProficiency) ...
}
\keyword{datasets}
