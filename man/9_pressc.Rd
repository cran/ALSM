\name{pressc}
\alias{pressc}

\title{calculate PRESS }
\description{prediction sum of squares

}
\usage{
pressc(fit)
}

\arguments{
  \item{fit}{
model of regression
}
}

\references{
Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition .page 360
}
\examples{

######## example page 361
pressc(lm(lny~x2+x3+x4,SurgicalUnit))
}






