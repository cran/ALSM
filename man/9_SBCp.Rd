\name{SBCp}
\alias{SBCp}
\title{
Calculate SBC
}
\description{Schwarz' Bayesian criterion

\deqn{SBC = n*ln(SSEp)-n*ln(n)+ln(n)*p)}

}
\usage{
SBCp(model)
}

\arguments{
  \item{model}{
model of regression
}
}

\references{
Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition .page 360
}

\examples{

##################use data Surgical Unit, page 360

SBCp(lm(lny~x4,SurgicalUnit))
}


