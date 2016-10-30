\name{AICp}
\alias{AICp}
\title{
Calculate AIC
}
\description{Akaike's information criterion

\deqn{AIC = n*ln(SSEp)-n*ln(n)+2*p}

}
\usage{
AICp(model)
}

\arguments{
  \item{model}{
model of regression
}
}

\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition. page 360
}

\seealso{
\code{\link{cpc}},\code{\link{SBCp}},\code{\link{pressc}}
}
\examples{
#### example page 360
AICp(lm(lny~x4,SurgicalUnit))

}
