\name{modelval}
\alias{modelval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Model Validation
%%  ~~function to do ... ~~
}
\description{
Compare (estimation, SE estimation, PRESSp, AICp, SBSp, R2 , R2.adj, MSE) Training and validition data
}
\usage{
modelval(building.set,response.building, prediction.set, response.prediction)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{building.set}{ matrix predictor Training data
%%     ~~Describe \code{x} here~~
}
\item{response.building}{vector response Training data
%%     ~~Describe \code{x} here~~
}
\item{prediction.set}{matrix predictor Validation  data
%%     ~~Describe \code{x} here~~
}
\item{response.prediction}{vector response Validation data
%%     ~~Describe \code{x} here~~
}
}

\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition .chapter 9
%% ~put references to the literature/web site here ~
}

\examples{

#### page 373 (table)
b<-SurgicalUnit
p<-SurgicalUnitAdditional

modelval(b[,c(1,2,3,8)],b[,10],p[,c(1,2,3,8)],p[,10])
modelval(b[,c(1,2,3,5,6,8)],b[,10],p[,c(1,2,3,5,6,8)],p[,10])
}

