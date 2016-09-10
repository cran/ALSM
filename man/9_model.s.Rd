\name{model.s}
\alias{model.s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Criteria for Model Selection
}
\description{
SSEp, R2.p, R2.adj.p, Cp, AICp, SBCp and PRESSP Values for All Possible Regression Modes
}
\usage{
model.s(x,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{matrix of predictors
%%     ~~Describe \code{x} here~~
}
\item{y}{response vector

}
}

\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition  .chapter 9
%% ~put references to the literature/web site here ~
}

\examples{

### table 9.2, page 353
library("leaps")
model.s(SurgicalUnit[,1:4],SurgicalUnit[,10])

}
