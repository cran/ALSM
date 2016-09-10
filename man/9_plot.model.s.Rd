\name{plotmodel.s}
\alias{plotmodel.s}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot Criterias for Model Selection
}
\description{plot  R2.p, R2.adj.p, Cp, AICp, SBCp and PRESSP Values for All Possible Regression Modes
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotmodel.s(x,y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{matrix of predictors
%%     ~~Describe \code{x} here~~
}
\item{y}{response vector

}
}

\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition .chapter 9
%% ~put references to the literature/web site here ~
}

\examples{

### figure 9.4, page 356
plotmodel.s(SurgicalUnit[,1:4],SurgicalUnit[,10])
}

