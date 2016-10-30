\name{boxcox.sse}
\alias{boxcox.sse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Box-Cox Transformations (SSE)
%%  ~~function to do ... ~~
}
\description{transformation from the family of power transformations on Y. box-cox transformation according to SSE.

also plot SSE against lambda
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
boxcox.sse(x,y,l=seq(-2,2,.1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{vector predictor variable
%%     ~~Describe \code{x} here~~
}
\item{y}{vector response variable
%%     ~~Describe \code{x} here~~
}
\item{l}{vector, Different values of lambda
%%     ~~Describe \code{x} here~~
}
}

\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition.page 134
%% ~put references to the literature/web site here ~
}

\examples{
### table 3.9 & figure 3.17 (page 136)
boxcox.sse(Plasma[,1],Plasma[,2])

### table 18.6 page 792
boxcox.sse(factor(ServoData[,2]),ServoData[,1])
}

