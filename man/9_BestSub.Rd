\name{BestSub}
\alias{BestSub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Automatic Search Procedures for Model Selection; Best Subsets Algorithms
%%  ~~function to do ... ~~
}
\description{best subsets according to a
specified criterion are identified without requiring the fitting of all of the possible subset
regression models.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BestSub(x,y,method=c('r2','r2adj','sse','cp','press','aic','sbc'),num=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{matrix of predictors
%%     ~~Describe \code{x} here~~
}
\item{y}{response vector

}
\item{method}{best subsets according to this
specified criterion. R2,R2adj,sse,cp,press,aic and sbc.
%%     ~~Describe \code{x} here~~
}
\item{num}{number of best subset model per number of predictor variable.
%%     ~~Describe \code{x} here~~
}
}

\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition.chapter 9
%% ~put references to the literature/web site here ~
}

\examples{
## page 363
library("leaps")
BestSub(SurgicalUnit[,1:8],SurgicalUnit[,10],method='r2',num=2)

}

