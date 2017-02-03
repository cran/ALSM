\name{normal.cor.test}
\alias{normal.cor.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{normal correation test
%%  ~~function to do ... ~~
}
\description{normal correation test
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
normal.cor.test(residuals, MSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{residuals}{residual
%%     ~~Describe \code{residuals} here~~
}
  \item{MSE}{MSE
%%     ~~Describe \code{MSE} here~~
}
}

\references{neter
%% ~put references to the literature/web site here ~
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (residuals, MSE)
{
    w <- 1:length(residuals)
    r <- cor(sort(residuals), sqrt(MSE) * (qnorm((w - 0.375)/(length(residuals) +
        0.25))))
    return(r)
  }
}

