\name{MLS}
\alias{MLS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{MLS
%%  ~~function to do ... ~~
}
\description{MLS
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MLS(MSE1, df1, c1, MSE2, df2, c2, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{mse1
  \item{MSE1}{
%%     ~~Describe \code{MSE1} here~~
}
  \item{df1}{df1
%%     ~~Describe \code{df1} here~~
}
  \item{c1}{c1
%%     ~~Describe \code{c1} here~~
}
  \item{MSE2}{mse2
%%     ~~Describe \code{MSE2} here~~
}
  \item{df2}{df2
%%     ~~Describe \code{df2} here~~
}
  \item{c2}{c2
%%     ~~Describe \code{c2} here~~
}
  \item{alpha}{a
%%     ~~Describe \code{alpha} here~~
}
}

\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition. chapter 25.
%% ~put references to the literature/web site here ~
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (MSE1, df1, c1, MSE2, df2, c2, alpha = 0.05)
{
    f1 = qf(1 - alpha/2, df1, Inf)
    f2 = qf(1 - alpha/2, df2, Inf)
    f3 = qf(1 - alpha/2, Inf, df1)
    f4 = qf(1 - alpha/2, Inf, df2)
    f5 = qf(1 - alpha/2, df1, df2)
    f6 = qf(1 - alpha/2, df2, df1)
    g1 <- 1 - 1/f1
    g2 <- 1 - 1/f2
    g3 <- (((f5 - 1)^2) - ((g1 * f5)^2) - ((f4 - 1)^2))/f5
    g4 <- f6 * ((((f6 - 1)/f6)^2) - 1 * (((f3 - 1)/f6)^2) - g2^2)
    hl <- sqrt(((g1 * c1 * MSE1)^2) + (((f4 - 1) * c2 * MSE2)^2) -
        1 * ((g3 * c1 * c2 * MSE1 * MSE2)))
    hu <- sqrt((((f3 - 1) * c1 * MSE1)^2) + ((g2 * c2 * MSE2)^2) -
        1 * ((g4 * c1 * c2 * MSE1 * MSE2)))
    l = c1 * MSE1 + c2 * MSE2
    L = sum(l)
    lower <- L - hl
    upper <- L + hu
    return(cbind(estimate = L, lower = lower, upper = upper))
  }
}
