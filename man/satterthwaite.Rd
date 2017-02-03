\name{satterthwaite}
\alias{satterthwaite}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{satterthwaite
%%  ~~function to do ... ~~
}
\description{satterthwaite
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
satterthwaite(c, MSE, df, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{c}{c
%%     ~~Describe \code{c} here~~
}
  \item{MSE}{mse
%%     ~~Describe \code{MSE} here~~
}
  \item{df}{df
%%     ~~Describe \code{df} here~~
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
function (c, MSE, df, alpha = 0.05)
{
    l = c * MSE
    L = sum(c * MSE)
    dff <- ((L^2)/sum((l^2)/df))
    lower <- (dff * L)/(qchisq(1 - alpha/2, round(dff)))
    upper <- (dff * L)/(qchisq(alpha/2, round(dff)))
    return(cbind(estimate = L, df = round(dff), lower = lower,
        upper = upper))
  }
}
