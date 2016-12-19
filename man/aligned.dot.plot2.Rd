\name{aligned.dot.plot2}
\alias{aligned.dot.plot2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Aligned dot plot specific variable correspond to two factor
%%  ~~function to do ... ~~
}
\description{Aligned dot plot specific variable correspond to two factor
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aligned.dot.plot2(y, factor1, factor2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{variable based on plot aligned dot plot
%%     ~~Describe \code{y} here~~
}
  \item{factor1}{first factor variable
%%     ~~Describe \code{factor1} here~~
}
  \item{factor2}{second factor variable
%%     ~~Describe \code{factor2} here~~
}
}

\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition.
%% ~put references to the literature/web site here ~
}

\examples{
library('ALSM')
y=CashOffers$y
A=CashOffers$A
B=CashOffers$B
aligned.dot.plot2(y,A,B)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
