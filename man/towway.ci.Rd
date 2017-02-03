\name{towway.ci}
\alias{towway.ci}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Confidence intervals for tow way ANOVA
%%  ~~function to do ... ~~
}
\description{
Confidence intervals for tow way ANOVA.

Estimation and confidense interval of Factor Level Mean.

Estimation and confidense interval of Contrast of Factor Level Means

Estimation and confidense interval of linear Combination of Factor level Means

Multiple Pairwise Comparisons and confidense interval of Factor level Means (Tukey and Bonferroni Procedurs)

Multiple Contrasts and confidense interval of Factor Level Means (scheffe and Bonferroni Procedurs)

Etimates and confidense interval Based on Treatment Means

Interactions Are Important:

Multiple Pairwise Comparisons of Treatment Means(Tukey and Bonferroni Procedure.)

Multiple Contrasts of Treatment Means(Scheffe and Bonferroni Procedure.)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
towway.ci(y, x1, x2, mc = NULL, mp = NULL,mt=NULL, mse= NULL,alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{response variabel
%%     ~~Describe \code{y} here~~
}
  \item{x1}{first factor
%%     ~~Describe \code{x1} here~~
}

  \item{x2}{second factor
%%     ~~Describe \code{x2} here~~
}
  \item{mc}{matrix of contrast(s)
%%     ~~Describe \code{mc} here~~
}
  \item{mp}{matrix of pairwise. each row is one pairwise
%%     ~~Describe \code{mp} here~~
}
\item{mt}{matrix of treatment means. (interaction NOT important)
%%     ~~Describe \code{x1} here~~
}
  \item{mse}{MSE of model
%%     ~~Describe \code{mse} here~~
}
  \item{alpha}{Confidence intervals calculate with 1-alpha  percent
%%     ~~Describe \code{alpha} here~~
}
}

\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition. chapter 19.
%% ~put references to the literature/web site here ~
}



\examples{
library('ALSM')
y=HayFeverRelief$y
A=HayFeverRelief$A
B=HayFeverRelief$B

m=matrix(c(1,2,3,0,0,0,.5,-1,.5),byrow = TRUE,nrow = 3)
towway.ci(y,A,B,mc=m)
}
