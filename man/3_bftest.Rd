\name{bftest}
\alias{bftest}
\title{
Brown-Forsythe Test between two group

}
\description{Tests for Constancy of Error Variance between two group
}
\usage{
bftest(fit,group,alpha=.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
model of regression
}
\item{group}{
vector, determine two group
}

\item{alpha}{
Type I error level
}
}
\details{length group and number of observations should be equal
%%  ~~ If necessary, more details than the description above ~~
}
\value{test statistic, p.value, alpha and degree of freedom
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition .page 116
}



\examples{
################### EXAMPLE PAGE 117


### Group
g<-rep(1,25)
g[TolucaCompany$x<=70]=0
### TEST
bftest(lm(y~x,TolucaCompany),g)

}

