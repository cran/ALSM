\name{cpc}
\alias{cpc}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate cp critera}
\description{Mallows' Cp Criterion
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cpc(r,f)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{Reduced model
%%     ~~Describe \code{x} here~~
}
  \item{f}{Full model
%%     ~~Describe \code{x} here~~
}

}

\references{
Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition .page 358
}


\seealso{
\code{\link{AICp}},\code{\link{SBCp}},\code{\link{pressc}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#### example page 358

cpc(lm(lny~x4,SurgicalUnit),lm(lny~x1+x2+x3+x4,SurgicalUnit))


}

