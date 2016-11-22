\name{ssea.oneway}
\alias{ssea.oneway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Planning of Sample Sizes with Estimation Approach Single factor ANOVA
%%  ~~function to do ... ~~
}
\description{This approach is to specify the major comparisons of interest. Determine the expected widths of the confidence intervals for various sample sizes, given standard deviation (sigma).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ssea.oneway(number.group,mc,sigma,n.weight,ci.width,type=c("s", "b"), alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{number.group}{number of treatments.
%%     ~~Describe \code{number.group} here~~
}
  \item{mc}{matrix of contrast. each row determine one contrast.
%%     ~~Describe \code{mc} here~~
}
  \item{sigma}{Standard deviation.
%%     ~~Describe \code{sigma} here~~
}
  \item{n.weight}{weight of sample size Corresponding to each treatment.
%%     ~~Describe \code{n.weight} here~~
}
  \item{ci.width}{width of confidence interval Corresponding to each contrast.
%%     ~~Describe \code{ci.width} here~~
}
  \item{type}{type of calculate confidence interval. }
  \item{alpha}{Confidence interval calculate with 1-alpha  percent
  }
}

\value{sample size obtain based on confidence  interval.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition. page 759.
%% ~put references to the literature/web site here ~
}


\examples{
mc=matrix(c(1,-1,0,0,.5,.5,-.5,-.5,1/3,1/3,1/3,-1),byrow=TRUE,nrow = 3)
ssea.oneway(4,mc,sigma=2,n.weight=c(1,1,1,1),ci.width=c(2.61,1.85,2.14)*2,type='s')
#### page 761
mc=matrix( c(1,0,0,-1,0,1,0,-1,0,0,1,-1),byrow=TRUE,nrow = 3)
ssea.oneway(4,mc,2,n.weight=c(1,1,1,2),ci.width=c(1,1,1)*2,type='b',alpha=.1)

#### problem 17.25 page 772
mc=matrix(c(1,-1,0,0,0,0,
            .5,.5,-.5,-.5,0,0,
            0,0,1,-1,0,0,
            .25,.25,.25,.25,-.5,-.5),byrow=TRUE,nrow=4)
ssea.oneway(6,mc,.15,n.weight=rep(1,6),ci.width=rep(2*0.08,4),type='b',alpha=.05)

}

