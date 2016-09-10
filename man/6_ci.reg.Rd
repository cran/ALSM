\name{ci.reg}
\alias{ci.reg}
\title{Estimation and confidence interval of Mean Response and Prediction of New Observation in regression model}
\usage{
ci.reg(model, newdata, type = c("b", "s","w","n","m","nm","gn"), alpha = 0.05,m=1)
}
\arguments{
\item{model}{model of regression}
\item{newdata}{Data frame, New data on which the point estimate and confidencr interval is calculated.}
\item{type}{ Type of confidence interval }
\item{alpha}{Confidence interval calculate with 1-\eqn{\alpha} percent}
\item{m}{When use "type=nm", m is equal, new observations are to be selected at the same levels x_h}
}


\value{ calculate estimation and cofidence interval
}

\description{
Estimation of Mean Response and Prediction
of New Observation:

Interval Estimation of E{Y_h}

Confidence Region for Regression Surface. WorkingHotelling confidence band

Simultaneous Confidence Intervals for Several Mean Responses ( Working-Hotelling, Bonferroni confidence limit )

Prediction of New Observation Yh(new)

Prediction of Mean of m New Observations at Xh

Predictions of g New Observations. Simultaneous Scheffe prediction limits for g new observations at g different levels X_h



}
\details{
Type values:

\code{b} Simultaneous Confidence Intervals for Several Mean Responses by Bonferroni simultaneous confidence intervals.

\code{s} Simultaneous Scheffe prediction limits for g new observations at g different levels x_h

\code{w} Confidence Region for Regression Surface or Simultaneous Confidence Intervals for Several Mean Responses

\code{n} Prediction of New Observation.

\code{m} Mean response of New Observation.

\code{nm} When In new observations are to be selected at the same levels X_h and their mean Yh(new) is to be predicted

\code{gn} Simultaneous Bonferroni prediction limits for g new observations at g different levels X_h


\strong{\emph{NOTE:  }}   for the rest of the type states, m = 1.

}
\note{
by default, type is 'b'
}


\references{
Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition
}
\examples{

#### problem 6.14.a page 250
d<-data.frame(282000,7.1,0)
ci.reg(lm(y~x1+x2+x3,GroceryRetailer),d,type='nm',m=3)

#### problem 6.13.b page 250
d<-data.frame(c(230000,250000,280000,340000),c(7.5,7.3,7.1,6.9),c(0,0,0,0))
ci.reg(lm(y~x1+x2+x3,GroceryRetailer),d,type='gn')

#### problem 6.12.a page 250
d<-data.frame(c(302000,245000,280000,350000,295000),c(7.2,7.4,6.9,7,6.7),c(0,0,0,0,1))
ci.reg(lm(y~x1+x2+x3,GroceryRetailer),d,type='b')

### example page 158
d<-data.frame(c(30,65,100))
ci.reg(lm(y~x,TolucaCompany),d,type='w',alpha=.1)

### example page 54
d<-data.frame(65)
ci.reg(lm(y~x,TolucaCompany),d,type='m',alpha=.1)

### example page 59
d<-data.frame(100)
ci.reg(lm(y~x,TolucaCompany),d,type='n',alpha=.1)




}

