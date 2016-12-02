\name{oneway}
\alias{oneway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Single-Factor ANOVA
}
\description{
Fitting of ANOVA Model

Analysis of Variance (test & table)

Test and confidence interval for Single Factor level Mean

inferences for Difference between Two Factor level Means

Contrast of Factor level Means

test and confidence interval for linear Combination of Factor level Means

Analysis of Means (CI for treatment effects)

\strong{*** Tests for Constancy of Error Variance: ***}

Hartley Test

Brown-Forsythe Test

\strong{***   Simultaneos Inference Procedures:  ***}

Tukey multiple comparison procedure

Scheffe multiple comparison procedure

Bonferroni multiple comparison procedure

\strong{***   Nonparametric  ***}


Nonparametric Rank F Test and multiple Pairwise Testing Procedure


\strong{***A variety of residuals in the output: ***}

residuals

semistudentized residuals

studentized residuals

studentized deleted  residuals


\strong{*** PLOT ***}

boxplot by factors

Line Plot of Estimated Factor Level Mean

Bar Graph and Main Effects Plot

bar graph and the main etfects plot of factor level  means is to display the confidence
limits


\strong{*** PLOT for residuals: ***}

against fitted value

Aligned Residual Dot Plot

Normal Q-Q plot

histogram

boxplot




}
\usage{
oneway(y, group, alpha=0.05,MSE=NULL, c.value=0, mc=NULL, residual,omission.variable=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{vector, response variable
%%     ~~Describe \code{y} here~~
}
  \item{group}{vector INTEGER, group variable
%%     ~~Describe \code{group} here~~
}
  \item{alpha}{Confidence interval calculate with 1-alpha percent

%%     ~~Describe \code{alpha} here~~
}
  \item{MSE}{mean squre of error
%%     ~~Describe \code{MSE} here~~
}
  \item{c.value}{c value for single factor test. H_0: mu_i=c
%%     ~~Describe \code{c.value} here~~
}
  \item{mc}{Matrix contrast(s), Each row was included in a contrast
%%     ~~Describe \code{mc} here~~
}
  \item{residual}{Type of residuals.  simple (default), semistudentized,  studentized  or  studentized.deleted
%%     ~~Describe \code{residual} here~~
}
\item{omission.variable}{numeric vector. Omission of Important Explanatory Variables. Residual analysis may also be used to study whether or not the single-factor ANOVA model is an adequate model.
%%     ~~Describe \code{mc} here~~
}
}

\value{
test & CI
}
\references{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition. chapter 16, 17 and 18
%% ~put references to the literature/web site here ~
}

\examples{
library('ALSM')
library('SuppDists')

with(KentonFoodCompany,oneway(y,group,mc=matrix(c(.5,.5,-.5,-.5),1,4)))


}
