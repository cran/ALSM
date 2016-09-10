\name{GroceryRetailer}
\alias{GroceryRetailer}
\docType{data}
\title{Grocery Retailer dataset
%%   ~~ data name/kind ... ~~
}
\description{A large, national grocery retailer tracks productivity and costs of its facilities
closely.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("GroceryRetailer")}
\format{
  A data frame with 52 observations on the following 4 variables.
  \describe{
    \item{\code{y}}{a numeric vector}
    \item{\code{x1}}{a numeric vector}
    \item{\code{x2}}{a numeric vector}
    \item{\code{x3}}{a numeric vector}
  }
}

\source{Michael H. Kutner; Christopher J. Nachtsheim; John Neter; William Li. Applied Linear Statistical Models Fifth Edition  .problem 6.9 page 249
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}

\examples{
data(GroceryRetailer)
## maybe str(GroceryRetailer) ; plot(GroceryRetailer) ...
}
\keyword{datasets}
