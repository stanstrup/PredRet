\name{PredRet_plot.pred.error.rel}
\alias{PredRet_plot.pred.error.rel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Relative prediction error
}
\description{
  Plot violin plot of relative prediction error for each chromatographic system in the database.
}
\usage{
 PredRet_plot.pred.error.rel(data = PredRet_get_db())
}
%- maybe also 'usage' for other objects documented here.


\arguments{
  \item{data}{The database data.frame as written by \code{\link{PredRet_get_db}}}
  }

\value{
A ggplot2 object.

}

\author{
  Jan Stanstrup, \email{stanstrup@gmail.com}
}

\examples{
\dontrun{
data <- PredRet_get_db()
p    <- PredRet_plot.pred.error.rel(data = data)
plot(p)
}

}
