\name{PredRet_plot.pred.cor}
\alias{PredRet_plot.pred.cor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot regression curve between predicted and experimental retention time
}
\description{
  Plot regression curve between predicted and experimental retention time.
}
\usage{
 PredRet_plot.pred.cor(data = PredRet_get_db())
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
p    <- PredRet_plot.pred.cor(data = data)
plot(p)
}

}
