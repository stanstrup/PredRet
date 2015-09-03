\name{PredRet_plot.db.count}
\alias{PredRet_plot.db.count}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plot number of compounds and number of predictions for each system.
}
\description{
  Plot number of compounds and number of predictions for each system.
}
\usage{
 PredRet_plot.db.count(data = PredRet_get_db())
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
p    <- PredRet_plot.db.count(data = data)
plot(p)
}

}
