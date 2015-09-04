\name{PredRet_plot.model.fit}
\alias{PredRet_plot.model.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Model fit
}
\description{
  Plot the model fit between two chromatographic systems.
}
\usage{
 PredRet_plot.model.fit(   from,to  )
}
%- maybe also 'usage' for other objects documented here.


\arguments{
  \item{from}{String containing the name of the chromatographic system to model FROM. If NULL all models are pulled.}
  \item{to}{String containing the name of the chromatographic system to model TO. If NULL all models are pulled.}
  }

\value{
A ggplot2 object. Red dots are compounds that were marked as suspicious (not necessarily in the displayed model).

}

\author{
  Jan Stanstrup, \email{stanstrup@gmail.com}
}

\examples{
\dontrun{
p    <- PredRet_plot.model.fit(from="LIFE_old",to="LIFE_new")
plot(p)
}

}
