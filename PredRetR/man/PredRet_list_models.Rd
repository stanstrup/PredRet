\name{PredRet_list_models}
\alias{PredRet_list_models}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  List PredRet models
}
\description{
  List PredRet models
}
\usage{
  PredRet_list_models()
}
%- maybe also 'usage' for other objects documented here.

\value{
A data.frame with columns "to" and "from" designating the chromatographic systems the models have been calculated between.

}

\author{
  Jan Stanstrup, \email{stanstrup@gmail.com}
}



\examples{
\dontrun{
models_in_db <- PredRet_list_models()
}

}