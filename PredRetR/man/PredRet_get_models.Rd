\name{PredRet_get_models}
\alias{PredRet_get_models}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get PredRet models
}
\description{
  Get PredRet modeling data
}
\usage{
  PredRet_get_models(from=NULL,to=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from}{String containing the name of the chromatographic system to model FROM. If NULL all models are pulled.}
  \item{to}{String containing the name of the chromatographic system to model TO. If NULL all models are pulled.}

  }

\value{
 \strong{A list for model containing:}
 
  
  \itemize{
  
  \item \strong{predict_from:} Name of system prediction is done FROM
  \item \strong{predict_to:}   Name of system prediction is done TO
  \item \strong{model_fit:}    dataframe containing the x and y values for the model fit:
                                                        \itemize{
                                                        \item \strong{x:} x values for the model fit
                                                        \item \strong{pred:} y values for the model fit
                                                        \item \strong{lower:} lower limit of confidence interval for y
                                                        \item \strong{upper:} upper limit of confidence interval for y
                                                        }
  \item \strong{model_points:}   dataframe with the points (experimental retention times) used for build the model:
                                                          \itemize{
                                                        \item \strong{first column} Retention times from the system we predict FROM (x)
                                                        \item \strong{second column} Retention times from the system we predict TO (y)
                                                        \item \strong{inchi} InChI's for the entries
                                                        }
  }
  
 


}

\author{
  Jan Stanstrup, \email{stanstrup@gmail.com}
}
