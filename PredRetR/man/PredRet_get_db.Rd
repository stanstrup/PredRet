\name{PredRet_get_db}
\alias{PredRet_get_db}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get PredRet retention times
}
\description{
  Downloads PredRet retention times directly from the database.
}
\usage{
  PredRet_get_db(userID          = NULL,
                 exp_pred        = c("exp","pred"), 
                 include_suspect = TRUE
                 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{userID}{Select specific user to get retention times for. Defaults to all data.}
  \item{exp_pred}{Get experimental ("exp") and/or predicted (pred) retention times only.}
  \item{include_suspect}{Include values marked as suspect.}
}

\value{
 \strong{A datafame containing:}
 
\strong{system:} system name\cr
\strong{name:} compound name\cr
\strong{pubchem:} pubchem ID\cr
\strong{inchi:} InChI\cr
\strong{data added:} date the entry was added\cr
\strong{username:} username of the uploader\cr
\strong{predicted:} TRUE/FALSE if the value is predicted or not (i.e. experimental)\cr
\strong{suspect:} TRUE/FALSE if the value is suspected as being wrong\cr
\strong{recorded_rt:} Experimental retention time\cr
\strong{predicted_rt:} Predicted retention time\cr
\strong{ci_lower:} Lower limit of confidence interval\cr
\strong{ci_upper:} Upper limit of confidence interval
}

\author{
  Jan Stanstrup, \email{stanstrup@gmail.com}
}
