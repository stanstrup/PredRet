\name{PredRet_plot.db.graph}
\alias{PredRet_plot.db.graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  PredRet database network graph
}
\description{
  PredRet database network graph.
}
\usage{
PredRet_plot.db.graph(database = PredRet_get_db(exp_pred = "exp"),
                        models = PredRet_get_models(),
                        sys_db = PredRet_get_chrom_systems(),
                      circular = TRUE,
                only_connected = TRUE              )
}
%- maybe also 'usage' for other objects documented here.


\arguments{
  \item{database}{The database data.frame as written by \code{\link{PredRet_get_db}}}
  \item{models}{The PredRet models list as written by \code{\link{PredRet_get_models}}}
  \item{sys_db}{The chromatographic system definitions list as written by \code{\link{PredRet_get_chrom_systems}}}
  \item{circular}{Plot a circular graph}
  \item{only_connected}{Plot only chromatographic systems that have overlap with at least one otherchromatographic  system.}
  }

\value{
The db network graph is plotted and the edge list returned.

}

\author{
  Jan Stanstrup, \email{stanstrup@gmail.com}
}

\examples{
\dontrun{
database <- PredRet_get_db(exp_pred = "exp")
models <- PredRet_get_models()
sys_db <- PredRet_get_chrom_systems()

d <- PredRet_plot.db.graph(database          = database,
                              models         = models,
                              sys_db         = sys_db,
                              circular       = TRUE,
                              only_connected = TRUE
)

}

}
