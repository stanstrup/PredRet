\name{PredRet_get_chrom_systems}
\alias{PredRet_get_chrom_systems}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Get PredRet chromatographic systems
}
\description{
  Get PredRet chromatographic systems and all descriptors
}
\usage{
  PredRet_get_chrom_systems()
}
%- maybe also 'usage' for other objects documented here.


\value{
 \strong{A list for each chromatographic system containing:}
 
\strong{username:}  username of the uploader\cr
\strong{system_name:} system name\cr
\strong{system_desc:} system description\cr
\strong{system_eluent:} Eluent description\cr
\strong{system_eluent_pH:} Eluent pH (acidic/alkaline)\cr
\strong{system_eluent_additive:} Eluent additive description\cr
\strong{system_column:} Name of the column used \cr
\strong{system_column_type:} Type of column used\cr
\strong{system_ref:} Reference. can be doi or link \cr
}

\author{
  Jan Stanstrup, \email{stanstrup@gmail.com}
}
