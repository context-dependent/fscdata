% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf_read_fsc_project.R
\name{sf_read_fsc}
\alias{sf_read_fsc}
\title{Import Sf / Qx records one or more FSC projects}
\usage{
sf_read_fsc(programs = NULL)
}
\arguments{
\item{programs}{A vector of program names to include in the query to salesforce}
}
\value{
A list of the following tibbles:
\itemize{
\item enrollments: tombstone record for each participant
\item surveys: raw qualtrics exports, linked logically to enrollments
\item components: program component completion status
}
}
\description{
This function returns a list of tibbles that comprise the research-relevant
contents of the FSC Evaluation Database for one or more programs.
If no programs are specified, records for all programs are returned.
}
\examples{
all_records <- sf_read_fsc()
fast_data <- sf_read_fsc(programs = "FAST")
fast_and_climb <- sf_read_fsc(programs = "CLIMB") 
}
