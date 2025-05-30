% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/initModel.R
\name{initModel}
\alias{initModel}
\title{Initialize the model:}
\usage{
initModel(
  config = NULL,
  path = NULL,
  configFolder = NULL,
  outputFolder = "output",
  references = NULL,
  restart = FALSE,
  runReporting = TRUE,
  sendToSlurm = NULL,
  slurmQOS = NULL,
  tasksPerNode = NULL,
  timeLimit = NULL,
  tasks32 = FALSE
)
}
\arguments{
\item{config}{run configurations}

\item{path}{character vector with folders to run the model in}

\item{configFolder}{character, directory to search for configs. If NULL, the
BRICK-internal config folder is used.}

\item{outputFolder}{directory of output folder}

\item{references}{named character vector of matching references}

\item{restart}{logical or character vector of elements to be restarted.
 If FALSE (default), then no restart is initiated.
 If TRUE, then the run in the given path or the latest run is restarted with default settings.
 Allowed elements of the character vector are:
 \itemize{
 \item \code{"copyGams"} to recopy the Gams scripts (necessary if changes were
       made in Gams code)
 \item \code{"createInput"} to recreate the input data
 \item \code{"createMatching"} to recreate the matching data
 \item \code{"useAsStart"} to use the run from which we restart as the starting point
 \item \code{"none"} (or any other string) to do none of the above
}}

\item{runReporting}{logical, whether to run the reporting, i.e. write the mif}

\item{sendToSlurm}{boolean whether or not the run should be started via SLURM}

\item{slurmQOS}{character, slurm QOS to be used}

\item{tasksPerNode}{numeric, number of tasks per node to be requested}

\item{timeLimit}{character, time limit of the slurm job given in the format hh:mm:ss}

\item{tasks32}{boolean whether or not the SLURM run should be with 32 tasks}
}
\value{
path (invisible)
}
\description{
Preparations of a model run, send the model to SLURM if desired
}
\details{
This function creates the run folder with the necessary config and gams files.
It then either calls the function to start the model directly or passes the model to SLURM.
}
\author{
Ricarda Rosemann
}
