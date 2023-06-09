% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/condor_rmdir.R
\name{condor_rmdir}
\alias{condor_rmdir}
\title{Condor Remove Directory}
\usage{
condor_rmdir(run.dir, top.dir = "condor", quiet = FALSE, session = NULL)
}
\arguments{
\item{run.dir}{name of a Condor run directory inside \code{top.dir}.}

\item{top.dir}{top directory on submitter machine that contains Condor run
directories.}

\item{quiet}{whether to suppress messages.}

\item{session}{optional object of class \code{ssh_connect}.}
}
\value{
No return value, called for side effects.
}
\description{
Remove directories on the submitter machine.
}
\details{
The default value of \code{session = NULL} looks for a \code{session} object
in the user workspace. This allows the user to run Condor functions without
explicitly specifying the \code{session}.
}
\examples{
\dontrun{

# General workflow
session <- ssh_connect("servername")

condor_submit()
condor_q()
condor_dir()
condor_download()  # after job has finished

# Remove one or more directories
condor_rmdir("01_this")                 # remove ~/condor/01_this (one run)
condor_rmdir(c("01_this", "02_that"))   # remove two model runs inside condor
condor_rmdir("test_runs", top.dir=".")  # remove ~/my_runs (many subdirs)
}

}
\seealso{
\code{\link{condor_submit}}, \code{\link{condor_q}},
\code{\link{condor_dir}}, and \code{condor_download} provide the main Condor
interface.

\code{\link{condor_rm}} stops Condor jobs and \code{\link{condor_rmdir}}
removes directories on the submitter machine.

\code{\link{condor-package}} gives an overview of the package.
}
\author{
Arni Magnusson.
}
