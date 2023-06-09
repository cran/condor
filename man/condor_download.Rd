% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/condor_download.R
\name{condor_download}
\alias{condor_download}
\title{Condor Download}
\usage{
condor_download(run.dir = NULL, local.dir = ".", top.dir = "condor",
  create.dir = FALSE, pattern = "End.tar.gz|condor.*(err|log|out)$",
  overwrite = FALSE, untar.end = TRUE, session = NULL)
}
\arguments{
\item{run.dir}{name of a Condor run directory inside \code{top.dir}.}

\item{local.dir}{local directory to download to.}

\item{top.dir}{top directory on submitter machine that contains Condor run
directories.}

\item{create.dir}{whether to create \code{local.dir} if it does not exist.}

\item{pattern}{regular expression identifying which result files to download.
Passing \code{pattern="*"} will download all files.}

\item{overwrite}{whether to overwrite local files if they already exist.}

\item{untar.end}{whether to extract \code{End.tar.gz} into
\emph{local.dir} after downloading. (Ignored if a file named
\file{End.tar.gz} was not downloaded.)}

\item{session}{optional object of class \code{ssh_connect}.}
}
\value{
No return value, called for side effects.
}
\description{
Download results from a Condor job.
}
\details{
The default value of \code{run.dir = NULL} looks for Condor job results in
\emph{top.dir}\code{/}\emph{local.dir}. For example, if
\code{local.dir = "c:/yft/run01"} then the default \code{run.dir} becomes
\code{"condor/run01"}.

The default value of \code{pattern="End.tar.gz|condor.*(err|log|out)$"}
downloads \code{End.tar.gz} and Condor log files. For many analyses, it can
be convenient to pack all results into End.tar.gz to make it easy to find,
download, and manage output files.

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

# Alternatively, download specific run to specific folder
condor_download("01_this_model", "c:/myruns/01_this_model")
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
