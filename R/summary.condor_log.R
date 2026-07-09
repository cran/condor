#' Summary Condor Log
#'
#' Produce a summary of a Condor log file.
#'
#' @param object an object of class \code{\link{condor_log}}.
#' @param details whether to include columns with additional details:
#'        \code{disk}, \code{memory}, \code{cpus}, and \code{machine}.
#' @param ... passed to \code{round}.
#'
#' @return
#' Data frame with the following columns:
#' \item{job.id}{job id.}
#' \item{status}{text indicating whether job status is submitted, executing,
#'   aborted, or finished.}
#' \item{submit.time}{date and time when job was submitted.}
#' \item{runtime}{total duration of a job.}
#'
#' If \code{details = TRUE}, the data frame also contains the following columns:
#' \item{disk}{disk space used by job (MB).}
#' \item{memory}{memory used by job (MB).}
#' \item{cpus}{number of cores used by job.}
#' \item{machine}{machine running the job.}
#'
#' @seealso
#' \code{\link{condor_log}} shows Condor log file.
#'
#' \code{\link{condor-package}} gives an overview of the package.
#'
#' @author Arni Magnusson.
#'
#' @examples
#' \dontrun{
#'
#' # Examine log files on submitter machine
#' session <- ssh_connect("servername")
#'
#' condor_dir()
#' condor_log()
#' summary(condor_log())
#'
#' #' # Alternatively, examine log files on local drive
#' condor_dir(local.dir="c:/myruns")
#' condor_log(local.dir="c:/myruns/01_this_model")
#' summary(condor_log(local.dir="c:/myruns/01_this_model"))
#' }
#'
#' @importFrom utils type.convert
#'
#' @export

summary.condor_log <- function(object, details=FALSE, ...)
{
  job.id <- gsub(".*\\(([0-9]+)\\..*", "\\1", object[1])
  job.id <- type.convert(job.id, as.is=TRUE)
  if(!is.integer(job.id))
    job.id <- NA_integer_

  status <- if(any(grepl("Job terminated", object))) {
              "finished"
            } else if(any(grepl("Job was aborted", object))) {
              "aborted"
            } else if(any(grepl("Job executing", object))) {
              "executing"
            } else if(any(grepl("Job submitted", object))) {
              "submitted"
            } else
              NA_character_

  submit.time <- gsub(".*\\) (.*) Job.*", "\\1", object[1])
  if(!grepl(":", submit.time))
    submit.time <- NA_character_

  runtime <- grep("Total Remote Usage", object, value=TRUE)
  hms <- gsub(".* (.*),.*", "\\1", runtime)  # hr, min, sec
  hms <- unlist(strsplit(hms, ":"))
  days <- gsub(".*Usr ([0-9]*).*", "\\1", runtime)
  hms[1] <- 24 * as.integer(days) + as.integer(hms[1])
  runtime <- paste(hms, collapse=":")
  if(length(runtime) == 0 || runtime == "")
    runtime <- NA_character_

  disk <- grep("Disk \\(KB\\)", object, value=TRUE)
  disk <- tail(disk, 1)  # final disk usage, if many values are reported
  disk <- gsub(".*: *([0-9]*) .*", "\\1", disk)
  disk <- type.convert(disk, as.is=TRUE)
  disk <- round(disk / 1024, ...)
  if(length(disk) == 0)
    disk <- NA_integer_

  memory <- grep("Memory \\(MB\\)", object, value=TRUE)
  memory <- tail(memory, 1)  # final memory usage, if many values are reported
  memory <- gsub(".*: *([0-9]*) .*", "\\1", memory)
  memory <- type.convert(memory, as.is=TRUE)
  memory <- round(memory, ...)
  if(length(memory) == 0)
    memory <- NA_integer_

  cpus <- grep("Cpus =", object, value=TRUE)
  cpus <- tail(cpus, 1)  # final number of cpus, if many values are reported
  cpus <- gsub(".*= ", "", cpus)
  cpus <- as.integer(cpus)

  machine <- grep("SlotName: ", object, value=TRUE)
  machine <- tail(machine, 1)  # final machine used, if many values are reported
  machine <- gsub(".*@(.*?)\\..*", "\\1", machine)

  if(details)
  {
    data.frame(job.id, status, submit.time, runtime, disk, memory, cpus,
               machine)
  }
  else
  {
    data.frame(job.id, status, submit.time, runtime)
  }
}
