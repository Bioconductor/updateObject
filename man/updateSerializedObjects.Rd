\name{updateSerializedObjects}

\alias{updateSerializedObjects}
\alias{collect_rds_files}
\alias{collect_rda_files}
\alias{update_rds_file}
\alias{update_rda_file}

\title{Update the serialized objects contained in a directory}

\description{
  Use \code{updateSerializedObjects()} to find and update all the
  serialized objects contained in a directory. This is the workhorse
  behind higher-level functions \code{\link{updatePackageObjects}()}
  and family (\code{\link{updateAllPackageObjects}()},
  \code{\link{updateBiocPackageRepoObjects}()}, and
  \code{\link{updateAllBiocPackageRepoObjects}()}).

  \code{collect_rds_files()}, \code{collect_rda_files()},
  \code{update_rds_file()}, and \code{update_rda_file()} are the
  low-level utilities used internally by \code{updateSerializedObjects()}
  to do the job.
}

\usage{
updateSerializedObjects(dirpath=".", recursive=FALSE,
                        filter=NULL, dry.run=FALSE)

## Low-level utilities upon which updateSerializedObjects() is built:
collect_rds_files(dirpath=".", recursive=FALSE)
collect_rda_files(dirpath=".", recursive=FALSE)
update_rds_file(filepath, filter=NULL, dry.run=FALSE)
update_rda_file(filepath, filter=NULL, dry.run=FALSE)
}

\arguments{
  \item{dirpath}{
    The path (as a single string) to an arbitrary directory.
  }
  \item{recursive}{
    \code{TRUE} or \code{FALSE}. Should the directory be searched recursively
    to find the objects to update? By default the directory is \emph{not}
    searched recursively.
  }
  \item{filter}{
    \code{NULL} (the default) or a single string containing a regular
    expression.

    When \code{filter} is set, only objects for which there is a match
    in the output of \code{updateObject(object, check=FALSE, verbose=TRUE)}
    actually get replaced with the object returned by the \code{updateObject}
    call. See Details section below for more on this.

    Note that the pattern matching is \emph{case sensitive}.
  }
  \item{dry.run}{
    \code{TRUE} or \code{FALSE}. By default, updated objects are written
    back to their original file. Set \code{dry.run} to \code{TRUE} to
    perform a trial run with no changes made.
  }
  \item{filepath}{
    The path (as a single string) to a file containing serialized objects.
    This must be an RDS file (for \code{update_rds_file}) or RDA file
    (for \code{update_rda_file}).
  }
}

\details{
  \code{update_rds_file()} and \code{update_rds_file()} use
  \code{\link[BiocGenerics]{updateObject}()} internally to update
  individual R objects.

  If no filter is specified (the default), each object is updated with
  \code{object <- updateObject(object, check=FALSE)}. If that turns out
  to be a no-op, then code 0 ("nothing to update") is returned.
  Otherwise 1 is returned.

  If a filter is specified (via the \code{filter} argument) then
  \code{updateObject(object, check=FALSE, verbose=TRUE)} is called on each
  object and the output of the call is captured with \code{capture.output()}.
  Only if the output contains a match for \code{filter} is the object
  replaced with the object returned by the call. If this replacement turns
  out to be a no-op, or if the output contained no match for \code{filter},
  then code 0 ("nothing to update") is returned. Otherwise 1 is returned.

  The pattern matching is \emph{case sensitive}.

  Note that determining whether a call to \code{updateObject()} is a no-op
  or not is done by calling \code{digest::\link[digest]{digest}()} on the
  original object and object returned by \code{updateObject()}, and by
  comparing the 2 hash values. This is a LOT MORE reliable than using
  \code{identical()} which is notoriously unreliable!
}

\value{
  \code{updateSerializedObjects()} returns a single integer which is the
  number of updated files or a negative error code (-2 if loading an RDS
  or RDA file failed, -1 if \code{updateObject()} returned an error).

  \code{collect_rds_files()} and \code{collect_rda_files()} return a
  character vector of (relative) file paths.

  \code{update_rds_file()} and \code{update_rda_file()} return a single
  integer which is one of the following codes:
  \itemize{
    \item -2 if loading the RDS or RDA file failed;
    \item -1 if \code{updateObject()} returned an error;
    \item 0 if there was nothing to update in the file;
    \item 1 if the file got updated.
  }
}

\seealso{
  \itemize{
    \item The \code{\link{updatePackageObjects}} function which is just a
          thin wrapper around \code{updateSerializedObjects}.

    \item The \code{\link[BiocGenerics]{updateObject}} generic function
          in the \pkg{BiocGenerics} package.

    \item The \code{\link[utils]{capture.output}} function in the \pkg{utils}
          package.

    \item The \code{\link[digest]{digest}} function in the \pkg{digest}
          package.
  }
}

\examples{
dirpath <- system.file("extdata", package="updateObject")

## ---------------------------------------------------------------------
## WITHOUT A FILTER
## ---------------------------------------------------------------------

## updateSerializedObjects() prints one line per processed file:
updateSerializedObjects(dirpath, recursive=TRUE, dry.run=TRUE)

## Note that updateSerializedObjects() needs to attach/load the packages
## in which the classes of the objects to update are defined. These
## packages are: GenomicRanges for GRanges objects, SummarizedExperiment
## for SummarizedExperiment objects, and InteractionSet for GInteractions
## objects. This means that sessionInfo() will typically report more
## attached and loaded packages after a updateSerializedObjects() run
## than before:
sessionInfo()

## Also updateSerializedObjects() will raise an error if it fails to
## attach or load a package (typically because the package is missing).
## It will NOT try to install the package.

## ---------------------------------------------------------------------
## WITH A FILTER
## ---------------------------------------------------------------------

## We want to filter on the presence of the **word** "DataFrame" in
## the output of 'updateObject( , check=FALSE, verbose=TRUE)'. We can't
## just set 'filter' to '"DataFrame" for that as this would also produce
## matches in the presence of strings like "AnnotatedDataFrame":
filter <- "\\bDataFrame\\b"

updateSerializedObjects(dirpath, recursive=TRUE, filter=filter,
                        dry.run=TRUE)
}
\keyword{utilities}
