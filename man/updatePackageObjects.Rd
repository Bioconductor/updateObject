\name{updatePackageObjects}

\alias{collect_rds_files}
\alias{collect_rda_files}
\alias{update_rds_file}
\alias{update_rda_file}
\alias{updatePackageObjects}
\alias{updateAllPackageObjects}

\title{Update all the serialized objects contained in a package
       or in a set of packages}

\description{
  Use \code{updatePackageObjects()} to update all the serialized objects
  contained in a package.

  Use \code{updateAllPackageObjects()} to update all the serialized objects
  contained in a set of packages.
}

\usage{
updatePackageObjects(pkg_dirpath=".",
                     filter=NULL, dry.run=FALSE)

updateAllPackageObjects(all_pkgs, skipped_pkgs=NULL,
                     filter=NULL, dry.run=FALSE)

## Low-level utilities:
update_rds_file(filepath, filter=NULL, dry.run=FALSE)
update_rda_file(filepath, filter=NULL, dry.run=FALSE)
collect_rds_files(dirpath=".")
collect_rda_files(dirpath=".")
}

\arguments{
  \item{pkg_dirpath}{
    The path (as a single string) to the top-level directory of a package
    source tree.
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
    back to their original files. Set \code{dry.run} to \code{TRUE} to
    perform a trial run with no changes made.
  }
  \item{all_pkgs}{
    Character vector of package names. All the corresponding package
    source trees must be in the current directory.
  }
  \item{skipped_pkgs}{
    Character vector of package names to ignore.
  }
  \item{filepath}{
    The path (as a single string) to a file containing serialized objects.
    This must be an RDS file (for \code{collect_rds_files}) or RDA file
    (\code{collect_rda_files}).
  }
  \item{dirpath}{
    The path (as a single string) to an arbitrary directory.
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
  \code{updatePackageObjects()} returns the number of updated files or
  a negative error code (-2 if loading an RDS or RDA file failed, -1 if
  \code{updateObject()} returned an error).

  \code{updateAllPackageObjects()} returns a named integer vector
  \emph{parallel} to \code{all_pkgs}.

  \code{update_rds_file()} and \code{update_rda_file()} return -2 if
  loading the RDS or RDA file failed, -1 if \code{updateObject()} returned
  an error, 0 if there was nothing to update in the file, and 1 if the
  file got updated.

  \code{collect_rds_files()} and \code{collect_rda_files()} return a
  character vector of (relative) file paths.
}

\seealso{
  \itemize{
    \item The \code{\link[BiocGenerics]{updateObject}} generic function
          in the \pkg{BiocGenerics} package.

    \item The \code{\link[utils]{capture.output}} function in the \pkg{utils}
          package.

    \item The \code{\link[digest]{digest}} function in the \pkg{digest}
          package.
  }
}

\examples{
\dontrun{
## ---------------------------------------------------------------------
## A SIMPLE updatePackageObjects() EXAMPLE
## ---------------------------------------------------------------------

## Note that we don't need to perform two passes ("dry run" + "real run"),
## one pass is enough. Here we show the 2-pass procedure only for
## illustrative purpose.

pkg_url <- "https://git.bioconductor.org/packages/CellBench"
system2("git", c("clone", pkg_url))
setwd("CellBench")
code <- updatePackageObjects(dry.run=TRUE)
if (code > 0) {
    updatePackageObjects()  # do it for good!
    ## Use an editor to bump the version in the DESCRIPTION file.
    commit_msg <- "'Pass serialized S4 instances thru updateObject()'"
    system2("git", c("commit", "-a", "-m", commit_msg))
    setwd("..")
}

## ---------------------------------------------------------------------
## FIND CANDIDATE PACKAGES IN CURRENT DIRECTORY
## ---------------------------------------------------------------------

## In this example we perform a "dry run" with updateAllPackageObjects()
## to find all the packages in a directory that contain old serialized
## objects.

## Load some packages upfront that are known to define updateObject()
## methods:
library(SummarizedExperiment)
library(GenomicAlignments)
library(DelayedArray)
library(HDF5Array)
library(SingleCellExperiment)
library(MultiAssayExperiment)
library(bsseq)
library(InteractionSet)
library(GSEABase)
library(Gviz)

## Let's assume that the current directory is populated with package
## git clones:
all_pkgs <- dir()  # get list of packages

## If we know that some packages are going to cause problems, we should
## skip them. Note that we could just do
##
##   all_pkgs <- setdiff(all_pkgs, SKIPPED_PKGS)
##
## for this. However, by using the 'skipped_pkgs' argument, all the
## packages will be represented in the returned vector, including the
## skipped packages:
SKIPPED_PKGS <- c("BaalChIP", "CytoTree", "isobar", "SNPhood")

## --- Without a filter ---

## updateAllPackageObjects() will stop with an error if a package is
## required but not installed. The user is responsible for installing
## all the required packages (this can be hard to know in advance).
## Having pre-loaded a bunch of packages above only increases our chance
## of a smooth run.
codes <- updateAllPackageObjects(all_pkgs, skipped_pkgs=SKIPPED_PKGS,
                                 dry.run=TRUE)

sessionInfo()  # many packages
table(codes)

## The above code was successfully run in the MEAT0 folder on nebbiolo1
## (BioC 3.15, 2067 packages) on Nov 18, 2021:
## - took about 14 min
## - loaded 1190 packages (as reported by sessionInfo())
## - required about 9GB of RAM
##
## > table(codes)
## codes
## codes
##   -3   -2   -1    0    1    2    3    4    5    6    7    8    9   10
##    4   15   66 1549  240   90   46   28    7    5    4    3    3    1
##   13   18   20   23  125
##    2    1    1    1    1
##
## > sum(codes > 0) / length(codes)
## [1] 0.2094823
## 21% packages with old S4 instances!

## --- With a filter ---

## We want to filter on the presence of the **word** "DataFrame" in
## the output of 'updateObject( , check=FALSE, verbose=TRUE)'. We can't
## just set 'filter' to '"DataFrame" for that as this would also produce
## matches in the presence of strings like "AnnotatedDataFrame":
filter <- "\\bDataFrame\\b"

codes <- updateAllPackageObjects(all_pkgs, skipped_pkgs=SKIPPED_PKGS,
                                 filter=filter,
                                 dry.run=TRUE)

}
}
\keyword{utilities}
