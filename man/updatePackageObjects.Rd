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
updatePackageObjects(pkg_dirpath=".", dry.run=FALSE)
updateAllPackageObjects(all_pkgs, skipped_pkgs=NULL, dry.run=FALSE)

## Low-level utilities:
update_rds_file(filepath, dry.run=FALSE)
update_rda_file(filepath, dry.run=FALSE)
collect_rds_files(dirpath=".")
collect_rda_files(dirpath=".")
}

\arguments{
  \item{pkg_dirpath}{
    The path (as a single string) to the top-level directory of a package
    source tree.
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
    \item \code{\link[BiocGenerics]{updateObject}} in the
          \pkg{BiocGenerics} package.
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
SKIPPED_PKGS <- c("BaalChIP", "cellmigRation", "CytoTree", "isobar", "SNPhood")

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
## (BioC 3.15, 2067 packages) on Nov 17, 2021:
## - took about 11 min
## - sessionInfo() reported between 650 and 700 packages were loaded!
## - required about 9GB of RAM
## > table(candidate_pkgs)
## FALSE  TRUE 
##  1679   388   --> 18% packages contained old S4 instances!
}
}
\keyword{utilities}
