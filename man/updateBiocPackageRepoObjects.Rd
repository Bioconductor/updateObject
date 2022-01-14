\name{updateBiocPackageRepoObjects}

\alias{updateBiocPackageRepoObjects}
\alias{updateAllBiocPackageRepoObjects}

\title{Update the serialized objects contained in a
       Bioconductor package Git repository or in a set of
       Bioconductor package Git repositories}

\description{
  \code{updateBiocPackageRepoObjects()} and
  \code{updateAllBiocPackageRepoObjects()} are wrappers to
  \code{\link{updatePackageObjects}()} and
  \code{\link{updateAllPackageObjects}()} that take care of
  committing and pushing the changes made to the package(s).
}

\usage{
updateBiocPackageRepoObjects(repopath=".", branch=NULL, filter=NULL,
        commit_msg=NULL, push=FALSE, remove.clone.on.success=FALSE,
        git=NULL)

updateAllBiocPackageRepoObjects(all_repopaths=".", skipped_repos=NULL, ...)
}

\arguments{
  \item{repopath}{
    The path (as a single string) to the local Git repository of a
    Bioconductor package.
    See \code{?\link{prepare_git_repo_for_work}} for more information.
  }
  \item{branch}{
    The branch (as a single string) of the Git repository to work on.
    See \code{?\link{prepare_git_repo_for_work}} for more information.
  }
  \item{filter}{
    See \code{?\link{updatePackageObjects}}.
  }
  \item{commit_msg}{
    The Git commit message. By default \code{"Pass serialized S4
    instances thru updateObject()"} is used.
  }
  \item{push}{
    Whether to push the changes or not. Changes are committed but not
    pushed by default. You need push access to the package Git repository
    at \url{git.bioconductor.org} in order to use \code{push=TRUE}.
  }
  \item{remove.clone.on.success}{
    Whether to remove the Git clone on success or not. Only applies
    if \code{repopath} does not exist and needs to be cloned.
  }
  \item{all_repopaths}{
    Character vector of paths to local Git repositories of Bioconductor
    packages.
  }
  \item{skipped_repos}{
    Character vector of repository paths to ignore.
  }
  \item{...}{
    \code{updateAllBiocPackageRepoObjects()} walks over the
    \code{all_repopaths} vector and calls
    \code{updateBiocPackageRepoObjects()} on each repository path.
    All the arguments in \code{...} are passed down to
    \code{updateBiocPackageRepoObjects()}.
  }
  \item{git}{
    See \code{?\link{prepare_git_repo_for_work}}.
  }
}

\value{
  \code{updateBiocPackageRepoObjects()} and
  \code{updateAllBiocPackageRepoObjects()} are wrappers to
  \code{\link{updatePackageObjects}()} and
  \code{\link{updateAllPackageObjects}()}, respectively,
  and return the same value.
}

\seealso{
  \itemize{
    \item \code{\link{updatePackageObjects}} and
          \code{\link{updateAllPackageObjects}}.

    \item Utility functions \code{\link{prepare_git_repo_for_work}}
          and \code{\link{commit_changes}} which are used internally by
          \code{updateBiocPackageRepoObjects} to perform the Git operations.
  }
}

\examples{
## ---------------------------------------------------------------------
## updateBiocPackageRepoObjects()
## ---------------------------------------------------------------------

## On a package with a mix of RDS and RDA files:
repopath <- file.path(tempdir(), "CellBench")
updateBiocPackageRepoObjects(repopath, branch="RELEASE_3_13",
                             remove.clone.on.success=TRUE)

## On a package with no serialized objects:
repopath <- file.path(tempdir(), "BiocGenerics")
updateBiocPackageRepoObjects(repopath, branch="RELEASE_3_13",
                             remove.clone.on.success=TRUE)

## ---------------------------------------------------------------------
## updateAllBiocPackageRepoObjects()
## ---------------------------------------------------------------------

## Let's assume that the current directory is populated with the
## Git repositories of all Bioconductor software packages and that
## we have push access to them:

ALL_REPOS <- dir()  # get list of package repos to update

READ_RDS_FAILURE <- c(
    "BindingSiteFinder",
    "ChIPpeakAnno",
    "drugTargetInteractions"
)

LOAD_FAILURE <- c(
    "AlphaBeta",
    "CellaRepertorium",
    "CNVRanger",
    "gscreend",
    "HiLDA",
    "immunotation",
    "MAST",
    "midasHLA",
    "mixOmics",
    "oligoClasses",
    "TitanCNA",
    "Uniquorn"
)

UPDATEOBJECT_FAILURE <- c(
    "ACE",
    "AnnotationHubData",
    "arrayMvout",
    "Autotuner",
    "BASiCS",
    "bigmelon",
    "Biobase",
    "CAMERA",
    "categoryCompare",
    "cellHTS2",
    "cellmigRation",
    "CEMiTool",
    "CeTF",
    "cleanUpdTSeq",
    "CoGAPS",
    "CoreGx",
    "CrispRVariants",
    "crlmm",
    "decompTumor2Sig",
    "DIAlignR",
    "enhancerHomologSearch",
    "fcoex",
    "geNetClassifier",
    "GreyListChIP",
    "GSgalgoR",
    "hmdbQuery",
    "iCOBRA",
    "MassArray",
    "midasHLA",
    "MinimumDistance",
    "MSnbase",
    "msPurity",
    "multiHiCcompare",
    "musicatk",
    "MutationalPatterns",
    "openPrimeR",
    "OSAT",
    "PharmacoGx",
    "pipeFrame",
    "ProteoDisco",
    "puma",
    "qcmetrics",
    "QDNAseq",
    "r3Cseq",
    "RadioGx",
    "RTN",
    "sangeranalyseR",
    "synapter",
    "tigre",
    "topGO",
    "ToxicoGx",
    "VariantFiltering",
    "wateRmelon",
    "xcms"
)

## Contain files to push larger than 5 Mb.
PUSH_FAILURE <- c(
    "BiocSklearn",
    "BubbleTree",
    "CINdex",
    "erma",
    "ivygapSE",
    "SplicingGraphs",
    "vtpnet"
)

## Skipped for other reasons e.g. contain objects for which
## updateObject() takes forever or the package needs to be
## installed but cannot at the moment.
OTHER_SKIPPED_REPOS <- c(
    "BaalChIP", "BiGGR", "CytoTree", "gwascat",
    "mirIntegrator", "oposSOM", "PFP", "ROntoTools", "SLGI"
)

SKIPPED_REPOS <- c(
    READ_RDS_FAILURE,
    LOAD_FAILURE,
    UPDATEOBJECT_FAILURE,
    PUSH_FAILURE,
    OTHER_SKIPPED_REPOS
)

FILTER <- "\\bDataFrame\\b"

\dontrun{
system.time(
    codes <- updateAllBiocPackageRepoObjects(ALL_REPOS,
                                             skipped_repos=SKIPPED_REPOS,
                                             branch="master",
                                             filter=FILTER,
                                             push=TRUE)
)
}
}
\keyword{utilities}
