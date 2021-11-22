\name{updateBiocPackageRepoObjects}

\alias{updateBiocPackageRepoObjects}
\alias{updateAllBiocPackageRepoObjects}

\title{Update all the serialized objects contained in a
       Bioconductor package Git repository or in a set of
       Bioconductor package Git repositories}

\description{
  \code{updateBiocPackageRepoObjects()} and
  \code{updateAllBiocPackageRepoObjects()} are wrappers to
  \code{\link{updatePackageObjects}()} and
  \code{\link{updateAllPackageObjects}()} that take care of
  committing and pushing the changes made to the packages.
}

\usage{
updateBiocPackageRepoObjects(repopath=".", branch=NULL, filter=NULL,
        commit_msg=NULL, push=FALSE, BBS_HOME=NULL, python=NULL)

updateAllBiocPackageRepoObjects(all_repopaths=".", skipped_repos=NULL, ...)
}

\arguments{
  \item{repopath}{
    The path (as a single string) to the local Git repository of a
    Bioconductor package.
  }
  \item{branch}{
    The branch (as a single string) of the Git repository to work on.

    If \code{NULL}, then the current branch is used (if \code{repopath}
    already exists) or the default branch is used (if \code{repopath} does
    not exist and needs to be cloned).
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
    at git.bioconductor.org to use \code{push=TRUE}.
  }
  \item{BBS_HOME}{
    The path (supplied as a single string) to a directory containing
    the BBS code.
  }
  \item{python}{
    If Python is in the PATH, it will be found and everything should be fine.
    If it's not, then provide the path to the Python executable to use via
    the \code{python} argument. Python 3 is required.
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
    All the arguments passed thru \code{...} are passed down to
    \code{updateBiocPackageRepoObjects()}.
  }
}

\details{
  The git operations are performed by Python scripts located in the BBS
  repository (\url{https://github.com/Bioconductor/BBS}). A local copy of
  BBS is needed.
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
  }
}

\examples{
## Let's assume that the current directory is populated with the
## Git repositories of all Bioconductor software packages:
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
    "AffiXcan",
    "animalcules",
    "AnnotationHubData",
    "arrayMvout",
    "Autotuner",
    "BASiCS",
    "bigmelon",
    "Biobase",
    "BiocOncoTK",
    "BiSeq",
    "CAMERA",
    "categoryCompare",
    "cellHTS2",
    "cellmigRation",
    "CEMiTool",
    "CeTF",
    "cleanUpdTSeq",
    "clustifyr",
    "CoGAPS",
    "CoreGx",
    "CrispRVariants",
    "crlmm",
    "decompTumor2Sig",
    "DIAlignR",
    "enhancerHomologSearch",
    "fcoex",
    "GeneRegionScan",
    "geNetClassifier",
    "GOpro",
    "GreyListChIP",
    "GSgalgoR",
    "hipathia",
    "hmdbQuery",
    "iCOBRA",
    "LOBSTAHS",
    "MassArray",
    "midasHLA",
    "MinimumDistance",
    "missRows",
    "MSnbase",
    "msPurity",
    "msqrob2",
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
    "QFeatures",
    "r3Cseq",
    "RadioGx",
    "RTN",
    "sangeranalyseR",
    "scp",
    "StarBioTrek",
    "synapter",
    "tigre",
    "TimiRGeN",
    "topGO",
    "ToxicoGx",
    "VariantFiltering",
    "wateRmelon",
    "xcms"
)

## Contain files to push larger than 5 Mb
PUSH_FAILURE <- c(
    "BiocSklearn",
    "BubbleTree",
    "CINdex",
    "erma",
    "ivygapSE",
    "SplicingGraphs",
    "vtpnet"
)

OTHER_SKIPPED_REPOS <- c(
    "BaalChIP", "BiGGR", "CytoTree", "GeneAnswers",
    "gwascat", "isobar", "mirIntegrator", "oposSOM", "PFP", "ROntoTools",
    "SLGI", "SNPhood"
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
                                             filter=FILTER,
                                             push=TRUE)
)
}
}
\keyword{utilities}
