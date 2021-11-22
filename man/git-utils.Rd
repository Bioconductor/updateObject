\name{git-utils}

\alias{git-utils}
\alias{prepare_git_repo_for_work}
\alias{bump_version_and_commit}

\title{Convenience Git-related utility functions used by
       updateBiocPackageRepoObjects()}

\description{
  \code{prepare_git_repo_for_work()} and \code{bump_version_and_commit()}
  are used internally by \code{\link{updateBiocPackageRepoObjects}()} to
  perform the git operations.

  Note that they both call Python scripts located in the BBS (Bioconductor
  Build System) repository (\url{https://github.com/Bioconductor/BBS}) at
  the moment, so a local copy of BBS is needed.
  This requirement might go away in the future.
}

\usage{
prepare_git_repo_for_work(repopath=".", branch=NULL,
                          BBS_HOME=NULL, python=NULL)

bump_version_and_commit(repopath=".", commit_msg=NULL, push=FALSE,
                        BBS_HOME=NULL, python=NULL)
}

\arguments{
  \item{repopath}{
    The path (as a single string) to the local Git repository of a
    Bioconductor package.

    If the specified path does not exist, \code{prepare_git_repo_for_work()}
    will try to infer the package name from \code{repopath} and clone it
    from \url{git.bioconductor.org}.
  }
  \item{branch}{
    The branch (as a single string) of the Git repository to work on.

    If \code{NULL}, then the current branch is used (if \code{repopath}
    already exists) or the default branch is used (if \code{repopath}
    does not exist and needs to be cloned).
  }
  \item{commit_msg}{
    The Git commit message. By default \code{"version bump"} is used.
  }
  \item{push}{
    Whether to push the changes or not. Changes are committed but not
    pushed by default. You need push access to the package Git repository
    at \url{git.bioconductor.org} in order to use \code{push=TRUE}.
  }
  \item{BBS_HOME,python}{
    Where to find the local BBS repository and Python.

    If Python is in the PATH, it will be found and everything should be fine.
    If it's not, then please provide the path to the Python executable to
    use via the \code{python} argument. Python 3 is required.
  }
}

\seealso{
  \itemize{
    \item \code{\link{updateBiocPackageRepoObjects}}.
  }
}

\examples{
repopath <- file.path(tempdir(), "limma")

prepare_git_repo_for_work(repopath)

## Note that the 'Date:' field (if present) gets updated to the
## current date:
bump_version_and_commit(repopath, push=FALSE)

unlink(repopath, recursive=TRUE)
}
\keyword{utilities}
