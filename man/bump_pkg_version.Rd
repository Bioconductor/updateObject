\name{bump_pkg_version}

\alias{bump_pkg_version}

\title{Bump the version of a package}

\description{
  Use \code{bump_pkg_version()} to bump the version of a package.
}

\usage{
bump_pkg_version(pkgpath=".", update.Date=FALSE)
}

\arguments{
  \item{pkgpath}{
    The path (as a single string) to the top-level directory of a package
    source tree.
  }
  \item{update.Date}{
    \code{TRUE} or \code{FALSE}. If \code{TRUE} then the Date field (if
    present) gets updated to the current date.
  }
}

\seealso{
  \itemize{
    \item \code{\link{updatePackageObjects}} which uses
          \code{bump_pkg_version} internally when \code{bump.Version=TRUE}.

    \item \code{\link{commit_changes}} for an example.
  }
}

\examples{
## See '?commit_changes'.
}
\keyword{utilities}
