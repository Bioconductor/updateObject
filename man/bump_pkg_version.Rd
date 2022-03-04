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
    The path (as a single string) to the top-level directory
    of an R package source tree.
  }
  \item{update.Date}{
    \code{TRUE} or \code{FALSE}. If \code{TRUE} then the Date field (if
    present) gets updated to the current date.
  }
}

\value{
  An invisible \code{NULL}.
}

\seealso{
  \itemize{
    \item \code{\link{updatePackageObjects}} which uses
          \code{bump_pkg_version} internally when \code{bump.Version=TRUE}.

    \item \code{\link{git_commit}} for an example.
  }
}

\examples{
## Create dummy R package:
create_dummy_pkg <- function(desc, pkgpath) {
    dir.create(pkgpath)
    descpath <- file.path(pkgpath, "DESCRIPTION")
    write.dcf(rbind(desc), descpath)
    descpath
}

pkgname <- "Dummy"
desc <- c(
    Package=pkgname,
    Title="Not a real package",
    Description="I'm not real u know.",
    Version="3.0.9",
    Date="1969-07-20"
)
pkgpath <- file.path(tempdir(), pkgname)
descpath <- create_dummy_pkg(desc, pkgpath)

## Bump its Version:
bump_pkg_version(pkgpath)
cat(readLines(descpath), sep="\n")

## Bump its Version again and set Date to current date:
bump_pkg_version(pkgpath, update.Date=TRUE)
cat(readLines(descpath), sep="\n")

## Throw it away:
unlink(pkgpath, recursive=TRUE)
}
\keyword{utilities}
