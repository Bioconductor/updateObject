### =========================================================================
### updatePackageObjects() and updateAllPackageObjects()
### -------------------------------------------------------------------------


### Return nb of updated files or negative error code.
updatePackageObjects <- function(pkgpath=".", filter=NULL,
                                 dry.run=FALSE, bump.Version=FALSE)
{
    if (!isSingleString(pkgpath))
        stop(wmsg("'pkgpath' must be a single string containig the path ",
                  "to the top-level directory of a package source tree"))
    if (!isTRUEorFALSE(bump.Version))
        stop(wmsg("'bump.Version' must be TRUE or FALSE"))

    ans <- updateSerializedObjects(pkgpath, filter=filter, dry.run=dry.run)
    if (bump.Version)
        bump_pkg_version(pkgpath, update.Date=TRUE)
    ans
}

### Return a named integer vector **parallel** to 'all_pkgpaths'.
updateAllPackageObjects <- function(all_pkgpaths, skipped_pkgs=NULL,
                                    filter=NULL,
                                    dry.run=FALSE, bump.Version=FALSE)
{
    vapply(all_pkgpaths,
        function(pkgpath) {
            if (!is.null(skipped_pkgs) && (pkgpath %in% skipped_pkgs)) {
                message("Skip package ", pkgpath, " ==> ", .SKIPPED_PACKAGE)
                return(.SKIPPED_PACKAGE)
            }
            updatePackageObjects(pkgpath, filter=filter,
                                 dry.run=dry.run, bump.Version=bump.Version)
        },
        integer(1)
    )
}

