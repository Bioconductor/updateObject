### =========================================================================
### updatePackageObjects() and updateAllPackageObjects()
### -------------------------------------------------------------------------


### Return nb of updated files or negative error code.
updatePackageObjects <- function(pkgpath=".", filter=NULL,
                                 dry.run=FALSE, bump.Version=FALSE)
{
    get_descpath(pkgpath)  # just to get an early check of 'pkgpath'
    if (!isTRUEorFALSE(bump.Version))
        stop(wmsg("'bump.Version' must be TRUE or FALSE"))

    code <- updateSerializedObjects(pkgpath, filter=filter, dry.run=dry.run)
    if (bump.Version && code > 0L) {
        ## bump_pkg_version() calls get_descpath() again so if the above
        ## call to get_descpath() emitted a warning then bump_pkg_version()
        ## will emit that same warning again.
        suppressWarnings(bump_pkg_version(pkgpath, update.Date=TRUE))
    }
    code
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

