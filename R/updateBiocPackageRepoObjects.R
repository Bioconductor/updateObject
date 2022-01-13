### =========================================================================
### updateBiocPackageRepoObjects() and updateAllBiocPackageRepoObjects()
### -------------------------------------------------------------------------


### Requires git.
updateBiocPackageRepoObjects <- function(repopath=".", branch=NULL,
                                         filter=NULL,
                                         commit_msg=NULL, push=FALSE,
                                         git=NULL)
{
    if (is.null(commit_msg)) {
        commit_msg <- "Pass serialized S4 instances thru updateObject()"
    } else if (!isSingleString(commit_msg) || commit_msg == "") {
        stop(wmsg("'commit_msg' must be a single (non-empty) string"))
    }

    ## 1. Prepare the Git repo for work (clone or pull).
    prepare_git_repo_for_work(repopath, branch, git)

    ## 2. Update package objects.
    call <- c("updatePackageObjects(\"", repopath, "\"")
    if (!is.null(filter))
        call <- c(call, ", filter=\"", filter, "\"")
    call <- c(call, ", bump.Version=TRUE)")
    message("RUNNING '", call, "'...")
    code <- updatePackageObjects(repopath, filter=filter, bump.Version=TRUE)
    message()
    if (code < 0)
        stop("updatePackageObjects() encountered an error")

    if (code == 0) {
        message("NOTHING TO UPDATE.")
        return(invisible(code))
    }

    ## 3. Bump package version, set current Date, commit, and push.
    commit_changes(repopath, commit_msg, push, git)

    ## Celebrate!
    msg <- c("UPDATE OBJECTS",
             " >> UPDATE DESCRIPTION FILE",
             " >> COMMIT")
    if (push)
        msg <- c(msg, " >> PUSH")
    msg <- c(msg, " SUCCESSFUL.")
    message(msg)
    invisible(code)
}

### Return a named integer vector **parallel** to 'all_repopaths'.
updateAllBiocPackageRepoObjects <- function(all_repopaths=".",
                                            skipped_repos=NULL, ...)
{
    FUN <- function(i) {
        repopath <- all_repopaths[[i]]
        message("")
        message("=======================================",
                "=======================================")
        message("PROCESSING '", repopath, "' ",
                "(", i, "/", length(all_repopaths), ")")
        message("---------------------------------------",
                "---------------------------------------")
        message("")
        if (!is.null(skipped_repos) && (repopath %in% skipped_repos)) {
            message("Skip repo ", repopath, " ==> ", .SKIPPED_PACKAGE)
            return(.SKIPPED_PACKAGE)
        }
        code <- updateBiocPackageRepoObjects(repopath, ...)
        message("")
        message("DONE PROCESSING '", repopath, "'.")
        message("")
        code
    }
    invisible(vapply(seq_along(all_repopaths), FUN, integer(1)))
}

