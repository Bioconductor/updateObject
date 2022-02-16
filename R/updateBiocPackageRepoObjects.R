### =========================================================================
### updateBiocPackageRepoObjects() and updateAllBiocPackageRepoObjects()
### -------------------------------------------------------------------------


### Requires git.
updateBiocPackageRepoObjects <- function(repopath=".", branch=NULL,
                                         filter=NULL,
                                         commit_msg=NULL, push=FALSE,
                                         remove.clone.on.success=FALSE,
                                         git=NULL, use.https=FALSE)
{
    if (is.null(commit_msg)) {
        commit_msg <- "Pass serialized S4 instances thru updateObject()"
    } else if (!isSingleString(commit_msg) || commit_msg == "") {
        stop(wmsg("'commit_msg' must be a single (non-empty) string"))
    }
    if (!isTRUEorFALSE(remove.clone.on.success))
        stop(wmsg("'remove.clone.on.success' must be TRUE or FALSE"))

    ## 1. Prepare the Git repo for work (clone or pull).
    is_new_clone <- prepare_git_repo_for_work(repopath, branch, git, use.https)

    ## 2. Update package objects.
    message()
    call <- c("updatePackageObjects(\"", repopath, "\"")
    if (!is.null(filter))
        call <- c(call, ", filter=\"", filter, "\"")
    call <- c(call, ", bump.Version=TRUE)")
    message("RUNNING '", gsub("\\\\", "\\\\\\\\", call), "'...")
    code <- updatePackageObjects(repopath, filter=filter, bump.Version=TRUE)
    message()
    if (code < 0L)
        stop("updatePackageObjects() encountered an error")

    if (code == 0L) {
        message("NOTHING TO UPDATE.")
    } else {
        ## 3. Commit and push.
        commit_changes(repopath, commit_msg, push, git)

        ## 4. Celebrate!
        message()
        msg <- c("UPDATE OBJECTS",
                 " >> UPDATE DESCRIPTION FILE",
                 " >> COMMIT")
        if (push)
            msg <- c(msg, " >> PUSH")
        msg <- c(msg, " SUCCESSFUL.")
        message(msg)
    }

    ## 5. Cleanup.
    if (remove.clone.on.success) {
        if (is_new_clone) {
            unlink(repopath, recursive=TRUE)
        } else {
            warning("'repopath' was an existing Git repo, didn't remove it")
        }
    }

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

