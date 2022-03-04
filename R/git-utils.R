### =========================================================================
### Git-related utility functions
### -------------------------------------------------------------------------


.GIT_SERVER <- 'git.bioconductor.org'

.find_git <- function(git=NULL)
{
    if (is.null(git)) {
        git <- Sys.which("git")
        if (git == "")
             stop("\n  The git command cannot be found in your PATH.\n  ",
                  wmsg("Please use the 'git' argument to specify ",
                       "the path to the git command."))
    } else {
        EXPLAIN <- c("'git' must be the path (supplied as ",
                     "a single string) to the git command")
        if (!isSingleString(git))
            stop(wmsg(EXPLAIN))
        if (!file.exists(git) || dir.exists(git))
            stop("\n  Invalid supplied path: \"", git, "\"\n  ",
                 wmsg(EXPLAIN, "."))
    }
    git <- as.character(git)
    version <- try(suppressWarnings(
                     system2(git, "--version", stdout=TRUE, stderr=TRUE)
                   ), silent=TRUE)
    if (inherits(version, "try-error") ||
        length(version) == 0L ||
        !grepl("^git ", version[[1L]], ignore.case=TRUE))
    {
        stop("\n  Invalid git executable: \"", git, "\"\n  ",
             wmsg("Please use the 'git' argument to specify ",
                  "the path to a valid git executable."))
    }
    git
}

### A wrapper around system2():
### - that does its best to make the command output visible in the code
###   chunks of an Rmd vignette;
### - that turns the error returned by the supplied command into an
###   R error, except when 'capture.stderr=TRUE' in which case
###   the stderr produced by the command is captured and returned.
.system3 <- function(command, args=character(0), capture.stderr=FALSE)
{
    if (capture.stderr)
        return(suppressWarnings(system2(command, args, stderr=TRUE)))
    if (interactive()) {
        status <- suppressWarnings(system2(command, args))
    } else {
        ## In the context of an Rmd vignette code chunk, it seems that knitr
        ## is unable to capture the output of the command. A workaround is
        ## to capture this output and have R display it. Et voila !
        output <- suppressWarnings(system2(command, args,
                                           stdout=TRUE, stderr=TRUE))
        if (length(output) != 0L)
            cat(output, sep="\n")
        status <- attr(output, "status")
        if (is.null(status))
            status <- 0L
    }
    if (status != 0L) {
        cat("\n")
        stop("\n  Command:\n\n    ",
             command, " ", paste(args, collapse=" "),
             "\n\n  returned error code ", status, ".")
    }
}

.run_git_command <- function(git, repopath=".", args=character(0),
                             capture.stderr=FALSE)
{
    stopifnot(isSingleString(repopath))
    oldwd <- getwd()
    setwd(repopath)
    on.exit(setwd(oldwd))
    .system3(git, args, capture.stderr=capture.stderr)
}

.is_git_repo <- function(git, repopath=".")
{
    if (!dir.exists(repopath))
        return(FALSE)
    args <- c("status", "--porcelain")
    stderr <- .run_git_command(git, repopath, args, capture.stderr=TRUE)
    status <- attr(stderr, "status")
    is.null(status)
}

### A more cautious version of .is_git_repo() that also makes sure that
### the repo doesn't contain uncommitted changes.
.check_git_repo_is_workable <- function(git, repopath=".")
{
    if (!dir.exists(repopath))
        stop(wmsg("the supplied path is not a directory"))
    args <- c("status", "--porcelain")
    stderr <- .run_git_command(git, repopath, args, capture.stderr=TRUE)
    status <- attr(stderr, "status")
    if (!is.null(status))
        stop(wmsg("Not a Git repo: ", repopath))
    if (length(stderr) != 0L && substr(stderr[[1L]], 1L, 1L) != "?")
        stop(wmsg("Git repo has uncommitted changes"))
}

.git_pull <- function(git, repopath=".", branch=NULL)
{
    if (!is.null(branch))
        .run_git_command(git, repopath, c("checkout", branch))
    .run_git_command(git, repopath, "pull")
}

.infer_pkgname_from_path <- function(repopath)
{
    pkgname <- basename(repopath)
    nc <- nchar(pkgname)
    last_char <- substr(pkgname, nc, nc)
    if (last_char %in% c("", ".", " "))
        stop(wmsg("cannot infer package name from supplied repo path"))
    pkgname
}

.git_clone_bioc_package <- function(git, pkgname, branch=NULL, repopath=NULL,
                                    use.https=FALSE)
{
    args <- "clone"
    if (!is.null(branch))
        args <- c(args, paste("--branch", branch))
    if (use.https) {
        base_url <- sprintf("https://%s/", .GIT_SERVER)
    } else {
        base_url <- sprintf("git@%s:", .GIT_SERVER)
    }
    args <- c(args, sprintf("%spackages/%s", base_url, pkgname))
    if (!is.null(repopath))
        args <- c(args, repopath)
    .system3(git, args)
}

### Prepare the Git repo for work by:
### - cloning it if 'repopath' does no exist;
### - switching it to the requested branch and pulling it if 'repopath'
###   already exists. But first we check for uncommitted changes and
###   raise an error if there are. We don't want to touch a local repo that
###   has uncommitted changes!
prepare_git_repo_for_work <- function(repopath=".", branch=NULL,
                                      git=NULL, use.https=FALSE)
{
    if (!isSingleString(repopath))
        stop(wmsg("'repopath' must be a single string"))
    if (!is.null(branch) && !isSingleString(branch))
        stop(wmsg("'branch' must be NULL or a single string"))
    if (!isTRUEorFALSE(use.https))
        stop(wmsg("'use.https' must be TRUE or FALSE"))
    git <- .find_git(git)

    if (file.exists(repopath)) {
        ## Pull.
        .check_git_repo_is_workable(git, repopath)
        .git_pull(git, repopath, branch)
        return(FALSE)
    }
    ## Clone.
    pkgname <- .infer_pkgname_from_path(repopath)
    .git_clone_bioc_package(git, pkgname, branch, repopath, use.https)
    return(TRUE)
}

set_git_user_name <- function(user_name)
{
    if (!isSingleString(user_name) || user_name == "")
        stop(wmsg("'user_name' must be a single (non-empty) string"))
    old_author_name <- Sys.getenv("GIT_AUTHOR_NAME")
    old_committer_name <- Sys.getenv("GIT_COMMITTER_NAME")
    Sys.setenv(GIT_AUTHOR_NAME=user_name, GIT_COMMITTER_NAME=user_name)
    c(old_author_name=old_author_name,
      old_committer_name=old_committer_name)
}

unset_git_user_name <- function(old_names)
{
    old_author_name <- old_names[["old_author_name"]]
    old_committer_name <- old_names[["old_committer_name"]]
    if (old_author_name == "") {
        Sys.unsetenv("GIT_AUTHOR_NAME")
    } else {
        Sys.setenv(GIT_AUTHOR_NAME=old_author_name)
    }
    if (old_committer_name == "") {
        Sys.unsetenv("GIT_COMMITTER_NAME")
    } else {
        Sys.setenv(GIT_COMMITTER_NAME=old_committer_name)
    }
}

set_git_user_email <- function(user_email)
{
    if (!isSingleString(user_email) || user_email == "")
        stop(wmsg("'user_email' must be a single (non-empty) string"))
    old_author_email <- Sys.getenv("GIT_AUTHOR_EMAIL")
    old_committer_email <- Sys.getenv("GIT_COMMITTER_EMAIL")
    Sys.setenv(GIT_AUTHOR_EMAIL=user_email, GIT_COMMITTER_EMAIL=user_email)
    c(old_author_email=old_author_email,
      old_committer_email=old_committer_email)
}

unset_git_user_email <- function(old_emails)
{
    old_author_email <- old_emails[["old_author_email"]]
    old_committer_email <- old_emails[["old_committer_email"]]
    if (old_author_email == "") {
        Sys.unsetenv("GIT_AUTHOR_EMAIL")
    } else {
        Sys.setenv(GIT_AUTHOR_EMAIL=old_author_email)
    }
    if (old_committer_email == "") {
        Sys.unsetenv("GIT_COMMITTER_EMAIL")
    } else {
        Sys.setenv(GIT_COMMITTER_EMAIL=old_committer_email)
    }
}

.git_commit_all_changes <- function(git, repopath=".", commit_msg)
{
    ## If 'repopath' is not a Git repo, 'git --no-pager diff' will spit
    ## out many screens of ugly output!
    if(!.is_git_repo(git, repopath))
        stop(wmsg("Not a Git repo: ", repopath))
    .run_git_command(git, repopath, c("--no-pager", "diff"))
    cat("\n")
    commit_msg <- gsub("\"", "\\\\\"", commit_msg)
    args <- c("commit", "-a", sprintf("-m \"%s\"", commit_msg))
    .run_git_command(git, repopath, args)
}

.git_push <- function(git, repopath=".")
{
    .run_git_command(git, repopath, "push")
}

git_commit <- function(repopath=".", commit_msg, push=FALSE,
                       git=NULL, user_name=NULL, user_email=NULL)
{
    if (!isSingleString(repopath))
        stop(wmsg("'repopath' must be a single string"))
    if (!isSingleString(commit_msg) || commit_msg == "")
        stop(wmsg("'commit_msg' must be a single (non-empty) string"))
    if (!is.null(user_name)) {
        old_names <- set_git_user_name(user_name)
        on.exit(unset_git_user_name(old_names))
    }
    if (!is.null(user_email)) {
        old_emails <- set_git_user_email(user_email)
        on.exit(unset_git_user_email(old_emails), add=TRUE)
    }
    if (!isTRUEorFALSE(push))
        stop(wmsg("'push' must be TRUE or FALSE"))
    git <- .find_git(git)

    .git_commit_all_changes(git, repopath, commit_msg)
    if (push) {
        cat("\n")
        .git_push(git, repopath)
    }
}

