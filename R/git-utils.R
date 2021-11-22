.find_BBS_HOME <- function(BBS_HOME=NULL)
{
    if (!is.null(BBS_HOME)) {
        if (!isSingleString(BBS_HOME))
            stop(wmsg("'BBS_HOME' must be the path (supplied as a single ",
                      "string) to a directory containing the BBS code."))
        return(BBS_HOME)
    }

    BBS_HOME <- Sys.getenv("BBS_HOME")
    if (BBS_HOME != "")
        return(BBS_HOME)

    ## Try current directory.
    BBS_HOME <- "./BBS"
    if (dir.exists(BBS_HOME))
        return(BBS_HOME)

    ## Try user home.
    BBS_HOME <- "~/BBS"
    if (dir.exists(BBS_HOME))
        return(BBS_HOME)

    ## Try ~/github/Bioconductor/BBS (my laptop).
    BBS_HOME <- "~/github/Bioconductor/BBS"
    if (dir.exists(BBS_HOME))
        return(BBS_HOME)

    stop(wmsg("No local copy of the BBS code ",
              "(https://github.com/Bioconductor/BBS)",
              "could be found on the system. ",
              "Make sure there is one and use the 'BBS_HOME' argument ",
              "to specify the path to it (or define environment ",
              "variable BBS_HOME)."))
}

.get_BBS_script <- function(script_name, BBS_HOME=NULL)
{
    BBS_HOME <- .find_BBS_HOME(BBS_HOME)
    script_path <- file.path(BBS_HOME, "utils", script_name)
    if (!file.exists(script_path))
        stop("'", script_path, "' not found.\n  Is 'BBS_HOME' set correctly?")
    script_path
}

.run_python_script <- function(python, script, args=character(0))
{
    message("RUNNING '", script, " ", paste(args, collapse=" "), "'...")
    exit_status <- system2(python, c(script, args))
    message()
    if (exit_status != 0L)
        stop("'", script, "' returned an error")
}

.run_BBS_script <- function(python, script_name, args=character(0),
                            BBS_HOME=NULL)
{
    script_path <- .get_BBS_script(script_name, BBS_HOME)
    .run_python_script(python, script_path, args)
}

### Prepare the Git repo for work by:
### - cloning it if 'repopath' does no exist;
### - switching it to the requested branch and pulling it if 'repopath'
###   already exists. But first we check for uncommitted changes and
###   raise an error if there are. We don't want to touch a local repo that
###   has uncommitted changes!
### Requires git (either in PATH or specified thru env var BBS_GIT_CMD),
### BBS, and Python.
### TODO: Drop dependency on BBS and Python by re-implementing what
### 'clone_or_pull_repo.py' does with direct calls to git (via system2()).
prepare_git_repo_for_work <- function(repopath=".", branch=NULL,
                                      BBS_HOME=NULL, python=NULL)
{
    if (!isSingleString(repopath))
        stop(wmsg("'repopath' must be a single string"))
    if (!is.null(branch) && !isSingleString(branch))
        stop(wmsg("'branch' must be NULL or a single string"))

    python <- find_python(python)

    args <- repopath
    if (!is.null(branch))
        args <- c("--branch", branch, args)
    .run_BBS_script(python, "clone_or_pull_repo.py", args, BBS_HOME)
}

### Bump package version, set current Date, commit, and push.
### Requires git (either in PATH or specified thru env var BBS_GIT_CMD),
### BBS, and Python.
### TODO: Drop dependency on BBS and Python by re-implementing what
### 'small_version_bumps.py' does with direct calls to git (via system2()).
bump_version_and_commit <- function(repopath=".", commit_msg=NULL, push=FALSE,
                                    BBS_HOME=NULL, python=NULL)
{
    if (!isSingleString(repopath))
        stop(wmsg("'repopath' must be a single string"))
    if (is.null(commit_msg)) {
        commit_msg <- "version bump"
    } else if (!isSingleString(commit_msg) || commit_msg == "") {
        stop(wmsg("'commit_msg' must be a single (non-empty) string"))
    }
    if (!isTRUEorFALSE(push))
        stop(wmsg("'push' must be TRUE or FALSE"))

    python <- find_python(python)

    Sys.setenv(commit_msg=commit_msg)
    Sys.setenv(new_date=as.character(Sys.Date()))
    args <- repopath
    if (push)
        args <- c(args, "--push")
    .run_BBS_script(python, "small_version_bumps.py", args, BBS_HOME)
}

