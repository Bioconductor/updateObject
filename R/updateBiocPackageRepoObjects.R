.find_BBS_HOME <- function(BBS_HOME=NULL)
{
    if (!is.null(BBS_HOME)) {
        if (!is.character(BBS_HOME) ||
            length(BBS_HOME) != 1L ||
            is.na(BBS_HOME))
        {
            stop("'BBS_HOME' must be the path (supplied as a single string) ",
                 "to a\n  directory containing the BBS code.")
        }
        return(BBS_HOME)
    }

    BBS_HOME <- Sys.getenv("BBS_HOME")
    if (BBS_HOME != "")
        return(BBS_HOME)

    ## Try current directory.
    BBS_HOME <- "BBS"
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

    stop("No local copy of the BBS code (https://github.com/Bioconductor/BBS)",
         "\n  could be found on the system. Make sure there is one and use",
         "\n  the 'BBS_HOME' argument to specify the path to it (or define",
         "\n  environment variable BBS_HOME).")
}

.get_BBS_script <- function(script_name, BBS_HOME=NULL)
{
    BBS_HOME <- .find_BBS_HOME(BBS_HOME)
    script_path <- file.path(BBS_HOME, "utils", script_name)
    if (!file.exists(script_path))
        stop("'", script_path, "' not found.\n  Is 'BBS_HOME' set correctly?")
    script_path
}

### Requires Python, git (either in PATH or specified thru env var
### BBS_GIT_CMD), and BBS.
updateBiocPackageRepoObjects <- function(repo_path=".", branch=NULL,
                                         filter=NULL,
                                         commit_msg=NULL, push=FALSE,
                                         python=NULL, BBS_HOME=NULL)
{
    python <- find_python(python)
    clone_or_pull_repo_script <- .get_BBS_script("clone_or_pull_repo.py",
                                                 BBS_HOME)
    small_version_bumps_script <- .get_BBS_script("small_version_bumps.py",
                                                 BBS_HOME)
    if (is.null(commit_msg))
        commit_msg <- "Pass serialized S4 instances thru updateObject()"

    ## Clone or pull package repo.
    args <- clone_or_pull_repo_script
    if (!is.null(branch))
        args <- c(args, "--branch", branch)
    args <- c(args, repo_path)
    exit_status <- system2(python, args)
    message()
    if (exit_status != 0L)
        stop("'", clone_or_pull_repo_script, "' returned an error")

    ## Update package objects.
    code <- updatePackageObjects(repo_path, filter=filter)
    if (code < 0)
        stop("updatePackageObjects() encountered an error")

    if (code == 0) {
        message("NOTHING TO UPDATE.")
        return(FALSE)
    }

    ## Bump version, update Date, commit, and push.
    Sys.setenv("commit_msg", commit_msg)
    Sys.setenv("new_date", Sys.Date())
    args <- small_version_bumps_script
    if (!is.null(branch))
        args <- c(args, "--branch", branch)
    args <- c(args, repo_path)
    exit_status <- system2(python, args)
    message()
    if (exit_status != 0L)
        stop("'", small_version_bumps_script, "' returned an error")

    message("OPRATION SUCCESSFUL.")
    TRUE
}

