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

### Requires Python, git (either in PATH or specified thru env var
### BBS_GIT_CMD), and BBS.
updateBiocPackageRepoObjects <- function(repo_path=".", branch=NULL,
                                         filter=NULL,
                                         commit_msg=NULL, push=FALSE,
                                         python=NULL, BBS_HOME=NULL)
{
    python <- find_python(python)
    ## Only as an early test.
    .get_BBS_script("clone_or_pull_repo.py", BBS_HOME)
    .get_BBS_script("small_version_bumps.py", BBS_HOME)
    if (is.null(commit_msg))
        commit_msg <- "Pass serialized S4 instances thru updateObject()"

    ## 1. Clone or pull package repo.
    args <- repo_path
    if (!is.null(branch))
        args <- c("--branch", branch, args)
    .run_BBS_script(python, "clone_or_pull_repo.py", args, BBS_HOME)

    ## 2. Update package objects.
    call <- c("updatePackageObjects(\"", repo_path, "\"")
    if (!is.null(filter))
        call <- c(call, ", filter=\"", filter, "\"")
    call <- c(call, ")")
    message("RUNNING '", call, "'...")
    code <- updatePackageObjects(repo_path, filter=filter)
    message()
    if (code < 0)
        stop("updatePackageObjects() encountered an error")

    if (code == 0) {
        message("NOTHING TO UPDATE.")
        return(FALSE)
    }

    ## 3. Bump version, set current Date, commit, and push.
    Sys.setenv(commit_msg=commit_msg)
    Sys.setenv(new_date=as.character(Sys.Date()))
    args <- repo_path
    if (!is.null(branch))
        args <- c("--branch", branch, args)
    if (push)
        args <- c(args, "--push")
    .run_BBS_script(python, "small_version_bumps.py", args, BBS_HOME)

    ## Celebrate!
    msg <- "UPDATE OBJECTS --> UPDATE DESCRIPTION FILE --> COMMIT"
    if (push)
        msg <- c(msg, " --> PUSH")
    msg <- c(msg, " SUCCESSFUL.")
    message(msg)
    TRUE
}

