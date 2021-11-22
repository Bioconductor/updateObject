find_python <- function(python=NULL)
{
    if (is.null(python)) {
        if (.Platform$OS.type == "windows") {
            ## TODO: Maybe use reticulate::py_versions_windows() (which is
            ## used by reticulate::py_discover_config()) to discover Python
            ## installations on a Windows system. See '?py_versions_windows'.
            python <- Sys.which("python")
        } else {
            python <- Sys.which("python3")
        }
        if (python == "")
             stop("\n  No Python executable can be found in PATH.\n  ",
                  wmsg("Please use the 'python' argument to specify ",
                       "the path to a valid Python executable."))
    } else {
        if (!isSingleString(python))
            stop(wmsg("'python' must be the path (supplied as a ",
                      "single string) to a Python executable"))
        if (!file.exists(python) || dir.exists(python))
            stop("\n  Invalid supplied path: \"", python, "\"\n  ",
                 wmsg("'python' must be the path (supplied as a ",
                      "single string) to a Python executable."))
    }
    python <- as.character(python)
    version <- try(system2(python, "--version", stdout=TRUE, stderr=TRUE),
                   silent=TRUE)
    if (inherits(version, "try-error") ||
        length(version) == 0L ||
        !grepl("Python", version[[1L]], ignore.case=TRUE))
    {
        stop("Invalid Python executable: \"", python, "\"\n  ",
             wmsg("Please use the 'python' argument to specify ",
                  "the path to a valid Python executable."))
    }
    if (!grepl("Python 3\\.", version[[1L]], ignore.case=TRUE))
        stop("'", python, "' is ", version, " but Python 3 is required")
    python
}

