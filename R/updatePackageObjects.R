collect_files <- function(dirpath, fileexts)
{
    pattern <- paste0("\\.(", paste(fileexts, collapse="|"), ")$")
    dir(dirpath, pattern=pattern, ignore.case=TRUE,
        recursive=TRUE, full.names=TRUE)
}

collect_rds_files <- function(dirpath=".")
    collect_files(dirpath, "rds")

collect_rda_files <- function(dirpath=".")
    collect_files(dirpath, c("rda", "RData"))

### If 'x' is an S4 object, doing 'inherits(x, "try-error")' will trigger
### the loading of 'attr(class(x), "package")' and we don't want that.
.is_try_error <- function(x) { !isS4(x) && inherits(x, "try-error") }

### Known invalid packages found in 'attr(class(x), "package")' as of
### Nov 17, 2021.
.KNOWN_INVALID_CLASSDEF_PKGS <- c(
  ## For some serialized S4 instances hub 'attr(class(x), "package")' is
  ## set to ".GlobalEnv"! This is the case for example for CellMapperList
  ## instances EH170 to EH175 in ExperimentHub. Not sure how that's allowed
  ## but let's just deal with it.
    ".GlobalEnv",
  ## SimResults class (e.g.
  ## "iCOBRA/inst/extdata/cobradata_example_simres.Rdata") is defined in the
  ## benchmarkR package which is not part of CRAN or Bioconductor (GitHub-only
  ## package).
    "benchmarkR",
  ## The galgo.Obj class (e.g. "GSgalgoR/inst/extdata/results/final_1.rda")
  ## used to be defined in galgoR but this package no longer exists (has
  ## been renamed GSgalgoR).
    "galgoR",
  ## The MutationFeatureData class (e.g.
  ## decompTumor2Sig/inst/extdata/Nik-Zainal_PMID_22608084-pmsignature-G.Rdata)
  ## is defined in the pmsignature package which is not part of CRAN or
  ## Bioconductor (GitHub-only package).
    "pmsignature",
  ## The QCStats class (e.g. "arrayMvout/inst/simpleaffy/afxsubQC.rda")
  ## was defined in simpleaffy which got removed in BioC 3.13.
    "simpleaffy",
  ## The YAQCStats class (e.g. "qcmetrics/inst/extdata/yqc.rda")
  ## was defined in yaqcaffy which got removed in BioC 3.14.
    "yaqcaffy"
)

### Make sure that the package where the class of 'x' is defined is loaded
### before calling 'updateObject()' on 'x'.
.load_classdef_pkg <- function(x_class)
{
    classdef_pkg <- attr(x_class, "package")
    if (is.null(classdef_pkg) || classdef_pkg %in% .KNOWN_INVALID_CLASSDEF_PKGS)
        return()
    suppressMessages(suppressWarnings(suppressPackageStartupMessages(
        loadNamespace(classdef_pkg)
    )))
}

.update_object <- function(x, filter=NULL)
{
    message("updateObject(", class(x)[[1L]], ", check=FALSE).. ",
            appendLF=FALSE)
    if (is.null(filter)) {
        suppressMessages(
            ans <- suppressWarnings(updateObject(x, check=FALSE))
        )
    } else {
        output <- capture.output(
            ans <- suppressWarnings(updateObject(x, check=FALSE, verbose=TRUE)),
            type="message"
        )
        match_filter <- any(grepl(filter, output))
        if (match_filter) {
            message("MATCH, ", appendLF=FALSE)
        } else {
            message("no match, ", appendLF=FALSE)
            ans <- x
        }
    }
    ans
}

.SKIPPED_PACKAGE      <- -3L
.LOAD_FILE_FAILED     <- -2L
.UPDATE_OBJECT_FAILED <- -1L
.NOTHING_TO_UPDATE    <-  0L
.FILE_UPDATED         <-  1L

update_rds_file <- function(filepath, filter=NULL, dry.run=FALSE)
{
    message("File ", filepath, ": readRDS().. ", appendLF=FALSE)
    x <- try(readRDS(filepath), silent=TRUE)
    if (.is_try_error(x)) {
        message("failed! ==> ", .LOAD_FILE_FAILED)
        return(.LOAD_FILE_FAILED)
    }
    message("ok; ", appendLF=FALSE)
    .load_classdef_pkg(class(x))
    y <- try(.update_object(x, filter=filter), silent=TRUE)
    if (.is_try_error(y)) {
        message("returned an error ==> ",
                .UPDATE_OBJECT_FAILED)
        return(.UPDATE_OBJECT_FAILED)
    }
    if (digest(x) == digest(y)) {
        message("no-op ==> ", .NOTHING_TO_UPDATE)
        return(.NOTHING_TO_UPDATE)
    }
    message("object updated; ", appendLF=FALSE)
    if (dry.run) {
        message("won't save file (dry run) ", appendLF=FALSE)
    } else {
        message("saving file.. ", appendLF=FALSE)
        saveRDS(y, file=filepath, compress=TRUE)
        message("OK ", appendLF=FALSE)
    }
    message("==> ", .FILE_UPDATED)
    .FILE_UPDATED
}

update_rda_file <- function(filepath, filter=NULL, dry.run=FALSE)
{
    message("File ", filepath, ": load().. ", appendLF=FALSE)
    envir <- new.env(parent=emptyenv())
    objnames <- try(
        suppressMessages(suppressWarnings(load(filepath, envir=envir))),
        silent=TRUE
    )
    if (.is_try_error(objnames)) {
        message("failed! ==> ", .LOAD_FILE_FAILED)
        return(.LOAD_FILE_FAILED)
    }
    message("ok [", length(objnames)," object(s)]; ", appendLF=FALSE)
    nothing_to_update <- TRUE
    for (objname in objnames) {
        x <- get(objname, envir=envir, inherits=FALSE)
        ## Some datasets could have been saved with
        ##   save(..., eval.promises=FALSE)
        ## so the first time we'll try to access 'x', the promise will
        ## evaluate and this could raise warnings or print messages.
        ## This is actually the case for the 'studiesTable' dataset in
        ## the cBioPortalData package.
        ## To prevent surprises further down we force evaluation now.
        suppressMessages(suppressWarnings(force(x)))
        .load_classdef_pkg(class(x))
        y <- try(.update_object(x, filter=filter), silent=TRUE)
        if (.is_try_error(y)) {
            message("returned an error ==> ",
                    .UPDATE_OBJECT_FAILED)
            return(.UPDATE_OBJECT_FAILED)
        }
        if (digest(x) == digest(y)) {
            message("no-op; ", appendLF=FALSE)
        } else {
            message("object updated; ", appendLF=FALSE)
            nothing_to_update <- FALSE
        }
        assign(objname, y, envir=envir, inherits=FALSE)
    }
    if (nothing_to_update) {
        message("nothing to update ==> ", .NOTHING_TO_UPDATE)
        return(.NOTHING_TO_UPDATE)
    }
    if (dry.run) {
        message("won't save file (dry run) ", appendLF=FALSE)
    } else {
        message("saving file.. ", appendLF=FALSE)
        save(list=names(envir), file=filepath, envir=envir, compress="xz")
        message("OK ", appendLF=FALSE)
    }
    message("==> ", .FILE_UPDATED)
    .FILE_UPDATED
}

### Return nb of updated files or negative error code.
updatePackageObjects <- function(pkg_dirpath=".", filter=NULL, dry.run=FALSE)
{
    codes <- integer(0)
    rds_paths <- collect_rds_files(pkg_dirpath)
    for (filepath in rds_paths) {
        code <- update_rds_file(filepath, filter=filter, dry.run=dry.run)
        codes <- c(codes, code)
    }
    rda_paths <- collect_rda_files(pkg_dirpath)
    for (filepath in rda_paths) {
        code <- update_rda_file(filepath, filter=filter, dry.run=dry.run)
        codes <- c(codes, code)
    }
    if (any(codes < 0L))
        return(min(codes))
    sum(codes)
}

### Return a named integer vector **parallel** to 'all_pkgs'.
updateAllPackageObjects <- function(all_pkgs, skipped_pkgs=NULL,
                                    filter=NULL, dry.run=FALSE)
{
    vapply(all_pkgs,
        function(pkg) {
            if (!is.null(skipped_pkgs) && (pkg %in% skipped_pkgs)) {
                message("Skip package ", pkg, " ==> ", .SKIPPED_PACKAGE)
                return(.SKIPPED_PACKAGE)
            }
            updatePackageObjects(pkg, filter=filter, dry.run=dry.run)
        },
        integer(1)
    )
}

