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

.OLD_update_object <- function(x)
{
    ## The QCStats class (e.g. "arrayMvout/inst/simpleaffy/afxsubQC.rda")
    ## was defined in simpleaffy which got removed in BioC 3.13.
    ## The YAQCStats class (e.g. "qcmetrics/inst/extdata/yqc.rda")
    ## was defined in yaqcaffy which got removed in BioC 3.14.
    ## The MutationFeatureData class (e.g. decompTumor2Sig/inst/extdata/Nik-Zainal_PMID_22608084-pmsignature-G.Rdata)
    ## is defined in pmsignature which is not part of CRAN or Bioconductor
    ## (GitHub-only package).
    ## The galgo.Obj class (e.g. "GSgalgoR/inst/extdata/results/final_1.rda")
    ## used to be defined in galgoR but this package no longer exists (has
    ## been renamed GSgalgoR).
    classdef_pkg <- attr(class(x), "package")
    if (!is.null(classdef_pkg) &&
        classdef_pkg %in% c("simpleaffy", "pmsignature", "galgoR", "yaqcaffy"))
        return(x)
    ## We're not touching eSet derivatives.
    if (is(x, "eSet"))
        return(x)
    ## QDNAseqCopyNumbers, Autotuner, BaalChIP, MethyLumiQC, MethyLumiSet,
    ## ccGeneList, CellMig, CeTF, trackedCells, CEMiTool, CogapsResult, and
    ## AnnotationHubMetadata objects don't support updateObject().
    ## updateObject() is broken on stanfit objects.
    ## updateObject() is currently broken on some MultiAssayExperiment
    ## objects (e.g. on "AffiXcan/inst/extdata/testing.tba.toydata.rds"),
    ## some BASiCS_Chain objects (e.g. "BASiCS/data/ChainRNA.rda"), and
    ## some xcmsSet objects (e.g. "CAMERA/data/mm14.rda").
    if (is(x, "QDNAseqCopyNumbers") ||
        is(x, "Autotuner") ||
        is(x, "BaalChIP") ||
        is(x, "MethyLumiQC") ||
        is(x, "MethyLumiSet") ||
        is(x, "ccGeneList") ||
        is(x, "CellMig") ||
        is(x, "CeTF") ||
        is(x, "trackedCells") ||
        is(x, "CEMiTool") ||
        is(x, "CogapsResult") ||
        is(x, "AnnotationHubMetadata") ||
        is(x, "stanfit") ||
        is(x, "MultiAssayExperiment") ||
        is(x, "BASiCS_Chain") ||
        is(x, "xcmsSet"))
        return(x)
    #ans <- try(suppressWarnings(suppressPackageStartupMessages(
    #             AnnotationHub:::.updateObject(x)
    #           )), silent=TRUE)
    #if (inherits(ans, "try-error"))
    #    ans <- x
    #ans
    AnnotationHub:::.updateObject(x)
}

.update_object <- function(x)
{
    ## The QCStats class (e.g. "arrayMvout/inst/simpleaffy/afxsubQC.rda")
    ## was defined in simpleaffy which got removed in BioC 3.13.
    ## The YAQCStats class (e.g. "qcmetrics/inst/extdata/yqc.rda")
    ## was defined in yaqcaffy which got removed in BioC 3.14.
    ## The MutationFeatureData class (e.g. decompTumor2Sig/inst/extdata/Nik-Zainal_PMID_22608084-pmsignature-G.Rdata)
    ## is defined in pmsignature which is not part of CRAN or Bioconductor
    ## (GitHub-only package).
    ## The galgo.Obj class (e.g. "GSgalgoR/inst/extdata/results/final_1.rda")
    ## used to be defined in galgoR but this package no longer exists (has
    ## been renamed GSgalgoR).
    classdef_pkg <- attr(class(x), "package")
    if (!is.null(classdef_pkg) &&
        classdef_pkg %in% c("simpleaffy", "pmsignature", "galgoR", "yaqcaffy"))
    {
        return(x)
    }
    AnnotationHub:::.updateObject(x)
}

.LOAD_FILE_FAILED     <- -2L
.UPDATE_OBJECT_FAILED <- -1L
.NOTHING_TO_UPDATE    <-  0L
.FILE_UPDATED         <-  1L

update_rds_file <- function(filepath, dry.run=FALSE)
{
    message("Processing '", filepath, "' ... ", appendLF=FALSE)
    x <- try(readRDS(filepath), silent=TRUE)
    if (inherits(x, "try-error")) {
        message("can't read RDS file --> ", .LOAD_FILE_FAILED)
        return(.LOAD_FILE_FAILED)
    }
    y <- try(.update_object(x), silent=TRUE)
    if (inherits(y, "try-error")) {
        message("updateObject() returned an error --> ",
                .UPDATE_OBJECT_FAILED)
        return(.UPDATE_OBJECT_FAILED)
    }
    if (digest(x) == digest(y)) {
        message("nothing to update --> ", .NOTHING_TO_UPDATE)
        return(.NOTHING_TO_UPDATE)
    }
    message(class(x)[[1L]], " updated ... ", appendLF=FALSE)
    if (dry.run) {
        message("won't save file (dry run)", appendLF=FALSE)
    } else {
        message("saving file ... ", appendLF=FALSE)
        saveRDS(y, file=filepath, compress=TRUE)
        message("OK", appendLF=FALSE)
    }
    message(" --> ", .FILE_UPDATED)
    .FILE_UPDATED
}

update_rda_file <- function(filepath, dry.run=FALSE)
{
    message("Processing '", filepath, "' ... ", appendLF=FALSE)
    envir <- new.env(parent=emptyenv())
    res <- try(suppressWarnings(load(filepath, envir=envir)), silent=TRUE)
    if (inherits(res, "try-error")) {
        message("can't load the file --> ", .LOAD_FILE_FAILED)
        return(.LOAD_FILE_FAILED)
    }
    updated_classes <- character(0)
    for (objname in names(envir)) {
        x <- get(objname, envir=envir, inherits=FALSE)
        y <- try(.update_object(x), silent=TRUE)
        if (inherits(y, "try-error")) {
            message("updateObject() returned an error --> ",
                    .UPDATE_OBJECT_FAILED)
            return(.UPDATE_OBJECT_FAILED)
        }
        if (digest(x) != digest(y))
            updated_classes <- c(updated_classes, class(x)[[1L]])
        assign(objname, y, envir=envir, inherits=FALSE)
    }
    if (length(updated_classes) == 0L) {
        message("nothing to update --> ", .NOTHING_TO_UPDATE)
        return(.NOTHING_TO_UPDATE)
    }
    message(paste0(updated_classes, "(s)", collapse=", "),
            " updated ... ", appendLF=FALSE)
    if (dry.run) {
        message("won't save file (dry run)", appendLF=FALSE)
    } else {
        message("saving file ... ", appendLF=FALSE)
        save(list=names(envir), file=filepath, envir=envir, compress="xz")
        message("OK", appendLF=FALSE)
    }
    message(" --> ", .FILE_UPDATED)
    .FILE_UPDATED
}

### Return nb of updated files or negative error code.
updatePackageObjects <- function(pkg_dirpath=".", dry.run=FALSE)
{
    codes <- integer(0)
    rds_paths <- collect_rds_files(pkg_dirpath)
    for (filepath in rds_paths) {
        code <- update_rds_file(filepath, dry.run=dry.run)
        codes <- c(codes, code)
    }
    rda_paths <- collect_rda_files(pkg_dirpath)
    for (filepath in rda_paths) {
        code <- update_rda_file(filepath, dry.run=dry.run)
        codes <- c(codes, code)
    }
    if (any(codes < 0L))
        return(min(codes))
    sum(codes)
}

### Return a named integer vector **parallel** to 'all_pkgs'.
updateAllPackageObjects <- function(all_pkgs, skipped_pkgs=NULL, dry.run=FALSE)
{
    vapply(all_pkgs,
        function(pkg) {
            if (!is.null(skipped_pkgs) && (pkg %in% skipped_pkgs)) {
                message("skipping package ", pkg)
                return(FALSE)
            }
            updatePackageObjects(pkg, dry.run=dry.run)
        },
        integer(1)
    )
}

