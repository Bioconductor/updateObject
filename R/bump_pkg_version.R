### =========================================================================
### bump_pkg_version()
### -------------------------------------------------------------------------


.readLinesWithEOLs <- function(con)
{
    if (is.character(con)) {
        con <- file(con, "rb")
        on.exit(close(con))
    }
    chunks <- list()
    while (length(chunk <- readBin(con, raw(), n=5000L)) != 0L) {
        chunks <- c(chunks, list(chunk))
    }
    if (length(chunks) == 0L)
        return(character(0))
    rawtext <- rawToChar(unlist(chunks))
    rawlines <- strsplit(rawtext, split="\n", fixed=TRUE)[[1L]]
    paste0(rawlines, "\n")
}

.small_version_bump <- function(version)
{
    suffix <- sub("(.*)[^0-9]([0-9]*)$", "\\2", version)
    prefix <- substr(version, 1L, nchar(version) - nchar(suffix))
    paste0(prefix, as.character(as.integer(suffix) + 1L))
}

### FUN is the callback function that will be used to update the value
### associated with the supplied key.
.update_dcf_field <- function(lines, key, FUN)
{
    stopifnot(is.character(lines), isSingleString(key))
    FUN <- match.fun(FUN)
    key <- paste0(key, ":")
    idx <- match(key, substr(lines, 1L, nchar(key)))
    if (is.na(idx))
        return(lines)
    line <- lines[[idx]]
    nc <- nchar(sub("([ \r\n]*)$", "\\2", line))
    value <- substr(line, nchar(key) + 1L, nc)
    eol <- substr(line, nc + 1L, nchar(line))
    lines[[idx]] <- paste0(key, FUN(value), eol)
    lines
}

### We want to preserve the EOLs used in the DESCRIPTION file, whether
### they are LFs or CRLFs. This is why we use .readLinesWithEOLs() and
### writeLines(..., sep="") to read and write the file, respectively.
bump_pkg_version <- function(pkgpath=".", update.Date=FALSE)
{
    stopifnot(isSingleString(pkgpath))
    stopifnot(isTRUEorFALSE(update.Date))
    descpath <- file.path(pkgpath, "DESCRIPTION")
    lines <- .readLinesWithEOLs(descpath)
    lines <- .update_dcf_field(lines, "Version", .small_version_bump)
    if (update.Date) {
        lines <- .update_dcf_field(lines, "Date",
                     function(val) paste0(" ", as.character(Sys.Date())))
    }
    writeLines(lines, descpath, sep="")
}

