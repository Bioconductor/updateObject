.create_dummy_pkg <- function(desc, pkgpath)
{
    dir.create(pkgpath)
    descpath <- file.path(pkgpath, "DESCRIPTION")
    write.dcf(rbind(desc), descpath)
    descpath
}

test_that("bump_pkg_version", {
    ## These tests don't run properly on Windows at the moment.
    ## TODO: Investigate this.
    skip_on_os("windows")

    ## Create dummy R package:
    pkgname <- "Dummy"
    desc <- c(
        Package=pkgname,
        Title="Not a real package",
        Description="I'm not real u know.",
        Version="3.0.9",
        Date="1969-07-20"
    )
    pkgpath <- file.path(tempdir(), pkgname)
    descpath <- .create_dummy_pkg(desc, pkgpath)

    ## Bump its Version:
    bump_pkg_version(pkgpath)
    desc2 <- read.dcf(descpath)
    expect_identical(colnames(desc2), names(desc))
    desc2 <- as.character(desc2)
    expect_identical(desc2[[4L]], "3.0.10")
    expect_identical(desc2[-4L], unname(desc)[-4L])

    ## Bump its Version again and set Date to current date:
    bump_pkg_version(pkgpath, update.Date=TRUE)
    desc3 <- read.dcf(descpath)
    expect_identical(colnames(desc3), names(desc))
    desc3 <- as.character(desc3)
    expect_identical(desc3[[4L]], "3.0.11")
    expect_identical(desc3[[5L]], as.character(Sys.Date()))
    expect_identical(desc3[-(4:5)], unname(desc)[-(4:5)])

    unlink(pkgpath, recursive=TRUE)
})

