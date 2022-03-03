### testthat breaks capture.output(..., type="message") (and also
### updateObject:::capture_message()). See:
###   https://github.com/r-lib/testthat/issues/1584
### So skipping these tests for now (until testthat gets fixed).
if (FALSE) {

test_that("updateSerializedObjects", {
    dirpath <- system.file("extdata", package="updateObject")

    ## --- Without a filter ---

    msg <- updateObject:::capture_message(
        code <- updateSerializedObjects(dirpath, recursive=TRUE, dry.run=TRUE)
    )
    expect_identical(code, 4L)
    expect_identical(length(msg), 5L)
    expect_identical(length(grep("\\bMATCH\\b", msg)), 0L)

    ## --- With a filter ---

    filter <- "\\bDataFrame\\b"
    msg <- updateObject:::capture_message(
        code <- updateSerializedObjects(dirpath, recursive=TRUE, filter=filter,
                                        dry.run=TRUE)
    )
    expect_identical(code, 4L)
    expect_identical(grepl("\\bMATCH\\b", msg),
                     c(TRUE, FALSE, TRUE, TRUE, TRUE))

    filter <- "\\bIRanges\\b"
    msg <- updateObject:::capture_message(
        code <- updateSerializedObjects(dirpath, recursive=TRUE, filter=filter,
                                        dry.run=TRUE)
    )
    expect_identical(code, 3L)
    expect_identical(grepl("\\bMATCH\\b", msg),
                     c(TRUE, TRUE, TRUE, FALSE, TRUE))

    filter <- "some non-sense blah blah that is very unlikely to occur"
    msg <- updateObject:::capture_message(
        code <- updateSerializedObjects(dirpath, recursive=TRUE, filter=filter,
                                        dry.run=TRUE)
    )
    expect_identical(code, 0L)
    expect_identical(length(msg), 5L)
    expect_identical(length(grep("\\bMATCH\\b", msg)), 0L)
})

}

