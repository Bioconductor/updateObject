### =========================================================================
### capture_message()
### -------------------------------------------------------------------------
###
### Unlike capture.output(..., type="output"), nested calls to
### capture.output(..., type="message") don't work properly:
###
###   bad_capture_message <- function(...) capture.output(..., type="message")
###   foo1 <- function() {msg <- bad_capture_message(); message("ok")}
###   > msg <- bad_capture_message(foo1())
###   ok
###   > msg
###   character(0)
###
### This is because the current implementation of capture.output() makes
### the incorrect assumption that the diversions for messages stream
### are stacked like the diversions for normal output are.
### capture_message() addresses that:
###
###   foo2 <- function() {msg <- capture_message(); message("ok")}
###   > msg <- capture_message(foo2())
###   > msg
###   [1] "ok"
###   > msg <- bad_capture_message(foo2())
###   > msg
###   [1] "ok"

### NOT exported!
capture_message <- function(...)
{
    ## Unlike for normal output there is no stack diversions for the
    ## messages stream. See Details section in '?sink'. So in order to
    ## restore the status of the current diversion on exit, we won't be
    ## able to just unstack with 'sink(file=NULL)' like capture.output()
    ## does. Instead we'll need to explicitely set the diversion back to
    ## its old connection.
    old_con <- getConnection(sink.number(type="message"))

    ## Set new diversion.
    file <- tempfile()
    con <- file(file, "w")
    sink(con, type="message")

    on.exit({sink(old_con, type="message"); close(con)})

    for(i in seq_len(...length())) {
        out <- withVisible(...elt(i))
        if (out$visible)
            print(out$value)
    }

    ## We close the connection to 'file' before we read from 'file'.
    on.exit()
    sink(old_con, type="message")
    close(con)
    readLines(file)
}

