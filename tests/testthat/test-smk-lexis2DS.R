#
# Set up
#

# context("lexis2DS::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("lexis2DS::smk::datatext")
test_that("simple lexis2DS, datatext names a whole data.frame", {
    D <- data.frame(id = 1:6, start = rep(0, 6), end = rep(5, 6), status = rep(1, 6), extra = 101:106)

    res <- lexis2DS(
        datatext = "D", intervalWidth = "10", maxmaxtime = 10,
        idCol = "D$id", entryCol = "D$start", exitCol = "D$end", statusCol = "D$status", vartext = NULL
    )

    expect_equal(class(res), "list")
    # carried-forward columns must keep D's own column names, not be corrupted
    # or misaligned by the data.frame construction (see lexis2DS datatext/vartext handling)
    expect_true(all(c("id", "start", "end", "status", "extra") %in% names(res$expanded.table)))
    expect_equal(res$expanded.table$extra, 101:106)
    expect_equal(nrow(res$expanded.table), 6)
})

# context("lexis2DS::smk::vartext")
test_that("simple lexis2DS, vartext names individual $-qualified columns", {
    D <- data.frame(id = 1:6, start = rep(0, 6), end = rep(5, 6), status = rep(1, 6), extra = 101:106, extra2 = 201:206)

    res <- lexis2DS(
        datatext = NULL, intervalWidth = "10", maxmaxtime = 10,
        idCol = "D$id", entryCol = "D$start", exitCol = "D$end", statusCol = "D$status",
        vartext = "D$extra,D$extra2"
    )

    expect_equal(class(res), "list")
    expect_true(all(c("D.extra", "D.extra2") %in% names(res$expanded.table)))
    expect_equal(res$expanded.table$D.extra, 101:106)
    expect_equal(res$expanded.table$D.extra2, 201:206)
})

# context("lexis2DS::smk::no.carry.forward")
test_that("simple lexis2DS, no datatext or vartext", {
    D <- data.frame(id = 1:6, start = rep(0, 6), end = rep(5, 6), status = rep(1, 6))

    res <- lexis2DS(
        datatext = NULL, intervalWidth = "10", maxmaxtime = 10,
        idCol = "D$id", entryCol = "D$start", exitCol = "D$end", statusCol = "D$status", vartext = NULL
    )

    expect_equal(class(res), "list")
    expect_equal(nrow(res$expanded.table), 6)
})

test_that("lexis2DS throws error when object does not exist", {
    D <- data.frame(id = 1:6, start = rep(0, 6), end = rep(5, 6), status = rep(1, 6))

    expect_error(
        lexis2DS(
            datatext = NULL, intervalWidth = "10", maxmaxtime = 10,
            idCol = "D$id", entryCol = "D$start", exitCol = "nonexistent_object", statusCol = "D$status", vartext = NULL
        ),
        regexp = "does not exist"
    )
})

# context("lexis2DS::smk::no.entryCol")
test_that("simple lexis2DS, entryCol not given defaults entry times to 0", {
    D <- data.frame(id = 1:6, end = rep(5, 6), status = rep(1, 6))

    res <- lexis2DS(
        datatext = NULL, intervalWidth = "10", maxmaxtime = 10,
        idCol = "D$id", entryCol = NULL, exitCol = "D$end", statusCol = "D$status", vartext = NULL
    )

    expect_equal(class(res), "list")
    expect_equal(nrow(res$expanded.table), 6)
    expect_true(all(res$expanded.table$SURVTIME == 5))
})

#
# Done
#

# context("lexis2DS::smk::shutdown")

# context("lexis2DS::smk::done")
