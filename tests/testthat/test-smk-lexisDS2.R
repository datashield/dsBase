#
# Set up
#

# context("lexisDS2::smk::setup")

set.standard.disclosure.settings()

#
# Tests
#

# context("lexisDS2::smk::datatext")
test_that("simple lexisDS2, datatext names a whole data.frame", {
    D <- data.frame(id = 1:6, start = rep(0, 6), end = rep(5, 6), status = rep(1, 6), extra = 101:106)

    res <- lexisDS2(
        datatext = "D", intervalWidth = "10", maxmaxtime = 10,
        idCol = "D$id", entryCol = "D$start", exitCol = "D$end", statusCol = "D$status", vartext = NULL
    )

    expect_equal(class(res), "list")
    # carried-forward columns must keep D's own column names, not be corrupted
    # or misaligned by the data.frame construction (see lexisDS2 datatext/vartext handling)
    expect_true(all(c("id", "start", "end", "status", "extra") %in% names(res$expanded.table)))
    expect_equal(res$expanded.table$extra, 101:106)
    expect_equal(nrow(res$expanded.table), 6)
})

# context("lexisDS2::smk::vartext")
test_that("simple lexisDS2, vartext names individual $-qualified columns", {
    D <- data.frame(id = 1:6, start = rep(0, 6), end = rep(5, 6), status = rep(1, 6), extra = 101:106, extra2 = 201:206)

    res <- lexisDS2(
        datatext = NULL, intervalWidth = "10", maxmaxtime = 10,
        idCol = "D$id", entryCol = "D$start", exitCol = "D$end", statusCol = "D$status",
        vartext = "D$extra,D$extra2"
    )

    expect_equal(class(res), "list")
    expect_true(all(c("D.extra", "D.extra2") %in% names(res$expanded.table)))
    expect_equal(res$expanded.table$D.extra, 101:106)
    expect_equal(res$expanded.table$D.extra2, 201:206)
})

# context("lexisDS2::smk::no.carry.forward")
test_that("simple lexisDS2, no datatext or vartext", {
    D <- data.frame(id = 1:6, start = rep(0, 6), end = rep(5, 6), status = rep(1, 6))

    res <- lexisDS2(
        datatext = NULL, intervalWidth = "10", maxmaxtime = 10,
        idCol = "D$id", entryCol = "D$start", exitCol = "D$end", statusCol = "D$status", vartext = NULL
    )

    expect_equal(class(res), "list")
    expect_equal(nrow(res$expanded.table), 6)
})

test_that("lexisDS2 throws error when object does not exist", {
    D <- data.frame(id = 1:6, start = rep(0, 6), end = rep(5, 6), status = rep(1, 6))

    expect_error(
        lexisDS2(
            datatext = NULL, intervalWidth = "10", maxmaxtime = 10,
            idCol = "D$id", entryCol = "D$start", exitCol = "nonexistent_object", statusCol = "D$status", vartext = NULL
        ),
        regexp = "does not exist"
    )
})

# context("lexisDS2::smk::no.entryCol")
test_that("simple lexisDS2, entryCol not given defaults entry times to 0", {
    D <- data.frame(id = 1:6, end = rep(5, 6), status = rep(1, 6))

    res <- lexisDS2(
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

# context("lexisDS2::smk::shutdown")

# context("lexisDS2::smk::done")
