# rt_count and rt_remove_comments unit tests
# This file is sourced in test_package.R

# rt_count
expect_equal(rt_count(c("a |> b |> c", "d |> e"), "|>"), 3L)
expect_equal(rt_count("some text", "xyz"), 0L)         # not 1 as with length(gregexpr())
expect_equal(rt_count(character(0), "a"), 0L)
expect_equal(rt_count("10, 100, 1000", "10"), 3L)
expect_equal(rt_count("10, 100, 1000", "\\b10\\b", fixed=FALSE), 1L)
expect_true(is.na(rt_count(NA_character_, "a")))

# rt_remove_comments
code <- c("# header", "x <- 5 # five", 'y <- "C#" # sharp', "\tz <- x |> sqrt()  # tab", "")
expect_equal(rt_remove_comments(code),
             c("", "x <- 5", 'y <- "C#"', "\tz <- x |> sqrt()", ""))
expect_equal(rt_count(code, "|>"), 1L)
expect_equal(rt_count(rt_remove_comments(c("# a |> b", "1 |> sqrt()")), "|>"), 1L)
expect_equal(rt_remove_comments("\u00e4 <- '\u00e9' # \u00fcn\u00ef"), "\u00e4 <- '\u00e9'")
expect_equal(rt_remove_comments(character(0)), character(0))
expect_true(is.na(rt_remove_comments("x <- (")))
