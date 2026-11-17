# rt_count and rt_remove_comments unit tests
# This file is sourced in test_package.R

# rt_count
expect_equal(rt_count(c("a |> b |> c", "d |> e"), "|>"), 3L)
expect_equal(rt_count("some text", "xyz"), 0L)         # not 1 as with length(gregexpr())
expect_equal(rt_count(character(0), "a"), 0L)
expect_equal(rt_count("10, 100, 1000", "10"), 3L)
expect_equal(rt_count("10, 100, 1000", "\\b10\\b", fixed=FALSE), 1L)
expect_true(is.na(rt_count(NA_character_, "a")))
expect_equal(rt_count("aaa", "aa"), 1L)  # overlaps are not counted
expect_error(rt_count("abc", NA_character_))
expect_error(rt_count("abc", c("a", "b")))
expect_error(rt_count("abc", ""))

# rt_remove_comments
code <- c("# header", "x <- 5 # five", 'y <- "C#" # sharp', "\tz <- x |> sqrt()  # tab", "")
expect_equal(rt_remove_comments(code),
             c("", "x <- 5", 'y <- "C#"', "\tz <- x |> sqrt()", ""))
expect_equal(rt_count(code, "|>"), 1L)
expect_equal(rt_count(rt_remove_comments(c("# a |> b", "1 |> sqrt()")), "|>"), 1L)
expect_equal(rt_remove_comments("\u00e4 <- '\u00e9' # \u00fcn\u00ef"), "\u00e4 <- '\u00e9'")
expect_equal(rt_remove_comments(character(0)), character(0))
expect_true(is.na(rt_remove_comments("x <- (")))
expect_equal(rt_remove_comments(c('x <- "a', '# not a comment', 'b" # real')),
             c('x <- "a', '# not a comment', 'b"'))
expect_equal(rt_remove_comments(c("x <- 1\ny <- 2 # c", "z")), c("x <- 1", "y <- 2", "z"))

# rt_contains uses rt_remove_comments (remcom)
quiet <- function(expr){res <- NULL; utils::capture.output(suppressMessages(res <- expr)); res}
expect_true(rt_contains(c("x <- 1 # note", "plot(a,", "     b)"), "plot(a,b)"))
expect_false(quiet(rt_contains(c("# secret", "x <- 1"), "secret")))
expect_true(rt_contains(c("econ |>", "  str()"), "|>"))        # code is matched as written
expect_false(quiet(rt_contains(c("x <- .5", "y <- 1"), "x<-0.5")))
expect_false(quiet(rt_contains(c("x <- (", "y"), "x")))          # syntax error
expect_true(rt_contains(c("a b", "c d"), "c d", remcom=FALSE))
