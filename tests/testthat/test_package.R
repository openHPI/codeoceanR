# Tests of codeoceanR package
# Started Nov 2020


# testing setup ----

owd <- "."
if(grepl("codeoceanR$", getwd()))
{
setwd("tests/testthat") # for sourcing (CTRL+SHIFT+S) instead of running devtools::test()
library(testthat)
}


rt_test_env <- list2env(rt_env(lang="en"))


# rt_test_object ----

test_that("rt_test_object warns correctly", {
source("objtests.R")
})


# rt_has_args ----

test_that("rt_has_args warns correctly", {
source("argtests.R")
})

if(FALSE) test_that("rt_has_args escaping works correctly", { # myfun not found in non-interactive testing
script <- rt_run_script("scriptEx.R")
expect_true(is.character(script))
expect_true(rt_test_task(1,NULL,NULL,section=1,script=script,solcode='myfun(sep="\t")'))
expect_true(rt_test_task(1,NULL,NULL,section=2,script=script,solcode='myfun(sep="\n")'))
expect_true(rt_test_task(1,NULL,NULL,section=3,script=script,solcode='myfun(sep="\\")'))
})


# cleanup ----
setwd(owd); rm(owd)
rm(rt_test_env)
