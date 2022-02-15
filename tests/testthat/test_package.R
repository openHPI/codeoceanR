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



# rt_test_object ----

test_that("rt_test_object warns correctly", {

testm <- if(interactive()) testthat::expect_message else testthat::expect_output
sol <- function(x) x+1
fun <- function(x) x
x <- 555
testm(rt_test_task(7, fun, sol, inputs=c("1:5", "c(2,3,4)")),
      "T7: 'fun(1:5)[1]' should be '2', not '1'.", fixed=TRUE)

fun <- function(x) data.frame(x)
testm(rt_test_task(7, fun, sol, inputs=c("1:5", "4:2)")),
			"T7: 'fun(1:5)' should have class 'numeric', not 'data.frame'.", fixed=TRUE)

# ToDo: expand this a lot!
})



# rt_due_warn ----

due_test <- if(interactive()) testthat::expect_message else testthat::expect_output
rt_env(id="")
test_that("rt_due_warn works correctly",{
  due_test(rt_due_warn(Sys.time()+125, tz=Sys.timezone(), format="%F %T"),
           "T: Submission is due in 2.1 mins.")
  due_test(rt_due_warn(Sys.time()+ 30, tz=Sys.timezone(), format="%F %T"),
           "T: Submission is due in 0.5 mins. --> Please submit now!")
})



# cleanup ----
setwd(owd); rm(owd)
rm(rt_test_env)
