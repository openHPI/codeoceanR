# Tests of codeoceanR package
# Started Nov 2020


# testing setup ----

owd <- "."
if(grepl("codeoceanR$", getwd()))
{
setwd("tests/testthat") # for sourcing (CTRL+SHIFT+S) instead of running devtools::test()
library(testthat)
}


rt_test_env <- list2env(rt_env(id="")) # to replace "Tnid: message" with "T: message")


# rt_test_object ----

test_that("rt_test_object warns correctly", {
source("objtests.R")
})


# rt_has_args ----

test_that("rt_has_args warns correctly", {
source("argtests.R")
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



# rt_run_script ----

script <- rt_run_script("scriptEx.R")


# rt_has_argument ----

results1 <- NA
rt_env(id=1); results1[1] <- rt_has_argument(rt_script_section(script, 1), "sep", "\t") # backslashes
rt_env(id=2); results1[2] <- rt_has_argument(rt_script_section(script, 2), "sep", "\n")
rt_env(id=3); results1[3] <- rt_has_argument(rt_script_section(script, 3), "sep", "\\")


results2 <- sapply(4:8, function(n){
rt_env(id=paste0(n,"T"))
code <- rt_script_section(script, n, collapse=";") # trailing semicolons
r1 <- rt_has_argument(code, "AAA", "1:4")
r2 <- rt_has_argument(code, "BBB", "8:1")
rt_env(id=paste0(n,"F"))
code <- rt_script_section(script, n, collapse=NULL)
r3 <- rt_has_argument(code, "AAA", "1:4")
r4 <- rt_has_argument(code, "BBB", "8:1")
out <- c(r1, r2, r3, r4)
names(out) <- c("AA_T","BB_T","AA_F","BB_F")
out
})

rt_env(id=9)
code <- rt_script_section(script, 9)  # quotation marks + logicals
results3 <- c(
  rt_has_argument(code, "con"),
  rt_has_argument(code, "warn"),
  rt_has_argument(code, "con", '"test_package.R"'),
  rt_has_argument(code, "warn", FALSE)
)
rt_env(id=10)
code <- rt_script_section(script, 10)   # line breaks
results4 <- c(
  rt_has_argument(code, "con"),
  rt_has_argument(code, "warn"),
  rt_has_argument(code, "con", '"test_package.R"'),
  rt_has_argument(code, "warn", FALSE)
)


test_that("rt_has_argument works correctly for data.frame task", {
  expect_true(all(results1))
  expect_true(all(results2))
  expect_true(all(results3))
  expect_true(all(results4))
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
