# Tests of codeoceanR package
# Started Nov 2020


# run scriptEx ----

owd <- "."
if(grepl("codeoceanR$", getwd()))
{
setwd("tests/testthat") # for sourcing (CTRL+SHIFT+S) instead of running devtools::test()
library(testthat)
}



script <- rt_run_script("scriptEx.R")


# rt_has_argument ----

results1 <- NA
task_id <- 1; results1[1] <- rt_has_argument(rt_select_script_section(script, 1), "sep", "\t") # backslashes
task_id <- 2; results1[2] <- rt_has_argument(rt_select_script_section(script, 2), "sep", "\n")
task_id <- 3; results1[3] <- rt_has_argument(rt_select_script_section(script, 3), "sep", "\\")


results2 <- sapply(4:8, function(n){
task_id <- paste0(n,"T")
code <- rt_select_script_section(script, n, collapse=TRUE) # trailing semicolons
r1 <- rt_has_argument(code, "AAA", "1:4")
r2 <- rt_has_argument(code, "BBB", "8:1")
task_id <- paste0(n,"F")
code <- rt_select_script_section(script, n, collapse=FALSE)
r3 <- rt_has_argument(code, "AAA", "1:4")
r4 <- rt_has_argument(code, "BBB", "8:1")
out <- c(r1, r2, r3, r4)
names(out) <- c("AA_T","BB_T","AA_F","BB_F")
out
})


task_id <- 9
code <- rt_select_script_section(script, 9)  # quotation marks + logicals
results3 <- c(
  rt_has_argument(code, "con"),
  rt_has_argument(code, "warn"),
  rt_has_argument(code, "con", '"test_package.R"'),
  rt_has_argument(code, "warn", FALSE)
)
task_id <- 10
code <- rt_select_script_section(script, 10)   # line breaks
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
task_id <- ""
test_that("rt_due_warn works correctly",{
  due_test(rt_due_warn(Sys.time()+125, format="%F %T"), "T: Submission is due in 2.1 mins.")
  due_test(rt_due_warn(Sys.time()+ 30, format="%F %T"), "T: Submission is due in 0.5 mins. --> Please submit now!")
})
