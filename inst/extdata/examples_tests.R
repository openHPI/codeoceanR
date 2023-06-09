# CodeOcean R exercises
# Berry Boessenkool, berry-b@gmx.de, Jan 2020 - Oct 2021

# This file should be hidden to students in real exercises.

# This script is run by `rt_score()` (in Rstudio) or by clicking "Score" (in CodeOcean).
# Running this script may never fail on CodeOcean, hence the usage of `try()` in rt_test_exercise.
# The tests are set up so the rt_warn messages are increasingly specific.
# Only one message should ever be given for a task, hence the usage of `return()` in rt_test_task.
# See also https://github.com/openHPI/codeoceanR#teachers

# To run this script in Rstudio within zz_codeoceanR.Rproj, set WD:  setwd("inst/extdata/")

library(codeoceanR) # for all functions prefixed with rt_

rt_test_exercise({

# rt_env(lang="de") # to have German messages

script1 <- rt_run_script("examples_1.R") # script 1 ----

rt_test_task(1, my_first_object , 99)
# thorugh intnum, either numeric or integer class is fine,  5:15  and  seq(5,15,1)  are both correct:
rt_test_task(2, my_second_object, 5:15)


script2 <- rt_run_script("examples_2.R") # script 2 ----

rt_test_task(3, object=NULL, value=NULL, section=3, script=script2,
             solcode='write.table(iris, file=tempfile(), sep="\t")', alt=list(file="anyval"))

# To build on previous task (don't do this too much, students find it frustrating
# to lose 2 points if they cannot solve the first task):
rt_test_task(4, NULL, NULL, solved=3, section=3, script=script2,
             solcode="write.table(iris, row.names=FALSE)")


# To require several objects for a task but give only one message in total:
rt_test_task(5, half_pi,   pi/2) &&
rt_test_task(5, double_pi, pi*2)


# For multiple choice tasks, the options can be given in any order:
rt_test_task(6, multiChoice, c(2,4), correct=TRUE)


# Functions can be checked with different inputs:
inputVec <- round(rnorm(30),1)
solution <- function(y) sqrt(replace(y, y<0, NaN)) # could also use suppressWarnings()
rt_test_task(7, silentRoot, solution, inputs=inputVec,
             rt_gives("warning", silentRoot(-5), ""))

rt_test_task(8, silentRoot, solution, inputs=list("y=6", rt_vec(inputVec[1:10])))


# a <- function(x) {aa <- "stuff"; b(x)}
# b <- function(y) {
# 	print(ls(parent.frame()))            # parent.frame: aa, x      env in which the function was called  - dynamic scoping
# 	print(ls(parent.env(environment())) )# parent.env: a, b   enclosing env in which function was defined - lexical scoping
# 	message(aa) # # error: 'aa' not found         -      dynGet("aa") would work
# 	}
# a(7)
# a <- function(x) {aa <- "stuff"; b(x)}
# b <- function(y) {env <- parent.frame(); eval(substitute(aa), env)}
# a(7)
# b <- function(y) {eval.parent(substitute(aa))}
# a(7)
# b <- function(y) {eval.parent(aa)}
# a(7) # aa not found: when passed to eval.parent, it is evaluated first (unless substituted)


})
