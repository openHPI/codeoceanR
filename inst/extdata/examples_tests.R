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

script1 <- rt_run_script("examples_1.R") # script 1 ----

rt_test_task(1, script1, my_first_object , 99)
# thorugh intnum, either numeric or integer class is fine,  5:15  and  seq(5,15,1)  are both correct:
rt_test_task(2, script1, my_second_object, 5:15)


script2 <- rt_run_script("examples_2.R") # script 2 ----
code3 <- rt_select_script_section(script2, 3)

rt_test_task(3, script=code3, object=NULL, value=NULL,
   rt_contains(code3, "write.table(", name="code section t3"),
   rt_has_argument(code3, "x", "iris"),
   rt_has_argument(code3, "file")
   , # Commas on separate lines enable sending a single line during test development
   rt_has_argument(code3, "sep", "\t")
   )


# To build on previous task (don't do this too much, students find it frustrating
# to lose 2 points if they cannot solve the first task):
rt_test_task(4, script2, NULL, NULL, solved=3, rt_has_argument(code3, "row.names", FALSE))


# To require several objects for a task but give only one message in total:
rt_test_task(5, script2, half_pi,   pi/2) &&
rt_test_task(5, script2, double_pi, pi*2)


# For multiple choice tasks, the options can be given in any order:
rt_test_task(6, script2, multiChoice, c(2,4), correct=TRUE)


# Functions can be checked with different inputs:
inputVec <- round(rnorm(30),1)
solution <- function(y) sqrt(replace(y, y<0, NaN)) # could also use suppressWarnings()
# if 'solution' is defined before rt_run_script, students could write silentRoot <- solution
rt_test_task(7, script2, silentRoot, solution, inputs='inputVec', export="inputVec",
             rt_gives_warning(silentRoot(-5), ""))
# with explicitely named argument and a vector at the same time:
rt_test_task(8, script2, silentRoot, solution, inputs=list("y=6", rt_vec(inputVec[1:10])))


# a <- function(x) {aa <- "stuff"; b(x)}
# b <- function(y) {
# 	print(ls(parent.frame()))            # parent.frame: aa, x      env in which the function was called  - dynamic scoping
# 	print(ls(parent.env(environment())) )# parent.env: a, b   enclosing env in which function was defined - lexical scoping
# 	message(aa) # # error: 'aa' not found         -      dynGet("aa") would work
# 	}
# a(7)


})
