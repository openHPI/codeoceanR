# CodeOcean R Quiz
# Berry Boessenkool, berry-b@gmx.de, early 2020 + Oct 2020

# This script is run by `rt_score()` (in Rstudio) or by clicking "Score" (in CodeOcean).
# Running this script may never fail on CodeOcean, hence keep it wrapped in `try()`.
# The object 'task_id' must be defined here for the error messages through `rt_warn`.
# The tests must be connected with "&&", not "&" to avoid generating multiple messages.
# ("&&" stops evaluating on the first FALSE it encounters).
# The tests are set up so the rt_warn messages are increasingly specific.
# See also https://github.com/openHPI/codeoceanR#teachers

library(codeoceanR) # for all functions prefixed with rt_

ntests <-  3 # number of tests
npassed <- 0 # number of passed tests


trytests <- try({

task_id <- "_run"
script1 <- rt_run_script("script_1.R")
script2 <- rt_run_script("script_2.R")



task_id <- 1 # 1 ------

if(
rt_script_runs(script1)
&&
rt_exists(my_first_object)
&&
rt_has_class(my_first_object, "numeric")
&&
rt_has_length(my_first_object, 1)
&&
rt_has_value(my_first_object, 99, noise=FALSE)
) npassed <- npassed + 1




task_id <- 2 # 2 -----

if(
rt_script_runs(script1)
&&
rt_exists(my_second_object)
&&
rt_has_class(my_second_object, "integer")
&&
rt_has_length(my_second_object, 11)
&&
rt_has_value(my_second_object, 5:15, noise=FALSE)
) npassed <- npassed + 1



task_id <- 3 # 3 -----

sol <- rt_select_script_section(script2, 3)
if(
rt_script_runs(sol)
&&
rt_test(grepl("write.table", sol), "code does not contain the command 'write.table'.")
&&
rt_has_argument(sol, "x", "iris")
&&
rt_has_argument(sol, "file")
&&
rt_has_argument(sol, "sep", "\t")
&&
rt_has_argument(sol, "row.names", FALSE)
) npassed <- npassed + 1



}, silent=TRUE)
task_id <- "_post"
if(inherits(trytests, "try-error")) {
  rt_warn("The test script failed. Sorry for the 0 points right now. ",
  				"You'll have to revert the last thing(s) you did. ",
  				if(interactive())"Please click 'Request comments' and copypaste the logfile below.\n", trytests)
}
# Final output -----------------------------------------------------------------

# Keep the following at the end of the script, for CodeOcean scoring:
cat(ntests, "examples,", npassed, "passed\n")
