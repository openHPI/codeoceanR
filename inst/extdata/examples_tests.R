# CodeOcean R exercises
# Berry Boessenkool, berry-b@gmx.de, Jan 2020 - Oct 2021

# This file should be hidden to students in real exercises.

# This script is run by `rt_score()` (in Rstudio) or by clicking "Score" (in CodeOcean).
# Running this script may never fail on CodeOcean, hence the usage of `try()` in rt_test_exercise.
# The tests are set up so the rt_warn messages are increasingly specific.
# See also https://github.com/openHPI/codeoceanR#teachers

library(codeoceanR) # for all functions prefixed with rt_

rt_test_exercise({

script1 <- rt_run_script("examples_1.R") # script 1 ----

rt_test_task(1, script1, my_first_object , "numeric", length=1 , value=99)
rt_test_task(2, script1, my_second_object, "integer", length=11, value=5:15)


script2 <- rt_run_script("examples_2.R") # script 1 ----

sol <- rt_select_script_section(script2, 3)
rt_test_task(3, script=sol,
   rt_test(grepl("write.table", sol), "code does not contain the command 'write.table'.")
   ,
   rt_has_argument(sol, "x", "iris")
   ,
   rt_has_argument(sol, "file")
   ,
   rt_has_argument(sol, "sep", "\t")
   ,
   rt_has_argument(sol, "row.names", FALSE)
)
# Commas on separate lines enable sending a single line during test development

})
