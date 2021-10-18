codeoceanR::rt_score() # Instructions in the course video / PDF section 1.4 Exercises

# Task 3 -----

# Write the iris data as txt file with tabstop as column separator.
# Please name all arguments explicitely, even the first ones.
# Write your code between the two markers.
# t3_start # do not change this line


# t3_end # do not change this line
# Again, run codeoceanR::rt_score()



# Task 4 ----
# Now also suppress the inclusion of rownames.
# Adapt your solution in the t3 section above.



# Task 5 ----
# Create two objects, according to their name.
half_pi <- 0
double_pi <- 0


# Task 6 ----
# Select the right option(s):
# 1. Not the answer
# 2. Yes, select this
# 3. Another false possibility
# 4. This one is correct, too.
multiChoice <- 0


# Task 7 ----
# Write a function that returns the root of it's input vector.
# For negative numbers, no warning message should be produced.
silentRoot <- function(x) 0
sqrt(-3:3) # warning
silentRoot(-3:3) # should not give a warning

silentRoot <- function(x) x

# When you are done, submit your score to openHPI with:
# codeoceanR::rt_submit()
