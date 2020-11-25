
myfun <- function(..., sep) return()

# t1_start
myfun(66, 77, sep="\t")
# t1_end

# t2_start
myfun(66, 77, sep="\n")
# t2_end

# t3_start
myfun(66, 77, sep="\\")
# t3_end



# t4_start
# comments + empty lines

simple_df <- data.frame(AAA=1:4, BBB=8:1)
simple_df

# t4_end

# t5_start # with more commands following the task
simple_df <- data.frame(AAA=1:4, BBB=8:1)
simple_df
# t5_end

# t6_start
simple_df <- data.frame(AAA=1:4, BBB=8:1)
# t6_end

# t7_start
simple_df <- data.frame(AAA=1:4, BBB=8:1);
simple_df
# t7_end

# t8_start
simple_df <- data.frame(AAA=1:4, BBB=8:1);
# t8_end

# t9_start
readLines(con="test_package.R", warn=FALSE)[1:3]
# t9_end

# t10_start # must be parsed with rt_select_script_section(collapse=FALSE)
readLines(con="test_package.R",
					warn=FALSE)[1:3]
# t10_end

# for(i in 11:14) cat(paste0("# t",i,"_start\n# t",i,"_end\n\n"))

