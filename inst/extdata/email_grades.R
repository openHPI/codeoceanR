# Prepare student grades for copypasting into individual emails
# Berry Boessenkool, Dec 2020

grades <- readxl::read_excel("C:/Dropbox/R/FREELANCING/HPI_2020_R/Quiz_grades.xlsx", skip=1, n_max=16)
grades <- grades[ , -c(3,20,21)]

grades$`avg Q` <- round(grades$`avg Q`, digits=2)
grades$total   <- round(grades$total  , digits=2)
grades$Grade  <- round0(grades$Grade  , digits=1, pre=1)
grades2 <- grades[ , grepl("^Q[0-9]",colnames(grades))]


pbsapply(1:nrow(grades), function(i)
{
clipr::write_clip(unname(grades[i,1]))
beepr::beep(10)
Sys.sleep(2.5)
clipr::write_clip("Fprog grade")
beepr::beep(10)
Sys.sleep(2.5)
clipr::write_clip(paste0("Dear ", unname(grades[i,2]),
			 ",\n\nThanks for participating in the course 'Fundamentals of programming in digital health'.",
			 "\nBefore we send the final grade, please check if the following scores are correct.",
			 "\nIf a score is higher than you expected, there's no need to contact me :).\n",
			 paste(colnames(grades2), grades2[i,], sep=": ", collapse="\n"),
			 "\nAverage quiz score (excluding two worst): ", grades$`avg Q`[i], " / 10",
			 "\nMidterm: ", grades$QM[i], " / 30",
			 "\nFinal: ", grades$QF[i], " / 13",
			 "\nTotal score (weights: Quizzes 40%, Midterm + Final each 30%): ", grades$total[i], " / 10",
			 "\nGrade: ", grades$Grade[i],
			 "\n\nPlease let me know if anything is wrong.",
			 "\n\nKind regards,\nBerry"))
beepr::beep(1)
Sys.sleep(if(i==nrow(grades)) 1.5 else 6.5)
})
beepr::beep(3)



# clipr::write_clip("Quiz scores")
# clipr::write_clip(paste0("Dear ", unname(grades[i,2]),
# 			 ",\n\nI'm writing to check if our grading sheet is correct.",
# 			 "\nCurrently, I have the following scores:\n",
# 			 paste(colnames(grades2), grades2[i,], sep=": ", collapse="\n"),
# 			 "\n\nPlease let me know if anything is wrong.",
# 			 "\n\nKind regards,\nBerry"))
