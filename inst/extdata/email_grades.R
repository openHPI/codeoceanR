# Prepare student grades for copypasting into individual emails
# Berry Boessenkool, Dec 2020

grades <- readxl::read_excel("C:/Dropbox/R/FREELANCING/HPI_2020_R/Quiz_grades.xlsx", skip=1, n_max=18)
grades <- grades[ , 1:17]
grades2 <- grades[ , substr(colnames(grades),1,1)=="Q"]


pbsapply(1:nrow(grades), function(i)
{
clipr::write_clip(unname(grades[i,1]))
beepr::beep(10)
Sys.sleep(2.5)
clipr::write_clip("Quiz scores")
beepr::beep(10)
Sys.sleep(2.5)
clipr::write_clip(paste0("Dear ", unname(grades[i,2]),
			 ",\n\nI'm writing to check if our grading sheet is correct.",
			 "\nCurrently, I have the following scores:\n",
			 paste(colnames(grades2), grades2[i,], sep=": ", collapse="\n"),
			 "\n\nPlease let me know if anything is wrong.",
			 "\n\nKind regards,\nBerry"))
beepr::beep(1)
Sys.sleep(6.5)
})

beepr::beep(3)
