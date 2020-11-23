# CodeOcean graded quizzes for R

By Berry Boessenkool, 2020, [berry-b@gmx.de](mailto:berry-b@gmx.de)

# students

#### install R package

*(once only)*

Run the following code (in R / Rstudio) to install or update the `codeoceanR` package:

```r
if(!requireNamespace("remotes", quietly=TRUE)) install.packages("remotes")
remotes::install_github("openHPI/codeoceanR", "main")
```


#### procedure

*(for each quiz)*

prepare quiz:

- through OpenHPI, go to the CodeOcean exercise
- download it to a good location on your PC
- close the CodeOcean browser tab
- run the following code (in R / Rstudio) for the zip file you just downloaded:
```r
codeoceanR::rt_create_task()  # see optional arguments below
```
- confirm to have closed the tab & select the quiz file (if not given as argument `zipfile`)
- open the Rproject if needed

take quiz:

- work on task 1
- save & source the script with CTRL + SHIFT + S, this will run `codeoceanR::rt_score()`
- work on task 2
- CTRL + SHIFT + S
- ...
- when done / time is up: run `codeoceanR::rt_score(final=TRUE)`, which opens the HPI task
- open the CodeOcean exercise again
- click `Score`
- click `Submit`


#### design

`rt_create_task()` can be run as-is or with any of the arguments:

- `zipfile`: defaults to interactive file choice, could be e.g. "C:/Dropbox/R/FProg20_R_quiz_1.zip" 
  _(On Mac OS, this must be any file within the auto-unzipped folder)_
- `exdir`: defaults to folder without zip extension, could be e.g "./newFolder_at_wd/quiz1"
- `isunzipped`: defaults to TRUE on Mac OS, where the file gets unzipped when downloading
- `deletezip`: defaults to delete the zip file if task creation was successful, could be FALSE

`rt_create_task()` should

- create a new folder with a `.Rproj` file
- open the Project in Rstudio
- with the `script_n.R` files already opened
- have everything prepared so `rt_score()` works out of the box.

Note: `codeoceanR::rt_score()` transfers your code to CodeOcean for scoring.

#### opened files

If you get the warning _"Rstudio User Settings file cannot be found. Files will not already be opened in .Rproj."._, open any .Rproj in Rstudio, then run:
```r
codeoceanR::rt_set_context_id()
```
For the next quiz, things should then work fine.



# teachers

It takes a bit of effort to initialize interactive R coding exercises in openHPI / CodeOcean, but it's worth it. 
The mostly automatic system enables you to put most day-to-day focus on developing good exercises instead of grading them.

Quizzes are accessed through openHPI but run and tested at CodeOcean, from which grades are passed back.
The tasks can also be solved in Rstudio, which is greatly recommended because it is the habitual _and_ future environment with interactivity, autocompletion, debugging (!), keyboard shortcuts and graphics.

Some participants had a hard time getting started in a time-pressed graded quiz setting.  
I suggest to first use the system at least twice(!) non-graded or non-time-pressured!  
You can't stress enough that participants need to run "Score" / `rt_score()` very often.  

## initial setup

The openHPI admins will have to create a dummy CodeOcean quiz in OpenHPI for you the first time.  
To be logged in, open CodeOcean through "Launch exercise tool".  
Then you can either go to <https://codeocean.openhpi.de/exercises/new>
or copy my [basic exercise](https://codeocean.openhpi.de/exercises/721). 
Potentially, admins must first duplicate it for you and set you as the author.

## Quiz acces point on openHPI

On OpenHPI, go to Course administration - Course structure and content, e.g. 
[url for fprog2020](https://open.hpi.de/courses/fprog-wi-2020/sections).  
In the desired section, click "Add item"

- **Title**: e.g. Quiz 3
- **Type**: External exercise tool
- **Exercise type**: Main
- **Maximal points**: e.g. 10
- **Submission deadline**: time before which the CodeOcean task must be **started**.  
The acutal deadline and grace period (20% score reduction) must be set in the CO task itself (if wanted).
- **Submission publishing date**: not relevant, can be left empty
- **Instructions**: e.g. Click the button below to launch the exercise.
- **LTI provider**: CodeOcean
- **Additional parameters**: locale=en&token=`8ffd83dc`&embed_options_disable_redirect_to_rfcs=true&embed_options_disable_redirect_to_feedback=true&embed_options_disable_interventions=true  
**replace `8ffd83dc` with the token from your CodeOcean exercise!**  
embedding options:  
  -  `redirect_to_rfcs`: disable finished users to be lead to open Request for Comments (RfCs).
  -  `redirect_to_feedback`: after "Submit", if there are no open RfCs, a feedback form is presented to 10% of users (min 20) without full score. 
  -  `interventions`: disable popups like "You seem to have trouble. Request comments here" while users are working.
- Remember to **copy the openHPI item link** to the CodeOcean description if you want to enable `rt_score(final=TRUE)`.


## CodeOcean exercises

All file types & roles etc can be seen in my basic exercise at <https://codeocean.openhpi.de/exercises/721>  
You can only have one 'main' file, the rest must be 'executable file (similar to main file)'.  
The structure of task and test scripts can also be seen at <https://github.com/openHPI/codeoceanR/tree/main/inst/extdata>  
A collection of exercises can be requested through [Berry](mailto:berry-b@gmx.de) or [Sebastian](mailto:sebastian.serth@hpi.de).  
There's an overview of all R quizzes at <https://codeocean.openhpi.de/exercises?&q[execution_environment_id_eq]=28>  

If the description includes an empty link (invisible on CO) to the openHPI excercise, like this `[](openHPI_item_url)`,
it will be used by `codeoceanR::score(final=TRUE)` for automated opening.

The Makefile run: could have `Rscript ${FILENAME}` as well, 
but then the run output does not contain the calls, making error sources harder to find.  
Hide your exercices if wanted (e.g. during development) by unchecking the 'Public' box.  
The admins would like you to use a prefix in the quiz name, e.g. Fprog20 for my course.

If you have data files to be read, make sure to check the box for Read-only.  
Otherwise users might change the file and your test script might fail,
e.g. if you have a file with meta data in the first lines and you want to 
emphasize to never touch raw data and use `read.table(...,skip=n)` instead.

## Testing

I suggest developing (and testing) the tasks within Rstudio.

Always test the entire quiz on CodeOcean as well, especially after expanding tests.  
Example: `rt_has_argument` didn't run online in the first version, 
since `parse(code)` needs to have `keep.source=TRUE`. 
The default option is TRUE only in an interactive R session!

Instead of checking code like in the [write.table task](https://github.com/openHPI/codeoceanR/blob/main/inst/extdata/script_2.R#L9-L13) and [test](https://github.com/openHPI/codeoceanR/blob/main/inst/extdata/tests.R#L61-L73), 
your tests can also execute the code and you test the resulting file.
This gives participants more freedom in how they structure the task.  

Attenton: if the user script contains line breaks and is read with `rt_select_script_section` with the default `collapse=TRUE`,
`rt_has_argument` leads to a test script failure.

In tests, **never compare with a result created by the user**, even if that is checked in a previous task.  
E.g., your test script could fail if you have the user do
```
df <- read.table("file.txt")
cmean <- mean(df[,-1])
```
and you test the df in one task and for the next
`rt_has_value(cmean,  mean(df[,-1]) )`  
There will be that one student who fails to specify header or sep or whatever,
meaning that `df` cannot be trusted to be correct.  
Besides not getting a point for the df task, the test script would fail and give no points at all.  
Rather read the dataset in the test script just before the cmean tests or hard-code the value.  
For the same reason, test 1 contains `iris <- datasets::iris`

## CO instead of Rstudio

If students use CodeOcean directly instead of downloading and working locally,
the following hints are especially important.

You can split up tests to several files (like with the scripts).  
Users will get several score boxes with green boxes for the successful files.
This is probably motivating, but also takes up a lot of screen space.  
Yes you could urge people to use PC pool computers with large screens, 
but having participants use their own (small) laptops has the advantage of 
easier logins on their side and keyboards they are used to.

You can technically have a single script file, but be aware that 
non-runnning files are common, frustrating and no points are given for tasks after an error.

Prefixing data files with `t*_filename.txt` is good to have the main `script_1.R` 
at the top of the page, also for statistics.


## Main URLs
replace 721 with the number of your exercise.  
https://codeocean.openhpi.de/exercises/721/edit  
https://codeocean.openhpi.de/exercises/721/implement  
https://codeocean.openhpi.de/exercises/721/statistics  
https://codeocean.openhpi.de/exercises/721/requests_for_comments  (not auto-updated)
https://codeocean.openhpi.de/exercises/721/study_group_dashboard/32  live view (RfCs auto-updated)


# developers

All functions in the package are prefixed with `rt_` (R test) for nice autocomplete selection.

This entire project profited from great info from Sebastian Serth, <sebastian.serth@hpi.de>.  
Experiences are based on the 2019 class "fundamentals of programming in digital health" with 31 participants.  
For the 2020 class, code was bundeled into an R package. Exercises can now be run locally in Rstudio. 
