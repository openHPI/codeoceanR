# CodeOcean graded exercises for R

By Berry Boessenkool, 2020-2021, [berry-b@gmx.de](mailto:berry-b@gmx.de)

# students

#### install R package

*(once only)*

Run the following code (in R / Rstudio) to install or update the `codeoceanR` package:

```r
if(!requireNamespace("remotes", quietly=TRUE)) install.packages("remotes")
remotes::install_github("openHPI/codeoceanR", "main")
```


#### procedure

*(for each exercise)*

prepare exercise:

- through OpenHPI, go to the CodeOcean exercise
- download it to a good location on your PC
- close the CodeOcean browser tab
- run (in R / Rstudio) `codeoceanR::rt_create_task()`
- confirm to have closed the tab & select the exercise file (if not given as argument `zipfile`, see below)

take exercise:

- work on task 1
- save & source the script with `CTRL + SHIFT + S`, this will run `codeoceanR::rt_score()`
- work on task 2
- `CTRL + SHIFT + S`
- ...
- when done / time is up: run `codeoceanR::rt_submit()`



#### design

`rt_create_task()` can be run as-is or with the arguments:

- `zipfile`: defaults to interactive file choice, could be e.g. "C:/Dropbox/R/FProg20_Rex_1.zip"  
  _If the exercise is unzipped (the default on Mac OS Safari), any file within the folder_
- `deletezip`: defaults to delete the zip file if task creation was successful, could be FALSE

`rt_create_task()` should

- create a new folder with a `.Rproj` file
- open the Project in Rstudio
- with the `script_n.R` files already opened
- have everything prepared so `rt_score()` works out of the box.

Note: `codeoceanR::rt_score()` transfers your code to CodeOcean for scoring.  
`codeoceanR::rt_submit()` at the end submits the score to openHPI.


# teachers

It takes a bit of effort to initialize interactive R coding exercises in openHPI / CodeOcean, but it's worth it. 
The mostly automatic system enables you to put most day-to-day focus on developing good exercises instead of grading them.

Exercises are accessed through openHPI but run and tested at CodeOcean, from which grades are passed back.
The tasks can also be solved in Rstudio, which is greatly recommended because it is the habitual _and_ future environment with interactivity, autocompletion, debugging (!), keyboard shortcuts and graphics.

Some participants had a hard time getting started in a time-pressed graded exercise setting.  
I suggest to first use the system at least twice(!) non-graded or non-time-pressured!  
You can't stress enough that participants need to run "Score" / `rt_score()` very often.  

## initial setup

The openHPI admins will have to create a dummy CodeOcean exercise in OpenHPI for you the first time.  
To be logged in, open CodeOcean through "Launch exercise tool".  
Then you can either go to <https://codeocean.openhpi.de/exercises/new>
or copy my [basic exercise](https://codeocean.openhpi.de/exercises/721). 
Potentially, admins must first duplicate it for you and set you as the author.

## exercise acces point on openHPI

On OpenHPI, go to Course administration - Course structure and content, e.g. 
[url for fprog2020](https://open.hpi.de/courses/fprog-wi-2020/sections).  
In the desired section, click "Add item"

- **Title**: e.g. exercise 3
- **Type**: External exercise tool
- **Exercise type**: Main
- **Maximal points**: e.g. 10
- **Submission deadline**: time before which the CodeOcean task must be **started**.  
The actual deadline and grace period (20% score reduction) must be set in the CO task itself (if wanted).
- **Submission publishing date**: not relevant, can be left empty
- **Instructions**: e.g. Click the button below to launch the exercise.
- **LTI provider**: CodeOcean
- **Additional parameters**: locale=en&token=`xxxxxxxx`&embed_options_disable_redirect_to_rfcs=true&embed_options_disable_redirect_to_feedback=true&embed_options_disable_interventions=true  
**replace `xxxxxxxx` with the token from your CodeOcean exercise!**  
embedding options:  
  -  `redirect_to_rfcs`: disable finished users to be lead to open Request for Comments (RfCs).
  -  `redirect_to_feedback`: after "Submit", if there are no open RfCs, a feedback form is presented to 10% of users (min 20) without full score. 
  -  `interventions`: disable popups like "You seem to have trouble. Request comments here" while users are working.
  -  [all options](https://github.com/openHPI/codeocean/blob/master/app/controllers/concerns/lti.rb#L212-L225)


## CodeOcean exercises

All file types & roles etc can be seen in my basic exercise at <https://codeocean.openhpi.de/exercises/721>  
You can only have one 'main' file, the rest must be 'executable file (similar to main file)'.  
The structure of task and test scripts can also be seen at <https://github.com/openHPI/codeoceanR/tree/main/inst/extdata>  
A collection of exercises can be requested through [Berry](mailto:berry-b@gmx.de) or [Sebastian](mailto:sebastian.serth@hpi.de).  
There's an overview of all R exercises at <https://codeocean.openhpi.de/exercises?&q[execution_environment_id_eq]=28>  

The Makefile run: could have `Rscript ${FILENAME}` as well, 
but then the run output does not contain the calls, making error sources harder to find.  

Hide your exercices if wanted (e.g. during development) by unchecking the 'Public' box.  
The admins would like you to use a prefix in the exercise name, e.g. Fprog20 for my course.

If you have data files to be read, make sure to check the box for Read-only.  
Otherwise users might change the file and your test script might fail,
e.g. if you have a file with meta data in the first lines and you want to 
emphasize to never touch raw data and use `read.table(...,skip=n)` instead.

## Testing

I suggest developing (and testing) the tasks within Rstudio.

In the test script, have `rt_run_script()` right before the actual tests.
That way, you can check for the value of `n` even if students create `n` 
for other purposes in a later script, e.g. in a student-defined loop.

Always test the entire exercise on CodeOcean as well, especially after expanding tests.  
Example: `rt_has_argument` didn't run online in the first version, 
since `parse(code)` needs to have `keep.source=TRUE`. 
The default option is TRUE only in an interactive R session!

If possible, recruit two student assistants just to test-run the exercise. They might find errors you overlooked.  
Example: in a statement combination task, the solution was meant to be `c(A=4, C=1, D=3)`,
and I had not thought of students using a different order (which is OK). The solution for the test script:  
`if(exists("obj")) obj <- obj[order(names(obj))]`  
Another example: I once had a task where I tested `rt_has_nrows(df_object, 6)` 
accidentally without first testing `rt_has_class(df_object, "data.frame")`. 
Someone used `readLines` instead of `read.table` and the test script failed.




Instead of checking code like in the [write.table task](https://github.com/openHPI/codeoceanR/blob/main/inst/extdata/script_2.R#L9-L13) and [test](https://github.com/openHPI/codeoceanR/blob/main/inst/extdata/tests.R#L61-L73), 
your tests can also execute the code and you test the resulting file.
This gives participants more freedom in how they structure the task.  

In tests, **never use an object created by the students for reference**.
Even results from a previous task should be created in the test-script.
Students can have their own copy of e.g. `iris`, hence use `datasets::iris`




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
