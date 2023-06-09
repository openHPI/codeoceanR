# CodeOcean graded exercises for R

By Berry Boessenkool, 2020-2023, [berry-b@gmx.de](mailto:berry-b@gmx.de)  
jump to [students](#students) / [teachers](#teachers) / [developers](#developers)

# students

*Watch lesson 1.4 in the [fundamentals of programming](https://open.hpi.de/courses/hpi-dh-fprog2022) course.*

### **setup** _(once only)_

- Create a folder for all the exercises
- Run the following code (in R / Rstudio) to install the `codeoceanR` package:

```r
install.packages("remotes")
remotes::install_github("openHPI/codeoceanR")
```

- On **Windows**: ignore the **Rtools warning**. If you like, [install](https://cran.r-project.org/bin/windows/Rtools) it at e.g. `C:/Rtools` (compiler paths may not have spaces) and run in R: `cat('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file= "~/.Renviron", append=TRUE)`
- On **Linux**: first run in terminal (STRG+ALT+T): `sudo add-apt-repository ppa:c2d4u.team/c2d4u4.0+` and `sudo apt-get update` and `sudo apt install libcurl4-openssl-dev libssl-dev r-cran-rjson`. Then in R the code above. If it fails, follow the instructions from installing the dependencies in R: `install.packages("curl")` and then with `"openssl"`, `"httr"` and `"rjson"`.

Potential problems (and solutions):

- For "Failed to install ... **Permission denied**": set write permissions (rightklick on folder - properties - security - edit - select user - full control - OK). Turn off virus scanner or reboot computer.
- Permanently use **custom folder for packages**: in R `cat('R_LIBS_USER="C:/path/to/library"', file= "~/.Renviron", append=TRUE)`. After R restart, should be the first result from `.libPaths()`.
- For "**Failed to R CMD build** package": `remotes::install_github("openHPI/codeoceanR", build=FALSE)`
- If only an **old version of R** is possible: for rjson `install.packages("devtools")` and `devtools::install_version("rjson", version="0.2.20", repos="http://cran.us.r-project.org")`
- If you use **another editor** than Rstudio: don't run `rt_create()` below, instead unzip the file manually.
- For `rt_create()` error message "Could not load the **Qt platform plugin xcb**": MAYBE! AT OWN RISK: `sudo apt remove libxcb-xinerama0` and `sudo apt install libxcb-xinerama0`, see [here](https://open.hpi.de/courses/programmieren-r2022/question/5a424cfa-3a86-4215-9337-9337e52c8277)!


### **procedure** _(for each exercise)_

prepare exercise:

- through OpenHPI, go to the CodeOcean exercise
- **download** it to the folder mentioned above, no need to unzip
- **close** the CodeOcean browser **tab**
- **run** (in R / Rstudio Console) `codeoceanR::rt_create()`
- **confirm** to have closed the tab
- **select** the exercise file. _If unzipped (the default on Mac OS Safari), any file within the task folder._

use `rt_create` only once per excercise. Start the project later by opening the `zz_*.Rproj` file in your file browser.


take exercise:

- work on task 1
- save & source the script with `CTRL + SHIFT + S`, this will run `codeoceanR::rt_score()`
- work on task 2
- `CTRL + SHIFT + S`
- ...
- when finished: run `codeoceanR::rt_submit()`

`codeoceanR::rt_score()` transfers your code to CodeOcean for scoring. Run this frequently.  
`codeoceanR::rt_submit()` at the end submits the score to openHPI. Run this only once.  


### design

`rt_create()` should

- create a new folder with a `.Rproj` file
- open the Project in Rstudio
- with the `script_n.R` files already opened
- have everything prepared so `rt_score()` works out of the box.

use `rt_create(deletezip=FALSE)` to not delete the zip file if task creation was successful.  


### codeOcean in browser

**Don't open several tabs** of an exercise. The autosave could overwrite all you've done from a second (unused=empty) tab.  
For the same reason, **don't use the back button** of your browser.  
Thanks to autosave, **you can refresh** the tab or reopen it later to continue working on the tasks.  
Remember to always access this from the openHPI platform.

- Click `RUN` to source the script. Read the output, fix your code if it throws an error. `RUN` again.
- Click `SCORE` to see how many of the tests you pass. Change your solutions if needed. `SCORE` again.  
Ignore messages for tasks you haven't worked on yet (they show how much work is left).
- `RUN` and `SCORE` often to easily find the location of mistakes.
- Finally, when you’re content, click `SUBMIT`.

The tasks are split in separate files. Each can be run independently. Scoring happens for all files at once.  
I suggest to always read the entire task before starting to work on it ;-)

Remember to explicitely call an object to see its value in the `RUN` output.  
`head(x)` / `str(x)` / `summary(x)` work fine.  
`plot(x)` outputs are only shown if the call is between `rt_plot1()`/`rt_plot2()`.

If `SCORE` tells you *'script_x.R' can not be executed*, you cannot get points for tasks (in that script) after the error.

If the top line shows `RENDER` instead of `RUN`, you're probably in a data text file. Just click on the desired script.R again.

Click the arrow on the top left (or *hide* at the bottom right) to hide the instructions for more screen-space for the code.

To reset a complete exercise, click the backwards clock button in the file panel on the left.  
To reset only a single file, click the button on the bottom right of the screen.  


# teachers

*Watch lesson 1.4 in the [fundamentals of programming](https://open.hpi.de/courses/hpi-dh-fprog2022) course.*

It takes a bit of effort to initialize interactive R coding exercises in openHPI / CodeOcean, but it's worth it. 
The mostly automatic system enables you to put most day-to-day focus on developing good exercises instead of grading them.

For more on CodeOcean and CodeHarbor, see the corresponding article and slides at the [SPLICE 2021 workshop](https://cssplice.github.io/SIGCSE21/proceedings.html).

Exercises are accessed through openHPI but run and tested at CodeOcean, from which grades are passed back.
The tasks can also be solved in Rstudio, which is greatly recommended because it is the habitual _and_ future environment with interactivity, autocompletion, debugging, keyboard shortcuts and integrated graphics, help, package manager, ...

Some participants had a hard time getting started in a time-pressed graded exercise setting.  
I suggest to first use the system at least twice(!) non-graded or non-time-pressured!  
You can't stress enough that participants need to run "Score" / `rt_score()` very often.  

## initial setup

The [openHPI admins](https://open.hpi.de/pages/contact) will have to create a CodeOcean exercise in OpenHPI for you the first time.  
To be logged in, open CodeOcean from openHPI through "Launch exercise tool".  
Then you can either go to <https://codeocean.openhpi.de/exercises/new>
or copy my [master template exercise](https://codeocean.openhpi.de/exercises/721). 
Potentially, admins must first duplicate it for you and set you as the author.

## exercise acces point on openHPI

On OpenHPI, go to Course administration - Course structure and content, e.g. 
[url for fprog2020](https://open.hpi.de/courses/fprog-wi-2020/sections).  
In the desired section, click "Add item"

- **Title**: e.g. exercise 3
- **Type**: External exercise tool
- **Exercise type**: Self Test (ungraded) or Main (graded)
- **Maximal points**: e.g. 10
- **Submission deadline**: optional: time before which the CodeOcean task must be **started**.  
The actual deadline and grace period (20% score reduction) must be set in the CO task itself (if wanted).
- **Submission publishing date**: not relevant, can be left empty
- **Instructions**: e.g. Click the button below to launch the exercise.
- **LTI provider**: CodeOcean
- **Additional parameters**: locale=en&token=`xxxxxxxx`&embed_options_disable_redirect_to_rfcs=true&embed_options_disable_redirect_to_feedback=true&embed_options_disable_interventions=true  
**replace `xxxxxxxx` with the token from your CodeOcean exercise!**  
embedding options:  
  -  `redirect_to_rfcs`: finished users can be lead to open Request for Comments (RfCs).
  -  `redirect_to_feedback`: after "Submit", if there are no open RfCs, a feedback form is presented to 10% of users (min 20) without full score. 
  -  `interventions`: popups like "You seem to have trouble. Request comments here" while users are working.
  -  [all options](https://github.com/openHPI/codeocean/blob/master/app/controllers/concerns/lti.rb#L212-L225)


## CodeOcean exercises

All file types & roles etc can be seen in my master template exercise at <https://codeocean.openhpi.de/exercises/721>  
You can only have one 'main' file, the rest must be 'executable file (similar to main file)'.  
The structure of task and test scripts can also be seen at <https://github.com/openHPI/codeoceanR/tree/main/inst/extdata>  
A collection of tasks can be requested through [Berry](mailto:berry-b@gmx.de) or [Sebastian](mailto:sebastian.serth@hpi.de).  
There's an overview of all R exercises at <https://codeocean.openhpi.de/exercises?&q[execution_environment_id_eq]=28>  

The Makefile run: could have `Rscript ${FILENAME}` as well, 
but then the run output does not contain the calls, making error sources harder to find.  

Hide your exercices if wanted (e.g. during development) by unchecking the 'Public' box.  
The admins would like you to use a prefix in the exercise name, e.g. Fprog21 for my course.

If you have data files to be read, make sure to check the box for Read-only.  
Otherwise users might change the file and your test script might fail,
e.g. if you have a file with meta data in the first lines and you want to 
emphasize to never touch raw data and use `read.table(...,skip=n)` instead.

## Testing

I suggest developing (and testing) the tasks within Rstudio.

In the test script, have `rt_run_script()` right before the actual tests.
That way, you can check for the value of `n` even if students create `n` 
for other purposes in a later script, e.g. in a student-defined loop.

Always test the entire exercise on CodeOcean as well.  
If possible, recruit two student assistants just to test-run the exercise. They might find errors you overlooked.  
Example: in a statement combination task, the solution was meant to be `c(A=4, C=1, D=3)`,
and I had not thought of students using a different order (which is OK). The solution for the test script:  
`if(exists("obj")) obj <- obj[order(names(obj))]`  




Instead of checking code like in the [write.table task](https://github.com/openHPI/codeoceanR/blob/main/inst/extdata/examples_2.R#L8-L11) and [test](https://github.com/openHPI/codeoceanR/blob/main/inst/extdata/examples_tests.R#L28-L40), 
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
For the 2021 class and the [2022 MOOC](https://open.hpi.de/courses/programmieren-r2022), the test suite was completely rewritten and test script length reduced greatly.

The source code for codeOcean itself is online at <https://github.com/openHPI/codeocean/>



 Updating codeoceanR on the CO-server is a two-step process:  
- In [GH docker actions](https://github.com/openHPI/dockerfiles/actions/workflows/r-4.yml) (be logged in), click 'Run workflow' then again 'Run workflow'. Wait 5 minutes.  
- In [CO execution environment](https://codeocean.openhpi.de/execution_environments/28), from the dropdown next to 'Edit', click 'Synchronize'. Should be done in a few seconds. Try to do this when no one is scoring exercises, as all running containers are interrupted.
Note that changes to the run/test commands in the [Dockerfile](https://github.com/openHPI/dockerfiles/blob/master/co_execenv_r/4/Dockerfile) are not synchronized.  
- Copy `packageVersion("codeoceanR")` to any script and run it to check whether the update was successfull.

