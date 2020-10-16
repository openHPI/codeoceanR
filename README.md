# CodeOcean graded quizzes for R


# students

Run the following code:

```r
if(!requireNamespace("remotes", quietly=TRUE))    install.packages("remotes")
if(!requireNamespace("codeoceanR", quietly=TRUE)) remotes::install_github("openHPI/codeoceanR")
codeoceanR::rt_updatePackage()
```

Through OpenHPI, go to CodeOcean exercise, then download it to a good location on your PC.  
Run the following code for the zip file you just saved:

```r
codeoceanR::create_task("path/to/ex.zip", "folder/to/be/used")
```

This creates a new folder with a `.Rproj` file and tries to open that in Rstudio.  
In the `Files` pane, open the `script_n.R` files.


# developers

All functions in the package are prefixed with `rt_` (R test) for autocomplete

# teachers

Instructions by Berry Boessenkool, January 2020, <berry-b@gmx.de>  
with great info from Sebastian Serth, <sebastian.serth@hpi.de>  
Based on the class "fundamentals of programming in digital health" with 31 participants.


The admins will have to create a dummy quiz in OpenHPI for you the first time.  
To be logged in, open CodeOcean through "Launch exercise tool".  
Then you can either go to https://codeocean.openhpi.de/exercises/new
or have the admins copy my [master exercise](https://codeocean.openhpi.de/exercises/642)
and set you as the author. Once that's done, you can duplicate that one yourself.
(You cannot duplicate my exercise, to prevent random students from seeing my test scripts.)

Overview of all R quizzes:
https://codeocean.openhpi.de/exercises?&q[execution_environment_id_eq]=28


## Quiz acces point on openHPI

On OpenHPI, go to Course administration - Course structure and content, e.g. [url for fprog2019](
https://open.hpi.de/courses/fprog-wi-2019/sections)  
In the desired section, click "Add item"

- **Type**: External exercise tool
- **Exercise type**: Main
- **Maximal points**: e.g. 10
- **Submission deadline**: time before which the CodeOcean task must be **started**.  
There's no real way to limit the working time.  
The admins have a [java tool](https://github.com/openHPI/codeocean-scraper) 
to download scores limited to a certain time stamp.
- **Submission publishing date**: not relevant, can be left empty
- **Instructions**: Click the button below to launch the exercise.
- **LTI provider**: CodeOcean
- **Additional parameters**: locale=en&token=1de7bf22&embed_options_disable_redirect_to_rfcs=true&embed_options_disable_redirect_to_feedback=true&embed_options_disable_interventions=true  
replace `1de7bf22` with the token from your exercise  
embedding options:  
-  `redirect_to_rfcs`: disable finished users to be lead to open Request for Comments (RfCs).
-  `redirect_to_feedback`: after "Submit", if there are no open RfCs, 
a feedback form is presented to 10% of users (min 20) without full score. 
-  `interventions`: disable popups like "You seem to have trouble. Request comments here" while users are working.


## CodeOcean exercise specifics

All file types & roles etc can be seen in my master exercise at
https://codeocean.openhpi.de/exercises/642  
In editing your own exercise (button at topright or URL below), you need to check the 'Public' box.

Quite a lot of work and experience has gone into this master quiz.
I'll try to list the most important lessons here.

In the description, you get markdown line breaks with two spaces at the end of the line.

Some participants had a hard time getting started in CodeOcean in a time-pressed graded quiz setting.  
I suggest to first use the system at least once non-graded and non-pressured!

The first task with an intentional error should help participants get a feel for error debugging.  
You can't stress enough that participants need to run "Score" very often.  

I suggest participants not to switch between codeOcean and Rstudio. 
I know Rstudio has autocomplete + keyboard shortcuts + more interactiveness, 
but in general people were more confused than helped.

The Makefile run: could have `Rscript ${FILENAME}` as well, 
but then the run output does not contain the calls, making error sources harder to find.

If you have data files to be read, make sure to check the box for Read-only.  
Otherwise users might change the file and your test script might fail,
e.g. if you have a file with meta data in the first lines and you want to 
emphasize to never touch raw data and use `read.table(...,skip=n)` instead.

## Testing

I suggest developing (and testing) the tasks within Rstudio.
Just source "tests.R" (after the first time, `loctests()` will do that for you).  
"test_functions.R" contains a local warn function for interactive use

Always test the entire quiz on CodeOcean as well, especially after expanding tests.  
Example: `has_argument` didn't run online in the first version, 
since `parse(code)` needs to have `keep.source=TRUE`. 
The default option is TRUE only in an interactive R session!

Instead of checking code like in the write.table task, 
your tests can also execute the code and you test the resulting file.
This gives participants more freedom in how they stucture the task.  

Attenton: if the user script contains line breaks and is read with `parse_script_section(script2, 4)` with the default `collapse=TRUE`,
`has_argument` leads to a test script failure.

In tests, never compare with a result created by the user, even if that is checked in a previous task.  
E.g., your test script could fail if you have the user do
```
df <- read.table("file.txt")
cmean <- mean(df[,-1])
```
and you test the df in one task and for the next
`has_value(cmean,  mean(df[,-1]) )`  
There will be that one user who fails to specify header or sep or whatever,
meaning that `df` cannot be trusted to be correct.  
Besides not getting a point for the df task, the test script would fail and give no points at all.  
Rather read the dataset in the test script just before the cmean tests or hard-code the value.  
For the same reason, test 1 contains `iris <- datasets::iris`

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
replace 642 with the number of your exercise.  
https://codeocean.openhpi.de/exercises/642/edit  
https://codeocean.openhpi.de/exercises/642/implement  
https://codeocean.openhpi.de/exercises/642/statistics  
https://codeocean.openhpi.de/exercises/642/requests_for_comments  (not auto-updated)
https://codeocean.openhpi.de/exercises/642/study_group_dashboard/32  live view (RfCs auto-updated)

