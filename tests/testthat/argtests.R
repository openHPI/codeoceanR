rt_test_env <- list2env(codeoceanR:::rt_env(lang="en"))
# rt_has_args  unit tests
# This file is sourced in test_package.R

ck <- function(v,m, code, target, ...)
{
m <- sub("(","\\(",sub(")","\\)",m, fixed=TRUE), fixed=TRUE)
m <- sub("[","\\[",sub("]","\\]",m, fixed=TRUE), fixed=TRUE)
testm <- if(interactive()) testthat::expect_message else testthat::expect_output
testm(x <- rt_has_args(code, target, 7, ...), if(v) NA else m)
stopifnot(v==x)
}

cv <- c("orange", "mediumpurple2")           # color vector
ed <- data.frame(a=1:4, b=5:8, c=6:7, e=2:5) # example data.frame

args("plot.default")
args("seq") # is empty   -> hence test with seq (arguments must be named explicitely)
length # is a primitive funtion, hence args(eval(u_fun)) and length tests

rt_has_args(code="length(1:5)", target='length(cv)', snumber=7) # expected message:
# T: code section t7: argument 'x' should have class 'character', not 'integer'.

# c.*7 = code section t7

ck(F,"c.*7: argument 'lwd' should be '3', not '2'."          ,  "plot(1:5, lwd=2)"            ,'plot(1:5, lwd=3)'             )
ck(F,"c.*7 should contain the argument 'lwd'."               ,  "plot(1:5)"                   ,'plot(1:5, lwd=3)'             )
ck(T,""                                                      ,  "plot(1:5)"                   ,'plot(1:5, lwd=3)', opt="lwd"  )
ck(F,"c.*7: argument 'lwd' should be '3', not '2'."          ,  "plot(1:5, lwd=2)"            ,'plot(1:5, lwd=3)', opt="lwd"  )
ck(F,"c.*7 should contain the function 'plot', not '99'."    ,  "99"                          ,'plot(1:5, lwd=3)'             )
ck(F,"c.*7 should cont.* 'plot', not 'plot.default'."        ,  "plot.default(1:5, lwd=2)"    ,'plot(1:5, lwd=3)'             )
                                                                                                                              #
ck(T,""                                                      ,  "length(cv)"                  ,'length(cv)'                   )
ck(F,"ument 'x' should have class 'character', not 'integer'",  "length(1:5)"                 ,'length(cv)'                   )
ck(F,"c.*7: argument 'x' should be 'cv', not '1:5'."         ,  "length(1:5)"                 ,'length(cv)', nameonly=TRUE    )
                                                                                                                              #
ck(T,""                                                      ,  "plot(1:5,    lwd=3)"         ,'plot(1:5, lwd=3)'             )
ck(T,""                                                      ,  "plot(1:5,\n  lwd=3)"         ,'plot(1:5, lwd=3)'             )# line breaks OK
ck(T,""                                                      ,c("plot(1:5,"," lwd=3)")        ,'plot(1:5, lwd=3)'             )# vectors OK
                                                                                                                              #
ck(F,"c.*7: argument 'lwd' should be '3', not '2'."          ,  "plot(1:5,\n  lwd=2)"         ,'plot(1:5, lwd=3)'             )
ck(F,"c.*7: argument 'lwd' should be '3', not '2'."          ,c("plot(1:5,"," lwd=2)")        ,'plot(1:5, lwd=3)'             )
                                                                                                                              #
ck(F,"str2lang for c.*7 produced error:.*unexpected symbol"  ,"plot(1:5) points(2,3)"         ,'plot(1:5)'                    )
ck(F,"may contain one single command only, not 2."           ,"plot(1:5)\n points(2,3)"       ,'plot(1:5)'                    )
ck(F,"may contain one single command only, not 2."           ,"plot(1:5); points(2,3)"        ,'plot(1:5)'                    )
                                                                                                                              #
ck(F,"c.*7 should not contain the argument 'a' more than onc","plot(1, a=2,a=3)"              ,'plot(1:5)'                    )
ck(F,"c.*7 should not contain.*gument 'a, b' more than once.","plot(1, a=2,a=3, b=4,b=5,b=6)" ,'plot(1:5)'                    )
                                                                                                                              #
ck(T,""                                                      ,"plot(1:5, lwd=2)"              ,'plot(1:5, lwd=2,lwd=3)'       )# Checks first argument only
ck(F,"c.*7: argument 'lwd' should be '3', not '2'."          ,"plot(1:5, lwd=2)"              ,'plot(1:5, lwd=3,lwd=4)'       )
                                                                                                                              #
ck(T,""                                                      ,"plot(1:5, col='red')"          ,'plot(1:5, col="red")'         )# both qmarks fine
ck(T,""                                                      ,'plot(1:5, col="red")'          ,'plot(1:5, col="red")'         )
                                                                                                                              #
ck(F,"c.*7: argument 'x' should have.*eger', not 'character'","plot('1:5', lwd=2)"            ,'plot(1:5, lwd=2)'             )
ck(F,"c.*7: argument 'x' should be '1:5', not '\"1:5\"'."    ,"plot('1:5', lwd=2)"            ,'plot(1:5, lwd=2)', nameonly=TRUE)
ck(F,"c.*7: argument 'x' should be '1:5', not 'c(1, 2,.*5)'.","plot(c(1,2,3,4,5))"            ,'plot(1:5)', nameonly=TRUE     )
ck(F,"ent 'lwd' should have class 'numeric', not 'character'","plot(1:5, lwd='2')"            ,'plot(1:5, lwd=2)'             )
                                                                                                                              #
ck(F,"c.*7: argument 'x' should have length 6, not 5."       ,"plot(1:5, lwd='2')"            ,'plot(1:6, lwd=2)'             )
ck(F,"c.*7: argument 'x'.*class 'data.frame', not 'integer'.","plot(1:5, lwd='2')"            ,'plot(iris,lwd=2)'             )
                                                                                                                              #
ck(T,""                                                      ,"plot(c(1,2,3,4,5))"            ,'plot(1:5)'                    )
ck(F,"c.*7: argument 'x'[4] should be '4', not '9'."         ,"plot(c(1,2,3,9,5))"            ,'plot(1:5)'                    )
                                                                                                                              #
ck(F,"c.*7: argument 'col'.*ld be '\"red\"', not '\"blue\"'.","plot(1:5, lwd=2, col='blue')"  ,'plot(1:5, col="red", lwd=3)'  )# Checked in order of appearance in target:
                                                                                                                              #
ck(T,""                                                      ,"plot(1:5, lwd=2)"              ,'plot(1:5, lwd=3)', alt=list(lwd=2:4     ))
ck(T,""                                                      ,"plot(1:5, lwd=2)"              ,'plot(1:5, lwd=3)', alt=list(lwd="anyval"))
ck(F,"argument 'lwd' should be '3', not '2'."                ,"plot(1:5, lwd=2)"              ,'plot(1:5, lwd=3)'             )
                                                                                                                              #
ck(F,"c.*7: argument names cannot be matched in trainer code","seq(0,5,2)"                    ,'seq(0,6,2)'                   )
ck(F,"Arguments for 'seq' must be named explicitely in c.*7.","seq(0,5,2)"                    ,'seq(from=0,to=6,by=2)'        )
ck(T,""                                                      ,"box()"                         ,'box("plot")', opt="which"     )
ck(F,"gument 'which' should be '\"plot\"', not '\"outer\"'." ,"box('outer')"                  ,'box("plot")', opt="which"     )
#
ck(F,"c.*7: argument 'y'[1] should be '1', not '2'."         ,"barplot(e~b, data=ed)"         ,'barplot(a~b, data=ed)'        )
ck(F,"should contain the argument 'col'"                     ,"plot(a~b, data=ed)"            ,'plot(a~b, data=ed, col=cv[e])')
ck(F," 'col' should have length 4, not 2."                   ,"plot(a~b, data=ed, col=cv)"    ,'plot(a~b, data=ed, col=cv[e])')
ck(T,""                                                      ,"plot(a~b,data=ed,col=cv[ed$e])",'plot(a~b, data=ed, col=cv[e])')
ck(T,""                                                      ,"plot(a~b, data=ed)"            ,'plot(a~b, data=ed)'           )
ck(F,"argument 'x'[1] should be '5', not '2'."               ,"plot(a~e, data=ed)"            ,'plot(a~b, data=ed)'           )
ck(T,""                                                      ,"plot(ed$a~ed$b)"               ,'plot(ed$b, ed$a)'             )
ck(T,""                                                      ,"plot(ed$b, ed$a)"              ,'plot(ed$a~ed$b)'              )
ck(T,""                                                      ,"plot(ed$b, ed$a)"              ,'plot(ed$a~ed$b)'              )
ck(F,"'x'[1] should be '5', not '1'"                         ,"plot(ed$a, ed$a)"              ,'plot(ed$a~ed$b)'              )
ck(T,""                                                      ,"plot(ed$a~ed$b,col=2)"         ,'plot(ed$a~ed$b)'              )
ck(T,""                                                      ,"plot(a~b,data=ed)"             ,'plot(formula=a~b, data=ed)'   )
ck(T,""                                                      ,"plot(formula=a~b,data=ed)"     ,'plot(formula=a~b, data=ed)'   )
ck(T,""                                                      ,"plot(ed$b, ed$a )"             ,'plot(ed$b, ed$a          )'   )
ck(T,""                                                      ,"plot(a~b,data=ed)"             ,'plot(ed$b, ed$a          )'   )
ck(T,""                                                      ,"plot(ed$a~ed$b  )"             ,'plot(ed$b, ed$a          )'   )
ck(T,""                                                      ,"plot(ed$a~ed$b  )"             ,'plot(formula=ed$a~ed$b   )'   )
ck(T,""                                                      ,"plot(a~b,data=ed)"             ,'plot(formula=a~b, data=ed)'   )
ck(T,""                                                      ,"plot(ed$b, ed$a )"             ,'plot(formula=a~b, data=ed)'   )
ck(T,""                                                      ,"plot(ed$a~ed$b  )"             ,'plot(formula=a~b, data=ed)'   )
ck(T,""                                                      ,"plot(a~b,data=ed)"             ,'plot(formula=ed$a~ed$b   )'   )
                                                                                                                              #
ck(F,"c.*7: argument 'x'[1] should be '5', not '6'."         ,"plot(ed$c, ed$a)"              ,'plot(ed$b, ed$a          )'   )
ck(F,"argument 'x' should be '5, 6, 7, 8', not '6, 7, 6, 7'.","plot(ed$c, ed$a)"              ,'plot(ed$b, ed$a)', stepwise=FALSE)
ck(F,"c.*7: argument 'x'[1] should be '5', not '6'."         ,"plot(ed$c, ed$a)"              ,'plot(ed$a~ed$b           )'   )
ck(F,"argument 'x' should have class 'integer', not 'NULL'." ,"plot(ed$d, ed$a)"              ,'plot(ed$b,ed$a           )'   )
ck(F,"c.*7: argument 'x'[1] should be '5', not '2'."         ,"plot(a~e,data=ed)"             ,'plot(ed$b, ed$a          )'   )
ck(F,"c.*7: argument 'x'[1] should be '5', not '2'."         ,"plot(a~e,data=ed)"             ,'plot(a~b, data=ed)'           )
ck(T,""                                                      ,"plot(ed$b,ed$a,col=cv[ed$e])"  ,'plot(a~b, data=ed, col=cv[e])')



if(FALSE){
# Formulas are all around hard to handle: message differs depending on stepwise:
rt_has_args("barplot(ed$a~ed$b)", barplot(formula=a~b, data=ed), 7)
rt_has_args("barplot(ed$a~ed$b)", barplot(formula=a~b, data=ed), 7, stepwise=FALSE)
# ERROR ----
ed <- data.frame(a=1:4, b=5:8, c=6:7, e=6:9) # correctly F with e
rt_has_args("plot(a~c,data=ed)", plot(formula=a~b, data=ed), 7)
rt_has_args("plot(a~c,data=ed)", plot(ed$b, ed$a), 7)
# possibly has to do with attach. R CHECK frowns upon that anyways


}
