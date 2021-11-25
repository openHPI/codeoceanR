rt_test_env <- new.env() # to replace "Tnid: message" with "T: message")
# rt_has_args  unit tests
# This file is sourced in test_package.R

ck <- function(v,m, expr)
{
m <- sub("(","\\(",sub(")","\\)",m, fixed=TRUE), fixed=TRUE)
m <- sub("[","\\[",sub("]","\\]",m, fixed=TRUE), fixed=TRUE)
testm <- if(interactive()) testthat::expect_message else testthat::expect_output
if(!v) testm(expr, m)
x <- expr
stopifnot(v==x)
}


ck(F,"code section t7: argument 'lwd' should be '3', not '2'.",                    rt_has_args("plot(1:5, lwd=2)",         plot(1:5, lwd=3), 7))
ck(F,"T: code section t7 should contain the argument 'lwd'.",                      rt_has_args("plot(1:5)",                plot(1:5, lwd=3), 7))
ck(F,"T: code section t7 should contain the function 'plot', not '99'.",           rt_has_args("99",                       plot(1:5, lwd=3), 7))
ck(F,"T: code section t7 should contain the function 'plot', not 'plot.default'.", rt_has_args("plot.default(1:5, lwd=2)", plot(1:5, lwd=3), 7))

ck(T,"",rt_has_args(  "plot(1:5,    lwd=3)" , plot(1:5, lwd=3), 7))
ck(T,"",rt_has_args(  "plot(1:5,\n  lwd=3)" , plot(1:5, lwd=3), 7))
ck(T,"",rt_has_args(c("plot(1:5,"," lwd=3)"), plot(1:5, lwd=3), 7))

ck(F,"T: code section t7: argument 'lwd' should be '3', not '2'.", rt_has_args(  "plot(1:5,\n  lwd=2)" , plot(1:5, lwd=3), 7))
ck(F,"T: code section t7: argument 'lwd' should be '3', not '2'.", rt_has_args(c("plot(1:5,"," lwd=2)"), plot(1:5, lwd=3), 7))

ck(F,"T: str2lang for code section t7 produced error: <text>:1:11: unexpected symbol"        , rt_has_args("plot(1:5) points(2,3)",   plot(1:5), 7))
ck(F,"T: str2lang for code section t7 produced error: parsing result not of length one, but 2",rt_has_args("plot(1:5)\n points(2,3)", plot(1:5), 7))
ck(F,"T: str2lang for code section t7 produced error: parsing result not of length one, but 2",rt_has_args("plot(1:5); points(2,3)",  plot(1:5), 7))

ck(F,"T: code section t7 should not contain the argument 'a' more than once.",   rt_has_args("plot(1, a=2,a=3)",              plot(1:5), 7))
ck(F,"T: code section t7 should not contain the argument 'a, b' more than once.",rt_has_args("plot(1, a=2,a=3, b=4,b=5,b=6)", plot(1:5), 7))

# Checks first argument only:
ck(T,"",rt_has_args("plot(1:5, lwd=2)", plot(1:5, lwd=2,lwd=3), 7))
ck(F,"T: code section t7: argument 'lwd' should be '3', not '2'.",rt_has_args("plot(1:5, lwd=2)", plot(1:5, lwd=3,lwd=4), 7))

# both qmarks fine:
ck(T,"",rt_has_args("plot(1:5, col='red')",  plot(1:5, col="red"), 7))
ck(T,"",rt_has_args('plot(1:5, col="red")',  plot(1:5, col="red"), 7))

ck(F,"T: code section t7: argument 'x' should have class 'inte.*eric', not 'character'.",rt_has_args("plot('1:5', lwd=2)", plot(1:5, lwd=2), 7))
ck(F,"T: code section t7: argument 'x' should be '1:5', not '\"1:5\"'."                 ,rt_has_args("plot('1:5', lwd=2)", plot(1:5, lwd=2), 7, nameonly=TRUE))
ck(F,"T:.*ent 'lwd' should have class 'integer' or 'numeric', not 'character'.",         rt_has_args("plot(1:5, lwd='2')", plot(1:5, lwd=2), 7))

ck(F,"T: code section t7: argument 'x' should have length 6, not 5."                  , rt_has_args("plot(1:5, lwd='2')", plot(1:6, lwd=2), 7))
ck(F,"T: code section t7: argument 'x' should have class 'data.frame', not 'integer'.", rt_has_args("plot(1:5, lwd='2')", plot(iris,lwd=2), 7))

ck(T,"",rt_has_args("plot(c(1,2,3,4,5))", plot(1:5), 7               ))
ck(F,"T: code section t7: argument 'x' should be '1:5', not 'c(1, 2, 3, 4, 5)'.", rt_has_args("plot(c(1,2,3,4,5))", plot(1:5), 7, nameonly=TRUE))
ck(F,"T: code section t7: argument 'x'[4] should be '4', not '9'."              , rt_has_args("plot(c(1,2,3,9,5))", plot(1:5), 7               ))

# Checked in order of appearance in desired expr:
ck(F,"T: code section t7: argument 'col' should be '\"red\"', not '\"blue\"'.",   rt_has_args("plot(1:5, lwd=2, col='blue')",  plot(1:5, col="red", lwd=3), 7))

ck(F,"T: code section t7: argument 'height'[2] should be 'Unemployed', not 'GNP'.", rt_has_args("barplot(GNP~Year, data=longley)",  barplot(Unemployed~Year, data=longley), 7))



# Trouble:
if(FALSE){ # not for automated testing:
rt_has_args("seq(0,5,2)",  seq(0,6,2), 7)
# Should fail, but doesn't because
args("seq") # is empty
args("plot.default")

# Not very good with different messages depending on stepwise:
rt_has_args("barplot(longley$Unemployed~longley$Year)", barplot(Unemployed~Year, data=longley), 7)
rt_has_args("barplot(longley$Unemployed~longley$Year)", barplot(Unemployed~Year, data=longley), 7, stepwise=FALSE)

# Does not give freedom to choose x,y or y~x:
rt_has_args("plot(longley$Unemployed~longley$Year)", plot(longley$Year, longley$Unemployed), 7)
rt_has_args("plot(longley$Year, longley$Unemployed)", plot(longley$Unemployed~longley$Year), 7)

}
