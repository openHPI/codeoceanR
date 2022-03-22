rt_test_env <- list2env(codeoceanR:::rt_env(lang="en"))
# rt_test_objects  unit tests
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


# Functions ----
sol <- function(x) x+1
fun <- function(x) x
x <- 555
ck(F,"'fun(1:5)[1]' should be '2', not '1'.", rt_test_task(7, fun, sol, inputs=c("1:5", "c(2,3,4)")))
fun <- function(x) data.frame(x)
ck(F, "'fun(1:5)' should have class 'numeric', not 'data.frame'.", rt_test_task(7, fun, sol, inputs=c("1:5", "4:2)")))



# class ----
ck(T,"",                                                                  rt_test_object(1:5, seq(1,5,1)))
ck(F,"'1:5' should have class 'numeric', not 'integer'.",                 rt_test_object(1:5, seq(1,5,1), intnum=FALSE))
ck(F,"'rivers' should have class 'data.frame', not 'numeric'.",           rt_test_object(rivers, iris))
ck(F,"'iris' should have class 'numeric', not 'data.frame'.",             rt_test_object(iris, rivers))
ck(F,"'iris' should have class 'matrix', not 'data.frame'.",              rt_test_object(iris, VADeaths))
ck(F,"'VADeaths' should have class 'data.frame', not 'matrix, array'.",   rt_test_object(VADeaths, iris))

aa <- as.matrix(iris)
ck(F,"'aa' should have class 'data.frame', not 'matrix, array'.",         rt_test_object(aa, iris))
ck(F,"'iris' should have class 'matrix', not 'data.frame'.",              rt_test_object(iris, aa))


# dim ----
aa <- rivers[1:10]
ck(F,"'aa' should have length 141, not 10.",                              rt_test_object(aa, rivers))
ck(F,"'rivers' should have length 10, not 141.",                          rt_test_object(rivers, aa))

aa <- iris[1:10,]
ck(F,"'aa' should have 150 rows, not 10.",                                rt_test_object(aa, iris))
ck(F,"'iris' should have 10 rows, not 150.",                              rt_test_object(iris, aa))

aa <- iris[,1:3]
ck(F,"'aa' should have 5 columns, not 3.",                                rt_test_object(aa, iris))
ck(F,"'iris' should have 3 columns, not 5.",                              rt_test_object(iris, aa))


# names ----
aa <- rivers; names(aa) <- paste0(LETTERS, 1:141)
ck(F,"'aa' should not have names, but has: A1, B2, C3, [...]",            rt_test_object(aa, rivers))
ck(F,"'names(rivers)' should be 'A1, B2, .* J140, K141', not 'NULL'",     rt_test_object(rivers, aa))
ck(F,"'names(rivers)[1]' should be 'A1', not 'NULL'",                     rt_test_object(rivers, aa, stepnames=TRUE))
ck(T,"",                                                                  rt_test_object(aa, rivers, names=FALSE))
ck(T,"",                                                                  rt_test_object(rivers, aa, names=FALSE))

aa <- iris ; colnames(aa) <- LETTERS[1:5]
ck(F,"'colnames(aa)' should be 'Sepal.L.*, Species', not 'A, B, .*, E'",  rt_test_object(aa, iris))
ck(F,"'colnames(aa)[1]' should be 'Sepal.Length', not 'A'",               rt_test_object(aa, iris, stepnames=TRUE))
ck(F,"'colnames(iris)' should be 'A,.*E', not 'Sepal.Length, S.*ecies'",  rt_test_object(iris, aa))
ck(F,"'colnames(iris)[1]' should be 'A', not 'Sepal.Length'",             rt_test_object(iris, aa, stepnames=TRUE))

aa <- iris ; rownames(aa) <- paste0(LETTERS, 1:150)
ck(F,"'rownames(aa)' should be '1, 2,.* 149, 150', not 'A1, B2,.*N66, O6",rt_test_object(aa, iris))
ck(F,"'rownames(aa)[1]' should be '1', not 'A1'",                         rt_test_object(aa, iris, stepnames=TRUE))
ck(F,"'rownames(iris)' should be 'A1, B.*9, T150', not '1, 2,.*",         rt_test_object(iris, aa))
ck(F,"'rownames(iris)[1]' should be 'A1', not '1'",                       rt_test_object(iris, aa, stepnames=TRUE))

aa <- stack.x ; rownames(aa) <- LETTERS[1:21]
ck(F,"'rownames(aa)' should be 'NULL', not 'A, B, C, .* T, U'",           rt_test_object(aa, stack.x))
ck(F,"'rownames(aa)[1]' should be 'NULL', not 'A'",                       rt_test_object(aa, stack.x, stepnames=TRUE))
ck(F,"'rownames(stack.x)' should be 'A, B, C, D,.*S, T, U', not 'NULL'",  rt_test_object(stack.x, aa))
ck(F,"'rownames(stack.x)[1]' should be 'A', not 'NULL'",                  rt_test_object(stack.x, aa, stepnames=TRUE))

aa <- stack.x ; colnames(aa) <- LETTERS[1:3]
ck(F,"'colnames(aa)' should be 'Air.Flow, W.* Acid.Conc.', not 'A, B, C'",rt_test_object(aa, stack.x))
ck(F,"'colnames(aa)[1]' should be 'Air.Flow', not 'A'",                   rt_test_object(aa, stack.x, stepnames=TRUE))
ck(F,"'colnames(stack.x)' should be 'A, B, C', not 'Air.Flow, W.*.Conc.'",rt_test_object(stack.x, aa))
ck(F,"'colnames(stack.x)[1]' should be 'A', not 'Air.Flow'",              rt_test_object(stack.x, aa, stepnames=TRUE))


# column classes ----
aa <- iris ; aa$Species <- as.numeric(aa$Species)
ck(F,"'aa[ ,\"Species\"]' should .*class 'factor', not 'numeric'.",rt_test_object(aa, iris))
ck(F,"'iris[ ,\"Species\"]' should.*lass 'numeric', not 'factor'.",rt_test_object(iris, aa))
ck(F,"'iris[ ,\"Species\"]' should.*lass 'numeric', not 'factor'.",rt_test_object(iris, aa, intnum=FALSE))

aa <- iris ; aa$Species <- as.integer(aa$Species)
ck(F,"'aa[ ,\"Species\"]' should have.*s 'factor', not 'integer'.",rt_test_object(aa, iris))
ck(F,"'iris[ ,\"Species\"]' should have .*integer', not 'factor'.",  rt_test_object(iris, aa))
ck(F,"'iris[ ,\"Species\"]' should have .*integer', not 'factor'.",rt_test_object(iris, aa, intnum=FALSE))

aa <- BOD ; aa$Time <- as.integer(aa$Time)
ck(T,"",                                                                  rt_test_object(aa, BOD))
ck(F,"'aa[ ,\"Time\"]' should have c.*s 'numeric', not 'integer'.",rt_test_object(aa, BOD, intnum=FALSE))
ck(T,"",                                                                  rt_test_object(BOD, aa))
ck(F,"'BOD[ ,\"Time\"]' should hav.*ass 'integer', not 'numeric'.",rt_test_object(BOD, aa, intnum=FALSE))


# hasval ----
aa <- BOD; aa[2,1] <- 99 ; aa$Time <- as.integer(aa$Time)
ck(F,"'aa[2,\"Time\"]' should be '2', not '99'",                          rt_test_object(aa, BOD))
ck(F,"'aa[ ,\"Time\"]' should be '1, 2, 3,.*7', not '1, 99, 3, 4, 5, 7'", rt_test_object(aa, BOD, stepwise=FALSE))
ck(F,"'BOD[2,\"Time\"]' should be '99', not '2'",                         rt_test_object(BOD, aa))
ck(F,"'BOD[ ,\"Time\"]' should be '1, 99, 3, 4, 5, 7', not '1.* 4, 5, 7'",rt_test_object(BOD, aa, stepwise=FALSE))
ck(T,"",                                                                  rt_test_object(BOD, aa, hasval=FALSE))
ck(T,"",                                                                  rt_test_object(BOD, aa, hasval=FALSE, stepwise=FALSE))

