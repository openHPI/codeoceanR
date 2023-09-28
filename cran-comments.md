
This package runs tests on code by students.
In order for them to use the formula interface,
rt_has_args needs to use attach in line 119, see:
https://github.com/openHPI/codeoceanR/blob/main/R/rt_has_args.R#L119
After that, their code can be evaluated in the right environment.

I hope this use of attach can be accepted here.
