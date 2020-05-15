# swiplr
Use SWI-Prolog from R

# What for

To call prolog from Rmd files and generate prolog based reports from RStudio.

# Install

first install swi-prolog and add swipl binary in your path.
I tested it on both windows and linux. 
64bit Windows version seemed to miss swipl.exe, but 32bit one works fine.

It could work with other versions of prolog, however it is untested.

```r
devtools::install_github("https://github.com/battmanux/swiplr.git")
```
# Basic usage

load R library
```{r setup}
# this is an R chunk
library(swiplr)
```

add a prolog chunk
```{prolog foo_list}
% this is some prolog code
foo(bar).
foo(other).

% define queries by starting lines with ?-
?- foo(X).
```

  |FOO   |
  |:-----|
  |bar   |
  |other |

this returns a nice table `foo_list` that you can use from R

```{r}
# This is some R code calling prolog output 
# R variable is named after prolog chunk label

str(foo_list)
```

```
'data.frame':	2 obs. of  1 variable:
 $ FOO: chr  "bar" "other"
```

more details in [inst/tests.Rmd](inst/tests.Rmd)

Clean R documentation is not there yet, but main function is `pl_eval`
