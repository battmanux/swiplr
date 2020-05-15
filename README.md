# swiplr
Use SWI-Prolog from R

# What for

To call prolog from Rmd files and generate prolog based reports from RStudio.

# Install

First of all, install SWI-Prolog. Make sure swipl binary is in your path.

> I tested it on both windows and linux. 
> 64bit Windows seemed to miss swipl.exe, but 32bit works fine.

Then you can install the R package:

```r
# you need devtools, did you try install.packages(devtools) ?
devtools::install_github("https://github.com/battmanux/swiplr.git")
```
# Basic usage

Load R library
```{r setup}
# this is an R chunk
library(swiplr)
```

Add a prolog chunk
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

This returns a nice table `foo_list` that you can use from R

```{r}
# This is some R code calling prolog output 
# R variable is named after prolog chunk label

str(foo_list)
```

```
'data.frame':	2 obs. of  1 variable:
 $ FOO: chr  "bar" "other"
```

More details in [inst/tests.Rmd](inst/tests.Rmd)

Clean R documentation is not there yet, but main function is `pl_eval`
