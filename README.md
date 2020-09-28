# swiplr
Use SWI-Prolog from R

Full documentation at [PDF](inst/Usage.pdf) or [HTML](inst/Usage.html)

# What for

To call prolog from Rmd files and generate prolog based reports from RStudio.

# Install

First of all, install SWI-Prolog. Make sure swipl binary is in your path.

> I tested it once on windows. I manly use it from Linux. 
> 64bit Windows seemed to miss swipl.exe, but 32bit worked fine. In case windows
version looks broken and you need it, let me know!

Then you can install the R package:

```r
# you need devtools, did you try install.packages(devtools) ?
devtools::install_github("https://github.com/battmanux/swiplr.git")
```
# Basic usage

Load R library
```r
# this is an R chunk
library(swiplr)
```

Add a prolog chunk
```prolog
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

```r
# This is some R code calling prolog output 
# R variable is named after prolog chunk label

str(foo_list)
```

```r
'data.frame':	2 obs. of  1 variable:
 $ FOO: chr  "bar" "other"
```

