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

