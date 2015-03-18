raven
=====

A tool for setting up R environment with particular package versions

Install via:
```R
install.packages('devtools');
devtools::install_github('lev-kuznetsov/devtools');
devtools::install_github('dfci-cccb/raven');
```
=====

Currently supports most of cran and bioconductor packages, the exported
```provide``` function accepts package specifications in a named list
format with package name as the name and version as the value (i.e.
```provide (NMF = '0.20.5')```). The version may be omitted (i.e
```provide (NMF = )```) in which case a warning is issued listing all
available versions and the version eventually used. Fill in the version
be rid of the warning. The packages will be installed in a structure
under ```~/.raven``` by default, can be tweaked with the ```local```
parameter to ```provide``` or setting the ```raven.local``` option

The ```provide``` function sets up the library paths and if the
optional ```code``` parameter, if supplied, will return the result of
its evaluation, if not will invisibly return the library paths. The
call itself does not alter library paths directly, so if you would
like to set up package versions for the console a useful idiom is
to set library paths to the result of ```provide``` call:

```
.libPaths (raven::provide (jsonlite = '0.9.14'));
```

To submit your own package for the repository add a push webhook to
```http://raven-repository.appspot.com/registry/github``` to your
githubrepository. The package must have a DESCRIPTION file and its
dependencies must be resolvable at the time of the snapshot
