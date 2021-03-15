# Cookbook

**This is a step-by-step cookbook to run a full Q methodology analysis with the _qmethod_ package, once you have collected your data.**

Please send your suggestions for improving this document [by email](mailto:aiora.zabala@gmail.com) or open an issue on [the issue tracker](https://github.com/aiorazabala/qmethod/issues).

_Additional:_
* Details about extra functions to aid the data collection and to organise large multilingual Q studies, [here](./Data-management). 
* Another detailed explanation of the basic analysis is [here](https://raw.githack.com/crokology/QforR/master/Instructions.html), prepared by [Christoph Schulze](https://www.researchgate.net/profile/Christoph-Schulze-2).


***
### Contents
  * [1. Introduce data](#1-introduce-your-data-in-an-external-spreadsheet-software)
  * Install (only once):
    * [2. Install R](#2-install-r)
    * [3. Install 'qmethod'](#3-install-the-package-only-once)
  * Getting ready: 
    * [4. Load 'qmethod'](#4-load-the-package-every-time-you-open-r)
    * [5. Set working directory](#5-set-your-working-directory)
    * [6. Import data](#6-import-your-data-into-r)
  * Analysis:
    * [7. Explore correlations between Q-sorts](#7-explore-the-correlations-between-q-sorts)
    * [8. Explore factor loadings](#8-explore-the-factor-loadings)
    * [9. Decide upon the number of factors](#9-decide-upon-the-number-of-factors-to-extract)
    * [10. Final analysis](#10-run-the-final-analysis)
  * Results:
    * [11. Explore results](#11-explore-the-results)
    * [12. Export results and report](#12-export-the-results)

***


### 1. Introduce your data (in an external spreadsheet software)


1. The table shall have the following structure: statements as rows, Q-sorts as columns, and the scores of each Q-sort for each statement in the cells (as in the image).

![](http://aiorazabala.net/wp-content/uploads/2019/01/csvfile.png)

2. Export it into *.[CSV](http://en.wikipedia.org/wiki/Comma-separated_values) format, for example: `mydata.csv`. This format is most versatile and is the format used in this cookbook. Any other format is fine, as long as it can be imported in R.

    _Note:_ For the analysis, the data should only contain numbers. Column A in the image above contains the names of rows and requires setting row names (e.g. using `row.names()`, as explained in [step 6A](./Cookbook#a-import-from-csv) below). Row names are useful to navigate and interpret the results. However, if you want to skip setting row names, then delete any column in the CSV containing them.

Alternatively, if you have introduced your data in [PQMethod](http://schmolck.userweb.mwn.de/qmethod/), find the *.DAT file of your project, e.g. `myproject.dat`, and use function `import.pqmethod()` to import your data.

If you *do not* have one synthetic table as above, but only individual, **raw** Q sorts, you can use the `import.q.sorts()` function to build such a matrix with items as rows, and participants as columns.
For more information, see the page on [data management](./Data-management).

***


### 2. Install R

* [In Linux](http://cran.r-project.org/bin/linux/)
* [In Windows](http://cran.r-project.org/bin/windows/base/)
* [In Mac](http://cran.r-project.org/bin/macosx/)

See full details in [CRAN - R project](http://cran.r-project.org/). 
If you wish to use a [Graphical User Interface (GUI) for R](http://www.linuxlinks.com/article/20110306113701179/GUIsforR.html) rather than pure command lines, try [R Commander](http://socserv.mcmaster.ca/jfox/Misc/Rcmdr/installation-notes.html), [Deducer](http://www.deducer.org/pmwiki/index.php?n=Main.DownloadingAndInstallingDeducer), [RKWard](http://rkward.sourceforge.net/wiki/Main_Page) or [RStudio](http://www.rstudio.com/). 
However, all the code you need to know is below.

![](http://pluto.huji.ac.il/~msby/StatThink/Screenshots_WinXP/7-R-Console.PNG)

Example of the R console.
***


### 3. Install the package (only once)

Copy the code below and paste it in the R console:

```r
install.packages("qmethod")
```

***
### 4. Load the package (every time you open R)

Copy the code below and paste it in the R console:

```r
library(qmethod)
```

***


### 5. Set your working directory

The working directory is where your files are located, and where your results and plots will be exported.

```r
setwd("your_path")
```

You should replace `your_path` with the location of your folder. In **Linux** and **Mac** it will be something like `setwd("/home/johnbrown/MyQstudy")`. 
In **Windows** it will be something like `setwd("C://MyDocuments/MyQStudy")`

If you are using a GUI such as *R Studio*, you can set the working directory by using the menus. In [RStudio](http://www.rstudio.com/) go to *Session > Set Working Directory > Choose Directory*.

***


### 6. Import your data into R

There are a number of options, depending on the format of your data.

Below are examples of code, you will need to adapt those examples to your particular file names etc. To import other formats not indicated below (such as SPSS, Excel, or Stata), see [Quick R](http://www.statmethods.net/input/importingdata.html).

#### A. Import from CSV
See `help(read.csv)` for help on additional arguments, such as whether the first row of your data is the name of the columns, etc.
```r
mydata <- read.csv("mydata.csv")

# If needed, set the row names. For example, if the first column contains row names, use:
# row.names(mydata) <- mydata[ ,1]
# Then delete the first column, so that the matrix or data frame contains only numbers:
# mydata[ ,1] <- NULL
```

#### B. Import from PQMethod
```r
mydata <- import.pqmethod("myproject.dat")
```

#### C. Introduce it manually
```r
# Create one vector for each Q-sort:
qsort1 <- c(-1,  0, -2,  0, -2, ...)
qsort2 <- c(-1,  0, -1, -3,  2, ...)
...

# Bind all the Q-sorts together:
mydata <- cbind(qsort1, qsort2, ...)
```

#### D. Import from Raw Q-Sorts as CSV

You can use the functions `import.q.concourse`, `import.q.sorts`, `import.q.feedback` and `build.q.set` to import everything from raw *.CSV (and *.TEX) files start your project from scratch, and automatically.
For more information, see the page on [data management](data-management).

***
Check that the data are correctly imported:
```r
# See the number of statements and of Q-sorts:
dim(mydata)

# See the whole dataset:
mydata
```

***

### 7. Explore the correlations between Q-sorts
You can choose the correlation method from either: Pearson, Kendall or Spearman (Pearson being the default option). 

See the help page for the function `cor()` for more details.

```r
cor(mydata) 
```

***

### 8. Explore the factor loadings
This will help you decide whether to do automatic or manual flagging. For details on how to run the analysis with manual flagging or how to change the loadings, see [Advanced analysis](./Advanced-analysis).

```r
# Run the analysis
# Create an object called 'results', and put the output 
# of the function 'qmethod()' into this object
# (replace the number of factor 'nfactors' as necessary)
results <- qmethod(mydata, nfactors = 3)

# See the factor loadings
round(results$loa, digits = 2)

# See the flagged Q-sorts: those indicating 'TRUE'
results$flag
```
Additionally, you can print the loadings and the flags next to each other using the function `loa.and.flags`:
```r
# Print the table of factor loadings with an indication of flags, 
# as extracted from the object results$flag:
loa.and.flags(results)
```
Note the `qmethod()` function uses "PCA" `extraction` and "varimax" `rotation` by default, but these attributes can be changed easily (see `help(qmethod)` for full details; attribute `extraction` in `qmethod` package versions >=1.7 only).


***

### 9. Decide upon the number of factors to extract
See more about the criteria to decide on the number of factors in [Watts & Stenner (2012, pp.105-110)](http://www.uk.sagepub.com/books/Book234368).

#### A. Eigenvalues, total explained variability, and number of Q-sorts significantly loading
```r
results$f_char$characteristics
# Column 'eigenvals': eigenvalues
# Column 'expl_var':  percentage of explained variability
# Column 'nload':     number of Q-sorts flagged
```

#### B. Screeplot
```r
screeplot(prcomp(mydata), main = "Screeplot of unrotated factors", 
          type = "l")
```

***

### 10. Run the final analysis
Once the final number of factors has been decided, run the analysis again.

The method below uses by default Pearson coefficient for the initial correlation, "PCA" extraction and "varimax" rotation; you can change these in the arguments `extraction`, `rotation` and `cor.method` (see `help(qmethod)` for full details; attribute `extraction` in `qmethod` package versions >=1.7 only).

#### A. Studies with forced distribution
```r
# Create an object called 'results', and put the output 
# of the function 'qmethod' into this object:`
results <- qmethod(mydata, nfactors = 3)
```

#### B. Studies with non-forced distribution
```r
# Create a vector with the scores of the expected distribution
distro <- c(-3, -3, -2, -2, -2, -1, -1, -1,  0, ... )

# Create an object called 'results', and put the output 
# of the function 'qmethod' into this object:`
results <- qmethod(mydata, nfactors = 3, 
                   forced = FALSE, 
                   distribution = distro)
```

***

### 11. Explore the results

#### A. Summary: general characteristics and factor scores
```r
summary(results)
```

#### B. Full results
See details of all the objects in the results in [Zabala (2014, pp. 167)](http://journal.r-project.org/archive/2014-2/zabala.pdf).
```r
results
```

#### C. Plot the z-scores for statements
Statements are sorted from highest consensus (bottom) to highest disagreement (top).
```r
plot(results)
```

#### D. Reorder the statements from highest to lowest scores for each factor
```r
# Put z-scores and factor scores together
scores <- cbind(round(results$zsc, digits=2), results$zsc_n)
nfactors <- ncol(results$zsc)
col.order <- as.vector(rbind(1:nfactors, (1:nfactors)+nfactors))
scores <- scores[col.order]
scores

# Order the table from highest to lowest z-scores for factor 1
scores[order(scores$zsc_f1, decreasing = T), ]
# (to order according to other factors, replace 'f1' for 'f2' etc.)
```

#### E. Explore the table of distinguishing and consensus statements
See a detailed explanation of this table in [Zabala (2014, pp. 167-8)](http://journal.r-project.org/archive/2014-2/zabala.pdf).
```r
# Full table
results$qdc

# Consensus statements
results$qdc[which(results$qdc$dist.and.cons == "Consensus"), ]

# Statements distinguishing all factors
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes all"), ]

# Statements distinguishing factor 1 (for results of > 2 factors)
results$qdc[which(results$qdc$dist.and.cons == "Distinguishes f1 only"), ]
```

***

### 12. Export the results

#### A. In R data format
```r
save(results, file = "myresults.Rdata")

# Load them again
load("myresults.Rdata")
```

#### B. Individual tables to be imported into a spreadsheet
See all the tables in the results that can be exported in [Zabala (2014, pp. 167)](http://journal.r-project.org/archive/2014-2/zabala.pdf), or by looking at the structure of your results `str(results)`.

```r
# Table of z-scores:
write.csv(results$zsc,   file = "zscores.csv")

# Table of factor scores:
write.csv(results$zsc_n, file = "factorscores.csv")

# Table of Q-sort factor loadings:
write.csv(results$loa,   file = "loadings.csv")
```

#### C. Report of all results (text file)
```r
export.qm(results, file = "myreport.txt", style = "R")
```

#### D. Report of results (text file) with the structure of a [PQMethod](http://schmolck.userweb.mwn.de/qmethod/pqmanual.htm) report
This is equivalent to the report in a *.LIS file.
```r
export.qm(results, file = "myreport-pqm.txt", style = "PQMethod")
```
