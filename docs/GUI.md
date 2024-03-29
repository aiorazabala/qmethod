## Graphical User Interface

If you are not familiar with R, there is a Graphical User Interface (GUI) that can be used either online or offline. The [online version](https://azabala.shinyapps.io/qmethod-gui/) does not require installation. The offline version requires installing R and the package first (see instructions below).

[![](http://aiorazabala.net/qmethod-gui/Qmethod_Shiny_GUI2.png)](https://azabala.shinyapps.io/qmethod-gui/)

Note that the GUI offers only limited functionality from all what's available using the full package (See 'Technical notes' in the GUI).
### A. [Online version (link to external site)](https://azabala.shinyapps.io/qmethod-gui/)
### B. Install the offline version:

1. Install [R](https://cran.r-project.org/) and the package [qmethod](https://cran.r-project.org/web/packages/qmethod/index.html) in your computer, following steps 2 and 3 in the [Cookbook](./Cookbook). This needs to be done only once.

2. Open R. Run the package and the GUI, by copying & pasting this code:
```{r}
library(qmethod)       
runInterface()
```

You might need to also install the `shiny` package that provides the interface, e.g. `install.packages("shiny")`.

***
<sup>Are you looking for the manual way of installing the GUI? (for `qmethod` versions <1.8), [see the old instructions here](./GUI-old) (deprecated).</sup>
