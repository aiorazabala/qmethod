## Graphical User Interface: install manually (deprecated after `qmethod` v1.8)

These were two ways to install the GUI in `qmethod` versions <1.8, before the GUI was integrated into the package. It's left here for reference.

#### A. Run this script in R

```{r}
source('http://aiorazabala.net/qmethod-gui/qmethod-gui-install.R')
```
The graphical interface will open in your web browser.

This code does the following: it creates a folder named ```qmethod-gui```, downloads the two files for the visual interface (```server.R``` and ```ui.R```), installs the two R packages required for the visual interface (```qmethod``` and [shiny](http://cran.r-project.org/web/packages/shiny/index.html)), loads the two packages, and finally runs the interface, which will open in your web browser.

Once installed, to run it again follow these steps:
* Open R
* Copy and paste the code below (replace ```'C://Mypath'``` with the location of the folder ```qmethod-gui``` on your computer, e.g. if the folder in your computer is *C://My Documents/qmethod-gui/*, use ```setwd("C://My Documents")``` ).
```{r}
setwd("C://Mypath")   # 1. Set the location of the folder 'qmethod-gui'
library(shiny)        # 2. Load the package
runApp("qmethod-gui") # 3. Run the application
```

***
#### B. Do the above steps manually
1. Create a folder, e.g. ```qmethod-gui```
2. Download these two files in the above folder:
[server.R](http://aiorazabala.net/qmethod-gui/server.R) and
[ui.R](http://aiorazabala.net/qmethod-gui/ui.R)
3. Load the required library and run the application:
```{r}
library(shiny)       
runApp("qmethod-gui")
```
Within quotations in ```runApp("qmethod-gui")```, put the name of the folder created in step 1 (i.e. where you downloaded the files ```'ui.R'``` and ```'server.R'```). This folder can take any name, e.g. ```"myqmethod-gui"```.

***
