
# GETTING STARTED
## Install
### Windows / Mac Install
#### (With admin rights on the computer)
1. **Download/Install R**. https://cran.r-project.org
    At the moment, it works with the latest version, 3.6.3.
    If you have issues, from https://cran.r-project.org
    "Download R for windows" > "Base" or "Install R for the first time" > 
    lower on the page pick "Previous Releases" and get 3.6.3. 
    **Install**.
2. **Download/Install RStudio**, the development environment (IDE). https://www.RStudio.com/products/RStudio/download/#download
3. Go to (Start Analyzing Data)[#analyze-data] section.

#### But if you don't have admin rights on the computer
1. Follow instructions here: https://rpubs.com/tomhopper/windows_nonadmin_install
         This will lead you to download and unpack the zip files for both R,
         and RStudio, then run the .exe files within the bin folder.
2. Go to (Start Analyzing Data)[#analyze-data] section.
### Ubuntu / Linux Install
Only tested this on Ubuntu 18.04
1. Follow these instructions
https://cran.r-project.org/bin/linux/ubuntu/README.html
```
sudo nano /etc/apt/sources.list

## add the line below to the file. 
## For Ubuntu version 18.04 See the R ubuntu readme
deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/ 
deb http://cran.utstat.utoronto.ca/bionic-backports main restricted universe
## Save and exit (ctrl+o then ctrl+x)

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

sudo apt-get update
sudo apt-get install r-base
sudo apt-get install r-base-dev
```
2. Download and install RStudio, although it didn't work for me. 

Followed these instructions to install packages
https://www.digitalocean.com/community/tutorials/how-to-install-r-packages-using-devtools-on-ubuntu-18-04
```
sudo apt-get install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
sudo apt-get update
sudo -i R
```
4. Option A To install the package
```
# From within R
devtools::install_github("fsaer/tirefittingr")
```
5. Option B to install packages
```
#From command line
sudo install git
#navigate to a folder to clone the package to. I used /home/your_user_name/Documents/R-Projects
mkdir /home/your_user_name/Documents/R-Projects
cd /home/your_user_name/Documents/R-Projects
git clone https://github.com/fsaer/tirefittingr/
cd tirefittingr
sudo R
devtools::load_
```
## Analyze Data
Install R and RStudio (See above) 
Open the program "RStudio", **Not** "R x64 3.6.3"/etc.

**Note:** This is to use the package to analyze tires. This is NOT for further 
developing the package.

### Install Other useful Packages
copy the following code into the console (Bottom/left window) within RStudio:
```
install.packages("plotly")
install.packages("plyr")
install.packages("dplyr")
install.packages("magrittr")
install.packages("devtools")
```

### Install tirefittingr
#### Option A. Through the console. Run the following:
Where "0.5.0" is the latest release number. To see the latest go to
https://github.com/fsaer/tirefittingr/releases
```
devtools::install_github("fsaer/tirefittingr@0.5.0")  
```
#### Option B. Manually install tirefittingr
You'll have to clone/download 
the repo (See the big green button on the github webpage for this package) 
**As a ZIP File**, then open RStudio.

In the lower right pane of RStudio, switch to the "Packages" tab. Then select 
"Install" at the top of that window. 

In the pop-up window, for the "Install From" dropdown, select "Package 
Archive File (.Zip)" Then in the button pick the zip file of this package
that you just downloaded.

If you have trouble finding these buttons, you can follow the first two images
from this link: https://www.jube.io/r-blog/browsing-and-installing-packages 

### Start a new empty project to work in
In RStudio, create a new **empty** project to work in (File > new project).
Create a new directory and New Project. Do **not** use the tirefittingr github
package zip/unzipped folder as your directory.

Once you have a new, empty project, go file > New File > R Script. 

### Running through examples
#### Option A
View online at 
https://htmlpreview.github.io/?https://github.com/fsaer/tirefittingr/blob/master/docs/Notebook.nb.html

#### Option B
Download the github repo. 
https://github.com/fsaer/tirefittingr
Navigate to and open fsaer/tirefittingr/docs/notebook.nb.html
in a web browser. This will run you through examples.

### Last Thing to get you started
You probably want to start your scripts with the following code to 
load some useful packages that we've installed into memory so they 
can be used.
```
library(plyr)       #   if you use functions from any of these packages
library(dplyr)
library(plotly)
library(magrittr)
library(tirefittingr)
```
# Things to help you understand R
1. "<-" is an assignment operator, which assigns whatever is on the right side to the variable on the left. It is the same as using "=". "<<-" Assigns to a global variable.
3. Pressing ctrl+enter will run the current line of code, or highlighted text in the console
4. Typing "?" then a function will open the help page for that function. Note, if the function is part of one of the added packages (ggplot2 etc), then the package must be installed (install.packages("package_name_here")), AND loaded into the session: (require(package_name_here))
5. Function definition is done at the **top** of a script before the function is used. The actual main function starts way lower. 
To use a function, it must be part of a package that is installed and loaded into memory `library(packagename)`, 
or to define your own function, you can load it into memory by running the entire function definition in the console, or putting 
it into a script that is run (make sure the definition comes **before** the function is used). 
6. When reading others code, you may see the pipe operator "%>%" at the end of a line. It drastically changes how the group of lines is executed. You should read short the "magrittr vingette" (google it).
   -Essentially a line that ends in "%>%" is put into an argument of the function on the next line where a period "." is used as an argument, or if there are no periods, the default is the first argument. 
7. Unlike matlab the default for multiplication and division is element-wise. 
8. Using functions from other packages. There are two steps:
    1. Install (one-time) and 2. Load in to memory (every session)
Packages are available for install in two locations: CRAN and github. Most common ones
are available on CRAN, and can be installed by running the code
install.packages("packageName"). For example:
```
install.packages("plotly")

```

To load into memory use `library(packageName)`.
This code is often placed at the top of your script because it must be run 
every time the R process is restarted.
```
library(devtools)
library(plyr)
library(dplyr)
library(plotly)
library(magrittr)
```
# Get started improving this package!
1. After installing R and RStudio (see first section)
2. Make a local copy (clone) of the git repo. I suggest making a github 
account and getting github desktop
3. Install the following packages through the console window in RStudio. 
```
install.packages("devtools") # Lots of tools
install.packages("lintr") # keeps your code properly formatted
install.packages("roxygen2") # For updating documentation
install.packages("testthat") # For automatic testing
install.packages("knitr") # for making markdown notebooks
```
4. If you haven't downloaded/install RTools yet, download/install it now. https://cran.r-project.org/bin/windows/Rtools/
5. Restart RStudio
6. Open the tirefittingr project in RStudio (file>open project, select .Rproj file 
within the unzipped git repo folder)
7. Attempt to run a test using ctrl+shft+T or in the top right window on the build tab select more>test package. Test before doing commits. In the commit comments comment on the status of all the tests. Ex. "Tests:O20/F0/W0/S0". Or if you ran a devtools::check(), comment C:_/_/_
8. Load package  with devtools::load_all(".") or ctrl+shft+L
    Do this after making any changes, and before using the functions/scripts that have changed.
    This will execute ALL of the code in the package.
9. Use lintr to get guidelines on how to properly format your code. 
   `sSummaryExportFolder`.
10. Install github desktop https://desktop.github.com/ or use Git with RStudio https://support.RStudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN
Note: Raw TTC data will not be allowed into the public repository.

# If you want to both use the package and develop it!
1. Follow the instructions for both using the package and developing it in the sections above
2. Make sure you use different folders for the package itself and the project where you are 
using the package. 
3. After making changes to the package, save and close the project. Then open the project 
where you are using the package. Run the following code:
```
#make sure you use forward slashes '/' in filename not '\'
install.packages("C:/Users/path_to_package_project", repos = NULL, type = "source")
library(tirefittingr)
```
Don't forget to commit properly to github!

# How does this package work?
1. For each run name, loads the raw data into memory
2. To fit a curve, it uses a differential evolutionary algorithm which works by starting with an initial population of possible solutions. See DEOptim.
     - A solution is a set of Pacejka coefficients. For each set of coefficients the code will take the FZ, IA, and SA/SL values from each raw data point
     - And output an Fx/Fy value using the Pacejka curve. For a set of coefficients, all of the output Fy/Fx values are compared to the actual
     - Fx/Fy in the raw data. The total error for that set of coefficients (aka solution) is calculated.
     - Within the population of solutions the best solutions are mixed to create new solutions. After many rounds of selection, the best solution is
     added to the summary table along with the other runs.
3. The latest curve is then plotted. The code divides the x axis (SA/SL) into groups, and finds the most common FZ in that group then it plots the curve for that group along with the raw data points closest to the common FZ value.

