witch-plot - A library of (static and dynamic) ploting tools for the WITCH model and DICE/RICE model series.

Started in 2014 by Johannes Emmerling to have quick and reproducible sets of graphs for diagnostics, inspection, and publication.

## Requirements

1) **Installation:** Install R, Rtools, RStudio, GAMS, Github Desktop, (and optionally VSCode as advanced editor)

* R from https://cran.r-project.org/bin/windows/base/
* Rtools from https://cran.r-project.org/bin/windows/Rtools/
* RStudio from https://rstudio.com/products/rstudio/download/#download
* GAMS from https://www.gams.com/download/ (Run the installer in advanced mode and mark the check-box `Add GAMS directory to PATH environment variable`).
* GitHub Desktop from https://desktop.github.com/ and log-in with your personal GitHub Account.
* VisualStudio Code from https://code.visualstudio.com/ (optional)

## Installation and running the program

Get the source code either cloning it in Github desktop (preferred), or download it from https://github.com/witch-team/witch-plot, or using git at the command line.

To run the program, open teh folder "witch-plot" in Rstudio as a project or execute on the command line
```Shell
Rscript plotgdx_[witch|rice].R
```

The script will automatically search all results_*.gdx files in the specified folder in the second line of the script and include all in the analysis and plots.

## Main Functions

### get_witch_simple("varname")

Loads the variable "varname" from all results GDX files and stores them in a data.frame names "varname".

### gdxcompaR
ShinyApp based dynamic comparison tool for multiple results files.
Simplyu run 'runApp(appDir = "gdxcompaR/witch")' or runApp(appDir = "gdxcompaR/witch") to launch the interactive App after sourcing the main file 'plotgdx-witch.R' or 'plotgdx-rice.R'.



