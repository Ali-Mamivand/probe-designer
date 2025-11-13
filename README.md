# HCR v3 & smFISH Probe Designer

A Shiny web application for designing HCR v3 (Hybridization Chain Reaction) and smFISH (single molecule FISH).


## Installation

### Required R Packages
```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "ggplot2",
  "plotly",
  "httr",
  "jsonlite"
))
```

### Run Locally
```r
# Clone the repository
git clone https://github.com/Ali.Mamivand/probe-designer.git
cd probe-designer

# Run the app
shiny::runApp()
```
### Online Version (No Installation Required)

**Try it now:** [https://alim7227.shinyapps.io/HCR-probe-designer_b/](https://alim7227.shinyapps.io/HCR-probe-designer_b/)


## Acknowledgments

This application was developed using knowledge and resources from the **FISHing for RNAs: Classical to Single Molecule Approaches** course held at **EMBL Heidelberg**.
Special thanks to **Imre Gaspar** for his invaluable guidance and for sharing the original smFISH probe design script, as well as to all **EMBO RNAFishing** course instructors and organizers for their support and inspiration.
  
## Contact

**Ali Mamivand**  
📧 ali.mamivand72@gmail.com
