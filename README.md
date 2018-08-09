# Introduction

Interface Centre on Emission Inventories and Projections (CEIP) data to visualize emission contributions by sector across Europe. All data is sourced from the open CEIP website (www.ceip.at, CEIP 2018). A number of functions allows you to easily download, visualize and analyze the CEIP data.

The CEIP data covers data on a number of atmospheric pollutant emissions (CO, NH3, NMVOC, NOx, PM2_5, PM10, PMcoarse, SOx) across various societal sectors. During analysis sectors are referenced using their abbreviation.

| Sector Abbreviation | Sector Description |
|:--------------------|:-------------------|
| A | Public Power |
| B | Industry |
| C | Other Stationary Combustion |
| D | Fugitive |
| E | Solvent |
| F | Road Transport |
| G | Shipping |
| H | Aviation |
| I | Offroad |
| J | Waste |
| K | Agriculture Livestock |
| M | Agriculture Other |
| N | Other |
| NT | Not Assigned |

## Installation

To install the toolbox in R run the following commands in a R terminal

```r
if(!require(devtools)){install.packages(devtools)}
devtools::install_github("khufkens/ceipr")
library(ceipr)
```

## Use

### Downloading data

For downloading use the download_ceip() function. This function downloads and compiles all CEIP data for a given pollutant, and industry class across all avialable years into a single geotiff file. The format of the output geotiff file is "pollutant"_"sector-abbreviation".tif (e.g. NOx_A.tif)

```r
ceip_download(pollutants = "NOx",
              sector = "A",
              out_dir = "/your/output/directory/")
```

### Analyzing trends

You can analyze trends in the emission values using an ordinary linear regression using the ceip_regression() function which returns, the slope, R<sup>2</sup> value, and p-value. The data is the geotiff as generated by the ceip_download() function.

```r
ceip_regression(file = "/your/output/directory/NOx_A.tif")
```

### Animate data

TODO



