```diff
- This project is very much in flux... use with caution, things might break!
```

## Introduction

Interface Centre on Emission Inventories and Projections (CEIP) data to visualize emission contributions by sector across Europe. All data is sourced from the open CEIP website (www.ceip.at, CEIP 2018). A number of functions allows you to easily download, visualize and analyze the CEIP data. The package is a formalized version of the [CEIP analysis by Thomas Goorden](https://github.com/tgoorden/ceipr-report).

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
| K | Agriculture - Livestock |
| l | Agriculture - Other |
| M | Other |
| NT | National Total |

## Installation

To install the toolbox in R run the following commands in a R terminal

```r
if(!require(devtools)){install.packages("devtools")}
devtools::install_github("khufkens/ceipr")
library(ceipr)
```

## Use

### Downloading data

For downloading use the download_ceip() function. This function downloads all CEIP zipfiles into the a copy of the original file structure, with zipfiles sorted for by year.

```r
ceip_download(pollutants = "NOx",
              sector = "A",
              year = 2000,
              out_dir = "~")
```

### Reading local data

Read in data from downloaded zip files for all pollutants, sectors and years. The data will be stored into a dataframe df.

```r
df <- ceip_read(pollutant = "ALL",
                sector = "ALL",
                path = "~")
```

### Calculating and visualizing (country) statistics

TODO

### Converting to raster

Data can be converted to a raster format, either a geotiff or a local raster stack in the R workspace using the ceip_to_raster() function. By default all available years for a given pollutant and sector are combined in a single raster stack. A two letter country code (or string of those) can be used together with the 'country' parameter to restrict the data geographically to the specified country. In the example below data is restricted geographically to Belgium and France.

```r
raster_stack <- ceip_to_raster(df, 
                               pollutant = "NOx",
                               sector = "A",
                               country =c("BE","FR"),
                               internal = TRUE)

# write data to file on disk                               
ceip_to_raster(df, 
               pollutant = "NOx",
               sector = "A",
               country =c("BE","FR"),
               internal = TRUE)
```



