#' Formats the name of the CEIP zip file location
#'
#' location is either remote or local as defined
#' by the path variable
#'
#' @param path CEIP sectors
#' @param year year to download
#' @param pollutant different pollutants
#' @return zip file location string
#' @export
ceip_zip_file <- function(path, year, pollutant) {
  paste0(path,'/',year,'/',pollutant,'_2018_GRID_',year,'.zip')
}

# Internal singleton
sector_meta_data <- NULL

#' Sector meta data (code, name, etc)
#' @return sector meta data
#' @export
ceip_sector_meta_data <- function() {
   if (is.null(sector_meta_data)) {
     csv <- utils::read.table(sprintf("%s/inst/extdata/ceip_meta_data.csv",
                                               path.package("ceipr")),
                                       sep = ",",
                                       header = TRUE,
                                       stringsAsFactors = FALSE)
     sector_meta_data <- csv$abbreviation
     names(sector_meta_data) <- csv$sector
   }
  return(sector_meta_data)
}

#' Formats the name of the CEIP file inside the zip file
#'
#' @param sector CEIP sectors
#' @param pollutant different pollutants
#' @param year year to download
#' @return url location
#' @export
ceip_data_file <- function(year, pollutant, sector) {
  meta_data <- ceip_sector_meta_data()
  # create data file string
  data_file <- paste0(pollutant,'_',sector,'_',
        gsub(" ", "", names(which(ceipr::ceip_sector_meta_data() == sector))),
        '_2018_GRID_',year,'.txt')

  # if sectior is "NT" (National Total) strip long form name
  if(sector == "NT"){
    return(gsub("_NationalTotal","",data_file))
  } else {
    return(data_file)
  }
}

#' Reads internal file from a zip file (unzip in memory)
#'
#' @param zip location of a zip file (url / or local path)
#' @param filename filename in the zip file to extract
#' @return reads a specific file
#' @export
ceip_read_zip <- function(zip, filename) {

  # read_delim() settings
  # column names of the ceip files
  csv_column_names <- c("iso2", "year", "sector",
                        "pollutant", "longitude",
                        "latitude", "unit", "emission")

  # column formats of the ceip files
  csv_col_types <- readr::cols(
    iso2 = readr::col_character(),
    year = readr::col_integer(),
    sector = readr::col_character(),
    pollutant = readr::col_character(),
    longitude = readr::col_number(),
    latitude = readr::col_number(),
    unit = readr::col_character(),
    emission = readr::col_double()
  )

  # comment, delimitir and locale settigns
  delim <- ";"
  comment <- "#"
  us_locale <- readr::locale(decimal_mark = '.',
                      grouping_mark = ',')

  # read in thet data directly from zip file
  return(
    readr::read_delim(
      unz(zip, filename),
      comment = comment,
      col_names = csv_column_names,
      col_types = csv_col_types,
      delim = delim,
      locale = us_locale
    )
  )
  closeAllConnections() # explicitely closed to avoid warnings due to time-out
}

#' converts ceip subset data frame to a raster
#'
#' Never to be used stand alone, only as a helper function
#'
#' @param df subset of a full ceip data frame as returned by ceip_read()
#' @return raster layer
#' @export
#'
convert_to_raster <- function(df) {

  # create the grid as defined here:
  # http://webdab1.umweltbundesamt.at/download/01GridData/
  # EMEP_gridding_system_documentation.pdf

  # first create an arbitrary WGS84 grid
  r <- raster::raster(ncols = 1200, nrows = 520)

  # reassign the extent
  raster::extent(r) <- c(-30,90,30,82)

  # if no data is provided, return an empty
  # raster (helpful for failed reads etc)
  # else fill the raster with values
  if(nrow(df)==0 | is.null(df)){
    return(r)
  } else {

  # assign coordinates to create a spatial data frame
  sp::coordinates(df) <- ~longitude+latitude

  # fill with the corresponding values
  # from the spatial data frame (rasterize)
  r <- raster::rasterize(df, r, field = "emission")
  return(r)
  }
}

#' Helper function to slice and dice a map and convert it to an sp() object
#'
#' @param regions list of regions (countries to convert)
#' @param xlim longitude bounding box values
#' @param ylim latitude bounding box values
#' @param length_out segment output length
#' @param clip bolean TRUE / FALSE, clip polygons on bounding box
#' @return an spatialPolygon object which can be used for subsetting, and
#' the bounding box constraining the object
#' @keywords emission, voc, polygon, subsetting
#' @export
maps_to_sp = function(regions = "Belgium",
                      xlim = c(2.4, 6.5),
                      ylim = c(49, 52),
                      length_out = 100,
                      clip = TRUE) {

  m = maps::map(database = "worldHires",
                regions = regions,
                xlim = xlim,
                ylim = ylim,
                plot = FALSE,
                fill = TRUE)

  LL = sp::CRS("+init=epsg:4326")
  IDs <- sapply(strsplit(m$names, ":"), function(x) x[1])
  m = maptools::map2SpatialPolygons(m, IDs=IDs, proj4string = LL)
  m = rgeos::gBuffer(m, byid=TRUE, width=0)

  p = rbind(cbind(xlim[1], seq(ylim[1],ylim[2],length.out = length_out)),
            cbind(seq(xlim[1],xlim[2],length.out = length_out),ylim[2]),
            cbind(xlim[2],seq(ylim[2],ylim[1],length.out = length_out)),
            cbind(seq(xlim[2],xlim[1],length.out = length_out),ylim[1]))
  bb = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(list(p))),"bb")),
                           proj4string = LL)

  if (!clip)
    return(list(m,bb))
  else {
    m = rgeos::gIntersection(m, bb)
    return(list(m, bb))
  }
}
