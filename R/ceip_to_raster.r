#' Converts text based data from the
#' Centre on Emission Inventories and Projections (CEIP)
#' into raster grid data (geotiff or raster object in the R workspace)
#'
#' @param df CEIP data frame generated by ceip_read()
#' @param year which years to cover (default = 2000:2016)
#' @param pollutant emission values to include (default = "NOx")
#' @param sector sector to visualize
#' @param country country to visualize, if NULL all (default = \code{NULL})
#' @param out_dir directory where to store the geotiff if written to file
#' (default = tempdir())
#' @param trim shrink map to the area covered by data (default = \code{TRUE})
#' @param internal bolean \code{TRUE} / \code{FALSE}, write output to disk or
#' return raster stack to R workspace
#' @return Returns a geotiff written to disk or raster stack returned
#' to the R workspace when written to disk the out_dir location is used.
#' @keywords emission, voc
#' @export
#' @examples
#'
#' \dontrun{
#' ceip_grid()
#'}
#' @importFrom magrittr %>%

ceip_to_raster <- function(df,
                      year = 2000:2016,
                      pollutant = "SOx",
                      sector = "A",
                      country = NULL,
                      out_dir = tempdir(),
                      trim = TRUE,
                      internal = TRUE){

  # check if the data class is correct
  if(!any(class(df) == "ceipr_data")){
    stop("Data is not of class ceipr_data, not valid ceipr data!")
  }

  # convert the data from a dataframe to a raster
  # for a subset of the data
  rasters <- year %>% map(function(y)
      filter(df,
             if (!is.null(country)) {
               iso2 == !!country &
                 year == !!y &
                 pollutant == !!pollutant &
                 sector_abbr == !!sector
             } else {
               year == !!y &
                 pollutant == !!pollutant &
                 sector_abbr == !!sector
             }) %>%
        ceipr::convert_to_raster())

  # combine all data into a stack
  rasters <- raster::stack(rasters)

  # check if there is data in the resulting
  # raster stack if not bail
  if (nlayers(rasters) == 0){
    stop("Data subset is empty, check your input parameters and data file!")
  }

  # if required trim the raster
  # shrink extent to data coverage
  if (trim){
    rasters <- raster::trim(rasters)
  }

  # if internal return the raster stack
  # otherwise write to file
  if(internal){
    return(rasters)
  } else {
    raster::writeRaster(rasters,
                        paste0(out_dir,'/',pollutant,'_',sector,'.tif'),
                        overwrite = TRUE)
  }
}
