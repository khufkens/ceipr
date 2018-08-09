#' Converts text based data from the
#' Centre on Emission Inventories and Projections (CEIP)
#' into raster data (geotiff or raster object in the R workspace)
#'
#' @param file CEIP txt data file
#' @param out_dir output directory where to store data when writing to disk
#' @param internal bolean TRUE / FALSE, write output to disk or return to
#' R workspace
#' @return geotiff written to disk or raster data returned to R workspace
#' @keywords emission, voc
#' @export
#' @examples
#'
#' \dontrun{
#' ceip_grid()
#'}

ceip_grid <- function(file,
                      out_dir = tempdir(),
                      internal = FALSE){

  # read in data
  df <- utils::read.table(file,
                          sep = ";",
                          header = FALSE,
                          stringsAsFactors = FALSE)

  # Add variable names (hopefully consistent across
  # all files)
  names(df) <-  c("iso2", "year", "sector",
                  "pollutant", "longitude",
                  "latitude", "unit", "emission")

  # convert to spatial data frame
  sp::coordinates(df) <- ~longitude+latitude

  # create the grid as defined here:
  # http://webdab1.umweltbundesamt.at/download/01GridData/\
  # EMEP_gridding_system_documentation.pdf

  # first create an arbitrary WGS84 grid
  r <- raster::raster(ncols = 1200, nrows = 520)

  # reassign the extent
  raster::extent(r) <- c(-30,90,30,82)

  # fill with the corresponding values
  # from the spatial data frame (rasterize)
  r <- raster::rasterize(df, r, field = "emission")

  if (internal){
    return(r)
  } else {
    raster::writeRaster(r,
                        paste0(tools::file_path_sans_ext(file),".tif"),
                        overwrite = TRUE,
                        options = "COMPRESS=DEFLATE")
  }
}
