#' Download zipped Centre on Emission Inventories and Projections (CEIP) data
#'
#' The download routine will recreate the CEIP file structure in the output
#' directory (sorting files in folders by year).
#'
#' @param pollutant character string with pollutants
#' (CO, NH3, NMVOC, NOx, PM2_5, PM10, PMcoarse, SOx)
#' @param year years to download (all = 2000:2016)
#' @param out_dir output directory (default = tempdir())
#' @return Downloads ceip zip files into the output directory. CRAN policy does
#' not allow default settings to write to the userspace so tempdir() is the
#' default. However, a different directory can be used for caching.
#' @keywords emission, voc
#' @export
#' @examples
#'
#' \dontrun{
#' ceip_download(pollutant = "SOx",
#'               year = 2000)
#'}

ceip_download <- function(
  pollutant = c("SOx", "NOx"),
  year = 2000:2016,
  out_dir = tempdir()){

  # set the base url of the ceip server
  # might change in the future
  base_url = "http://webdab1.umweltbundesamt.at/download/gridding2018/"

  # check if output directory exists
  if(!dir.exists(out_dir)){
    stop("Specified output directory does not exist, check out_dir parameter!")
  }

  # suppress (empty) output
  invisible(
    purrr::map(year,function(y) {
      purrr::map(pollutant, function(p) {

        # format input file
        file <- ceipr::ceip_zip_file(path = base_url,
                      year = y,
                      pollutant = p)

        # format file destination
        dest_file <- file.path(out_dir, y, basename(file))

        # create directory structure if it does not exist
        if(!dir.exists(dirname(dest_file))){
          dir.create(dirname(dest_file))
        }

        # some feedback
        message(paste("Downloading:", basename(file)))

        # download a particular fiel
        status <- httr::GET(url = file,
                               httr::write_disk(path = dest_file,
                                                overwrite = TRUE),
                               httr::progress())

        # error / stop on 400 error
        if(httr::http_error(status)){
          warning(paste("Requested file:",
                          basename(file),
                          "is not found. Check connection or parameters!"))
        }
      })
  }))
}
