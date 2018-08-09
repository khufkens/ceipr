#' Download and compile emission data from
#' the Centre on Emission Inventories and Projections (CEIP)
#'
#' @param pollutants character string with pollutants
#' (CO, NH3, NMVOC, NOx, PM2_5, PM10, PMcoarse, SOx)
#' @param sectors different societal sectors for which to compile the data
#' as referenced by their abbreviation (A - N & NT)
#' @param out_dir output directory
#' @param base_url source url to download data from
#' @return geotiff written to disk
#' @keywords emission, voc
#' @export
#' @examples
#'
#' \dontrun{
#' ceip_download()
#'}

ceip_download <- function(
  pollutants = c("SOx"),
  sectors = "A", #c(LETTERS[1:13],"NT"),
  out_dir = "~",
  base_url = "http://webdab1.umweltbundesamt.at/download/gridding2018/"){

  # clean tempdir of stray txt files
  # as tempdir content persists throught an R session
  # [warning: might fuck up some other stuff]
  file.remove(list.files(tempdir(),"*.txt", full.names = TRUE))
  file.remove(list.files(tempdir(),"*.tif", full.names = TRUE))

  # split out year directories from download path
  # dynamically coded to adjust to new data automatically
  # could be static for shorter codebase
  years <- xml2::read_html(base_url)
  years <- data.frame(rvest::html_table(years))$Name
  years <- as.numeric(gsub("/",
                           "",
                           years[grep("^[0-9]",years)]))

  # list all ceip files in your download path
  files <- list.files(tempdir(), "*.txt", full.names = TRUE)

  # convert everything to geotiff files
  invisible(lapply(pollutants, function(pollutant){
    lapply(years, function(year){

      # get the zipfile for a particular pollutant
      zip_files <- xml2::read_html(paste(base_url, year, sep = "/"))
      zip_files <- data.frame(rvest::html_table(zip_files))$Name
      zip_file <- zip_files[grep(paste0(pollutant, ".*.zip"), zip_files)]

      # download and unzip the zipfile
      utils::download.file(url = paste(base_url, year, zip_file, sep = "/"),
                    destfile = file.path(tempdir(), "tmp.zip"),
                    quiet = TRUE)

      # check if the file was create if not raise a warning
      if(!file.exists(file.path(tempdir(), "tmp.zip"))){
        warning(paste0("No data available for pollutant: ",
                       pollutant,
                       " and year: ",
                       year,
                       " results will be unreliable!"))
        return(NULL)
      }

      # if exist, continue to unzip etc
      utils::unzip(file.path(tempdir(), "tmp.zip"), exdir = tempdir())

      # convert only required data (sectors) to geotiffs
      txt_files <- list.files(tempdir(), paste0(pollutant,".*.txt"),
                              full.names = TRUE)
      txt_files <- txt_files[grep(paste(paste0("_",sectors,"_"),
                                        collapse = "|"),
                                  txt_files)]

      lapply(txt_files, function(txt_file){
        try(ceip_grid(file = txt_file,
                             internal = FALSE))
      })

      # cleanup
      file.remove(txt_files)
    })

    # by pollutant and sector, get all files
    # and put into raster stack
    lapply(sectors, function(sector){

      # list tif files by sector (sorted by year by default)
      tif_files <- list.files(tempdir(),paste0("^.*_",sector,"_.*\\.tif$"),
                              full.names = TRUE)

      # write stack to file
      raster::writeRaster(raster::stack(tif_files),
                  file.path(out_dir, paste0(pollutant,"_",sector,".tif")),
                  overwrite = TRUE)

      # cleanup
      file.remove(tif_files)
    })
  }))
}
