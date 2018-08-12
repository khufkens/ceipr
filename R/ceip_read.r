#' Load CEIP zipped data into a tidy data frame
#'
#' Load data for specified pollutant(s), for specified sector(s) and all years from
#' a directory of zip files, or an online path. For the latter option your,
#' milage might vary and it is recommended to download files of interest first
#' using the ceip_download() function as it is less likely to cause time-out
#' issues and will limit overall traffic to the server.
#'
#' @param sector CEIP sectors (default is all sectors from A to M)
#' @param pollutant which pollutant to include (use "ALL" for all pollutantslet or "NT" for National Totals)
#' @param year which years to include in the data compilation
#' @param path path with original CEIP zip files
#' @param country ISO2 country code (two letters)
#' @return Returns a data frame (tibble) of CEIP data. This data is tidy
#' and can be easily used in statistical analysis. Or converted to geospatial
#' data using included functions.
#' @export

ceip_read <- function(pollutant = "NOx",
                      sector = toupper(letters[1:13]),
                      year = 2000:2016,
                      country = NULL,
                      path = "~/Desktop/tmp/") {

  # list zip files in path
  zip_files <- list.files(paste0(path, '/' ,year),"*.zip",
                          recursive = TRUE,
                          full.names = TRUE)
  if(!pollutant == "ALL") {
    # subset based upon pollutant
    zip_files <- zip_files[grep(pollutant,
                                basename(zip_files))]
  } else {
    pollutant <- ceip_sector_meta_data()$abbreviation
  }

  # trap errors if no files are detected
  if(length(zip_files) == 0){
    stop("No files retained in query, check path for correct file locations
         and the parameters specified !")
  }

  # Parse directly from zip file using code from
  # Thomas Goorden (helper functions), by default
  # all years are automatically included. Selection
  # of the years is done in the ceip_download() function.
  dplyr::bind_rows(purrr::map(zip_files, function(z) {
    dplyr::bind_rows(purrr::map(pollutant, function(p) {
      dplyr::bind_rows(purrr::map(sector, function(s) {

        # extract year from zip file
        filename <- tools::file_path_sans_ext(basename(z))
        y <- rev(unlist(strsplit(filename,"_")))[1]

        # read in data using all necessary specifies
        # query data directly from zip file
        df <- try(ceip_read_zip(z, ceip_data_file(y,p,s)))


        # trap import errors, mainly corrupted
        # zip files, file will be skipped
        if(inherits(df, "try-error")){
          message(paste("Import failed for:", z))
          message("The file will be skipped!")
          return(NULL)
        }

        # to limit the amount of data returned as much as possible:
        if(!is.null(country)) {
          df <- dplyr::filter(df,iso2 %in% country)
            filter(df,iso2==country)
        }

        # assign clean sector label (abbreviated)
        df$sector_abbr <- s

        # add a custom ceipr class for data
        # identification (sanity checks)
        class(df) <- c(class(df),"ceipr_data")

        # return data frame to be collated
        # using bind_rows()
        return(df)
      }))
    }))
  }))
}
