#' Load CEIP zipped data into a tidy data frame
#'
#' Load data for specified pollutant(s), for specified sector(s) and all years from
#' a directory of zip files, or an online path. For the latter option your,
#' milage might vary and it is recommended to download files of interest first
#' using the ceip_download() function as it is less likely to cause time-out
#' issues and will limit overall traffic to the server.
#'
#' @param sector CEIP sectors (default is all sectors from A to M)
#' @param pollutant which pollutant to include (use "ALL" for all pollutants
#' or "NT" for National Totals)
#' @param year which years to include in the data compilation
#' @param path path with original CEIP zip files (default = tempdir())
#' @param country ISO2 country code (two letters), the euro zone 18 can be
#' downloaded with the following string c("AT","BE","CY","EE","FI", "FR","DE",
#' "GR","IE","IT","LT","LV","LU","NL","PT","SK","SI","ES") (default = NULL)
#' @return Returns a data frame (tibble) of CEIP data. This data is tidy
#' and can be easily used in statistical analysis. Or converted to geospatial
#' data using included functions.
#' @export

ceip_read <- function(pollutant = "NOx",
                      sector = c(LETTERS[1:13],"NT"),
                      year = 2000:2016,
                      country = NULL,
                      path = tempdir()) {

  if(pollutant == "ALL") {
    pollutant <- ceip_sector_meta_data()
  }

  # progress bar:
  total_iterations <- length(year) * length(pollutant) * length(sector)
  pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)
  iteration <- 0

  # Parse directly from zip file. By default
  # all years are automatically included. Selection
  # of the years is done in the ceip_download() function.
  return (
    dplyr::bind_rows(purrr::map(year, function(y) {
      dplyr::bind_rows(purrr::map(pollutant, function(p) {
        # list zip files in path
        zip_files <- list.files(paste0(path, '/' ,y),"*.zip",
                                recursive = TRUE,
                                full.names = TRUE)
        # subset based upon pollutant
        zip_files <- zip_files[grep(p,basename(zip_files))]

        # trap errors if no files are detected
        if(length(zip_files) > 1){
          stop(glue::glue("More than one zip file found for pollutant {p} in year {y}: {zip_files}"))
        }
        if (length(zip_files) == 0) {
          message(glue::glue("No zip file found for {p} in year {y}, skipping."))
          return(NULL)
        } else {
          z <- dplyr::first(zip_files)
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
            }

            if (nrow(df) == 0) {
              # warning(paste("No data found",z,p,s,sep = " - "))
              return(NULL)
            }

            # assign clean sector label (abbreviated)
            df$sector_abbr <- s

            # add a custom ceipr class for data
            # identification (sanity checks)
            class(df) <- c(class(df),"ceipr_data")

            # free up memory
            gc()
            # update progress bar
            iteration <- iteration + 1
            setTxtProgressBar(pb, iteration)

            # return data frame to be collated
            # using bind_rows()
            return(df)


          }))
        }
      }))
    }))
  )
  close(pb)
}
