
#' Add eurostat population numbers to CEIP data frame
#'
#' @param df CEIP data frame generated by ceip_read() (of class ceipr_data)
#' @return Merges the two databases
#' @keywords emission, voc, population
#' @export

ceip_add_population <- function(df) {

  # check if the data class is correct
  if(!any(class(df) == "ceipr_data")){
    stop("Data is not of class ceipr_data, not valid ceipr data!")
  }

  # Load the data set for EU population data
  # Warning: only available from 2007!
  EU_population_data <- eurostat::get_eurostat("tps00001")

  # convert the timestamp to a year, for better referencing
  EU_population_data$year <- with(EU_population_data,
                                  strtoi(format(time,'%Y')))

  EU_population_data <- dplyr::rename(EU_population_data,
                                      population = values,
                                      iso2 = geo)

  EU_population_data %>%
    count(iso2,year) %>%
    filter(n > 1)

  return(
    dplyr::left_join(df, EU_population_data, by = c("iso2","year"))
  )
}

#' Calculate per capita emissions
#'
#' @param df CEIP data frame generated by ceip_read() (of class ceipr_data)
#' and supplemented with eurostat data using ceip_add_population()
#' @return Merges the two databasesde
#' @keywords emission, voc, population
#' @export

ceip_population_emissions <- function(df) {

  # check if the data class is correct
  if(!any(class(df) == "ceipr_data")){
    stop("Data is not of class ceipr_data, not valid ceipr data!")
  }

  # check if the eurostat data is included
  if(!any(grepl("population", names(df)))){
    stop("No population data found in the ceipr data frame.
         Use the ceip_add_population() function to merge in eurostat data.")
  }

  # calculate the emissions per capita
  ceipr$population_emission <- with(df, emission/population)
}
