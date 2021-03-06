#' Animate a raster stack of Centre on Emission Inventories
#' and Projections (CEIP) data into an animated gif for terrestrial locations.
#'
#' @param file CEIP txt data file
#' @param output_file path to a gif file to store the final animation
#' @param region which regions to consider (default = "Belgium")
#' @param width output width in px (default = 800)
#' @param height output height in px (default = 800)
#' @param xlim longitudinal geographic limits, override the regions argument
#' (default = c())
#' @param ylim latitudinal geographic limits, override the regions argument
#' (default = c())
#' @param time_step pause in seconds between two gif frames
#' @param upper_limit maximum value of scale (default = 10)
#' @return geotiff written to disk or raster data returned to R workspace
#' when written to disk the location of the txt file is used
#' @keywords emission, voc, plot, animation
#' @export

ceip_animation <- function(file = "~/SOx_A.tif",
                           output_file = "~/test.gif",
                           region = "belgium",
                           width = 1200,
                           height = 1200,
                           xlim = c(-30,90),
                           ylim = c(30,82),
                           upper_limit = 15000,
                           time_step = 0.5){

  # set animation options
  animation::ani.options(interval = time_step,
              width = width,
              height = height)

  # grab meta-data from filename
  # TODO file checks
  file_info <- unlist(strsplit(tools::file_path_sans_ext(basename(file)), "_"))

  # read in meta data
  meta_data <- utils::read.table(sprintf("%s/extdata/ceip_meta_data.csv",
                                  path.package("ceipr")),
                          sep = ",",
                          header = TRUE,
                          stringsAsFactors = FALSE)

  # read in the data
  map_data <- raster::brick(file)

  # define outline
  outline <- try(maps_to_sp(regions = region,
                        xlim = xlim,
                        ylim = ylim)[[1]],
                 silent = TRUE)

  if(is.null(region)){
    outline <- maps_to_sp(regions = ".",
                              xlim = xlim,
                              ylim = ylim,
                              clip = TRUE)[[1]]
    map_data <- raster::crop(map_data,
                     raster::extent(c(xlim, ylim)))
    proj <- "mercator"
  } else {
    map_data <- raster::mask(map_data, outline)
    proj <- "stereographic"
  }

  # theme and map adjusted from Timo Grossenbacher / @grssnbchr
  theme_map <- function(...) {
    ggthemes::theme_minimal() +
      ggplot2::theme(
        text = ggplot2::element_text(family = "Arial", color = "#22211d"),
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#ebebe5", size = 0.2),
        panel.grid.minor = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        legend.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
        panel.border = ggplot2::element_blank(),
        ...
      )
  }

  # define a colour palette to be used, spectral sucks but is the only
  # thing that can represent these highly non linear data well (even after
  # a sqrt transform)
  myPalette <- grDevices::colorRampPalette(
    rev(RColorBrewer::brewer.pal(11, "Spectral")))

  animation::saveGIF(movie.name = output_file, expr = {
    for (i in 1:raster::nlayers(map_data)){

      # convert to data frame for ggplot
      map_data_df <- as.data.frame(methods::as(raster::subset(map_data,i),
                                      "SpatialPixelsDataFrame"))
      colnames(map_data_df) <- c("value", "x", "y")

      # plotting routine, building plot step by step
      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = outline, ggplot2::aes(x = long, y = lat, group = group),
                     fill = "grey",
                     col = NA) +
        ggplot2::geom_tile(data=map_data_df, ggplot2::aes(x=x, y=y, fill=value), alpha = 1) +
        ggplot2::geom_polygon(data = outline, ggplot2::aes(x=long, y = lat, group = group),
                     fill = NA,
                     col = "white",
                     lwd = 0.5) +
        ggplot2::coord_equal() +
        theme_map() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::labs(x = NULL,
                      y = NULL,
                      title = paste0(file_info[1], " emissions"),
                      subtitle = paste0(meta_data$sector[which(meta_data$abbreviation == file_info[2])],
                               " values for ", 2000 + i),
             caption = "Data: Centre on Emission Inventories and Projections - graphics by @koen_hufkens") +
        ggplot2::scale_fill_gradientn(
          colours = myPalette(100),
          name = paste0("sqrt(", meta_data$unit[which(meta_data$abbreviation == file_info[2])],")"),
          #discrete = FALSE,
          limits = c(0, upper_limit),
          #trans = "sqrt",
          guide = ggplot2::guide_colourbar(
            direction = "horizontal",
            barwidth = unit(0.5, units = "npc"),
            keyheight = unit(2, units = "mm"),
            keywidth = unit(50, units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
            byrow = T,
            label.position = "bottom"
          )
        ) +
        ggplot2::coord_map(proj)
      graphics::plot(p)
    }
  })
}
