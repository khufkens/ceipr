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
