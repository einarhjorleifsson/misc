# usage, see: scripts/2022-05_fao-halli.R

#' Merge data with FAO or EEZ shapes and visualize
#'
#' @param data A tibble containing variable area, either FAO major zone code or country EEZ iso3a code.
#' @param shape Shapefile, either EEZ or FAO major zone polygons. The code name for each should be a named "area".
#' @param value The name of the variable to be plotted.
#' @param wrap The name of the variable to be wrapped. If missing, data by area will
#' be summed before plot is generated.
#'
#' @return A ggplot
#' @export
#'
baseplot <- function(data, shape, value = n, wrap = zone) {

  data <-
    data %>%
    rename(v = {{ value }},
           # CHECK: why does this pass error check if no wrap passed in arguement
           w = {{ wrap }})
  #return(data)

  #}

  # If no wrap specified, could here sum the values before a left join
  if(missing(wrap)) {
    data <-
      data %>%
      group_by(area) %>%
      summarise(v = sum(v, na.rm = TRUE),
                .groups = "drop")
  }


  suppressMessages(
    data <-
      shape %>%
      filter(area %in% data$area) %>%
      left_join(data)
  )

  bb <- data %>% st_bbox()

  suppressWarnings(
    shape2 <-
      data %>%
      st_crop(bb) %>%
      st_cast("MULTIPOLYGON")
  )


  p <-
    ggplot() +
    theme_bw() +
    geom_sf(data = shape2,
            colour = "grey", alpha = 0) +
    geom_sf(data = data,
            aes(fill = factor(v))) +
    geom_sf(data = countries) +
    scale_fill_viridis_d() +
    coord_sf(xlim = c(bb[1], bb[3]), ylim = c(bb[2], bb[4]))

  if(!missing(wrap)) {

    p <-
      p +
      facet_wrap(~ w)

  }

  return(p)

}
