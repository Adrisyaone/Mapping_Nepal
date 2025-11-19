# Full map of all national, subnational levels of Nepal
full_map <- function(shape_data, selected_opt, map_theme, legend, show_labels) {
  
  p <- ggplot() +
    geom_sf(data = shape_data, aes(fill = !!sym(toupper(selected_opt))))
  
  # ---- Add Labels If show_labels=TRUE ----
  if (isTRUE(show_labels)) {
    p <- p +
      geom_sf_text(
        data = shape_data,
        aes(label = !!sym(toupper(selected_opt))),
        size = 3,
        color = "black"
      )
  }
  
  # ---- Add Other Map Elements ----
  p <- p +
    xlab("Longitude") + 
    ylab("Latitude") +
    annotation_scale(location = "bl", width_hint = 0.3) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      pad_x = unit(0.75, "in"), 
      pad_y = unit(0.5, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    do.call(map_theme, list()) +  
    theme(legend.position = legend)
  
  return(p)
}


# MAP2
# Full map of all national, subnational levels of Nepal
full_map2 <- function(shape_data,  map_theme, legend, show_labels) { 
  
  
  p <- ggplot() +
    geom_sf(data = shape_data)
  

  
  
  # ---- Add Labels If TRUE ----
  if (isTRUE(show_labels)) {
    p <- p +
      geom_sf_text(
        data = shape_data,
        aes(label = "label"),
        size = 3,
        color = "black"
      )
  }
  

  
  # ---- Add Other Map Elements ----
  p <- p +
    xlab("Longitude") + 
    ylab("Latitude") +
    annotation_scale(location = "bl", width_hint = 0.3) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      pad_x = unit(0.75, "in"), 
      pad_y = unit(0.5, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    do.call(map_theme, list()) +   # apply selected theme
    theme(legend.position = legend)
  
  return(p)
}



# , layover, pr_name, dist_name, mun_name, pro_map, dist_map, mun_map


