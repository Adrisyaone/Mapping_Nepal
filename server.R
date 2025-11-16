# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # -------------------------
  # 1. Load all Provinces
  # -------------------------
  updateSelectInput(
    session, "province",
    choices = c(" ", sort(unique(dt$PROVINCE))),
    selected=" "
  )
  
  
  # -------------------------
  # 2. District depends on Province
  # -------------------------
  observeEvent(input$province_cascad, {
    
    dist_choices <- dt |>
      dplyr::filter(PROVINCE == input$province_cascad) |>
      dplyr::pull(DISTRICT) |>
      unique() |>
      sort()
    
    updateSelectInput(
      session, "district_cascad",
      choices = c(" ",dist_choices),
      selected = " "
    )
  })
  
  # -------------------------
  # 3. Municipality depends on District
  # -------------------------
  observeEvent(input$district_cascad, {
    
    muni_choices <- dt |>
      dplyr::filter(DISTRICT == input$district_cascad ) |>
      dplyr::pull(TYPE) |>
      unique() |>
      sort()
    
    updateSelectInput(
      session, "mun_type",
      choices = c(" ", muni_choices),
      selected = " "
    )})
  
  # -------------------------
  # 3. Municipality depends on District
  # -------------------------
  observeEvent(input$district_cascad, {
    
    muni_choices <- dt |>
      dplyr::filter(DISTRICT == input$district_cascad ) |>
      dplyr::pull(LOCAL) |>
      unique() |>
      sort()
    
    updateSelectInput(
      session, "municipality_cascad",
      choices = c(" ", muni_choices),
      selected = " "
    )
  })
  
  
  
  # Reactive dataset selector
  selected_shape <- reactive({
    if (input$admin_level == "National") {
      national
    } else if (input$admin_level == "Province") {
      province
    } else if (input$admin_level == "District") {
      district
    } else if (input$admin_level == "Municipality") {
      municipality
    }
    
    
  })
  
  selected_shape2 <- reactive({
    
    # Default
    shp <- national
    
    # Case 1: Nothing selected
    if ((is.null(input$province_cascad) || input$province_cascad == " ") &&
        (is.null(input$district_cascad) || input$district_cascad == " ") &&
        (is.null(input$municipality_cascad) || input$municipality_cascad == " ")) {
      shp <- national
    }
    
    # Case 2: Only province selected
    else if (( !is.null(input$province_cascad) && input$province_cascad != " " ) &&
             (is.null(input$district_cascad) || input$district_cascad == " ") &&
             (is.null(input$municipality_cascad) || input$municipality_cascad == " ")) {
      shp <- province %>% filter(PROVINCE == input$province_cascad)
    }
    
    # Case 3: Only district selected
    else if ((is.null(input$province_cascad) || input$province_cascad == " ") &&
             ( !is.null(input$district_cascad) && input$district_cascad != " " ) &&
             (is.null(input$municipality_cascad) || input$municipality_cascad == " ")) {
      shp <- district %>% filter(DISTRICT == input$district_cascad)
    }
    
    # Case 4: Only municipality selected
    else if ((is.null(input$province_cascad) || input$province_cascad == " ") &&
             (is.null(input$district_cascad) || input$district_cascad == " ") &&
             ( !is.null(input$municipality_cascad) && input$municipality_cascad != " " )) {
      shp <- municipality %>% filter(MUNICIPALITY == input$municipality_cascad)
    }
    
    # Case 5: Province + district selected
    else if (( !is.null(input$province_cascad) && input$province_cascad != " " ) &&
             ( !is.null(input$district_cascad) && input$district_cascad != " " ) &&
             (is.null(input$municipality_cascad) || input$municipality_cascad == " ")) {
      shp <- district %>% filter(DISTRICT == input$district_cascad)
    }
    
    # Case 6: Province + municipality selected
    else if (( !is.null(input$province_cascad) && input$province_cascad != " " ) &&
             (is.null(input$district_cascad) || input$district_cascad == " ") &&
             ( !is.null(input$municipality_cascad) && input$municipality_cascad != " " )) {
      shp <- municipality %>% filter(MUNICIPALITY == input$municipality_cascad)
    }
    
    # Case 7: District + municipality selected
    else if ((is.null(input$province_cascad) || input$province_cascad == " ") &&
             ( !is.null(input$district_cascad) && input$district_cascad != " " ) &&
             ( !is.null(input$municipality_cascad) && input$municipality_cascad != " " )) {
      shp <- municipality %>% filter(MUNICIPALITY == input$municipality_cascad)
    }
    
    # Case 8: All selected
    else if (( !is.null(input$province_cascad) && input$province_cascad != " " ) &&
             ( !is.null(input$district_cascad) && input$district_cascad != " " ) &&
             ( !is.null(input$municipality_cascad) && input$municipality_cascad != " " )) {
      shp <- municipality %>% filter(MUNICIPALITY == input$municipality_cascad)
    }
    
    return(shp)
  })
  

  
  
  
  #Map1
  output$map_plot <- renderPlot({
    req(selected_shape())
    full_map(selected_shape(), input$admin_level, input$map_theme, input$legend, input$show_labels)
  })
  
  output$download_map_png <- downloadHandler(
    filename = function() {
      paste0("Nepal_Map_", input$admin_level, ".png")
    },
    content = function(file) {
      
      ggsave(
        filename = file,
        plot = full_map(selected_shape(), input$admin_level, input$map_theme, input$legend, input$show_labels),   # generate the plot
        width = 12,                          # very high resolution
        height = 10,
        units = "in",
        dpi = 600                            # publication quality
      )
    }
  )
  
  
  output$download_map_pdf <- downloadHandler(
    filename = function() {
      paste0("Nepal_Map_", input$admin_level, ".pdf")
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = full_map(selected_shape(), input$admin_level, input$map_theme, input$legend, input$show_labels),
        width = 12,
        height = 10,
        units = "in"
      )
    }
  )
  
  
  
  #Map2
  output$map2 <- renderPlot({
    req(selected_shape2())
    full_map2(selected_shape2(),  input$map_theme, input$legend, input$show_labels)
  })
  
  output$download_map2_png <- downloadHandler(
    filename = function() {
      paste0("Nepal_Map_",  ".png")
    },
    content = function(file) {
      
      ggsave(
        filename = file,
        plot = full_map2(selected_shape2(),  input$map_theme, input$legend, input$show_labels),   # generate the plot
        width = 12,                          # very high resolution
        height = 10,
        units = "in",
        dpi = 600                            # publication quality
      )
    }
  )
  
  
  output$download_map2_pdf <- downloadHandler(
    filename = function() {
      paste0("Nepal_Map_",  ".pdf")
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = full_map2(selected_shape2(), input$map_theme, input$legend, input$show_labels),
        width = 12,
        height = 10,
        units = "in"
      )
    }
  )
  
  
  
}



