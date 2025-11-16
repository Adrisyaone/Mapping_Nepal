# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      selectInput("admin_level", "Administrative Level:",
                  choices = c("National", "Province", "District", "Municipality")),
      
      checkboxInput("subnational", "Do you want to plot subnational plot?", value = FALSE),
      
      conditionalPanel(
        condition = "input.subnational == true",
        selectInput("province_cascad", "Province", choices = c(" ","Bagmati", "Gandaki", "Karnali", "Koshi", "Lumbini", "Madesh", "Sudurpashchim") ),  # update in server
        selectInput("district_cascad", "Select District:", choices = NULL),
        selectInput("mun_type", "Type of municipality", 
                    choices = c("Development Area", "Gaunpalika", "Hunting Reserve", 
                                "Mahanagarpalika",  "Nagarpalika", "National Park", 
                                "Upamahanagarpalika", "Watershed and Wildlife Reserve",
                                "Wildlife Reserve")),
        selectInput("municipality_cascad", "Select Municipality:", choices = NULL)
      ),
      
      selectInput("map_theme", "Select Map Theme:",
                  choices = c("theme_bw", "theme_minimal", "theme_classic"), selected = "theme_bw"),
      selectInput("legend","Legend", choices = c("none", "top", "left", "right", "bottom")),
      checkboxInput("show_labels", "Label map", value = TRUE)
    ),
    
    # Main panel
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        # Tab 1: Main Map
        tabPanel(
          "Main Map",
          plotOutput("map_plot", height = "600px"),
          br(),
          downloadButton("download_map_png", "Download High-Resolution Map (png)"),
          downloadButton("download_map_pdf", "Download High-Resolution Map (pdf)")
        ),
        
        # Tab 2: Subnational Map (conditionally visible)
        tabPanel(
          "Subnational Map",
          conditionalPanel(
            condition = "input.subnational == true",
            plotOutput("map2", height = "600px"),
            br(),
            downloadButton("download_map2_png", "Download High-Resolution Map (png)"),
            downloadButton("download_map2_pdf", "Download High-Resolution Map (pdf)")
          )
        )
      )
    )
  )
)
