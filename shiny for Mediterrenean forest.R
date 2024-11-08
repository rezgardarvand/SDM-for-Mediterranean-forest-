# نصب و بارگذاری بسته‌های لازم
library(shiny)
library(leaflet)
library(terra) # برای کار با فایل‌های رستر چند لایه‌ای

# بارگذاری فایل چند لایه‌ای شامل مدل توزیع گونه‌ها
 # فایل را به صورت SpatRaster چند لایه‌ای بارگذاری کنید
setwd("D:/uni/thesis/doctoral thesis/proposal for mediterranean north of Iran/GEDI/analysis/result analysis based community/prediction/sdm/")
list_files <- list.files(pattern = ".tif",recursive = TRUE, full.names = TRUE)
GEDI <- rast(list_files)
layer_names_GEDI <- basename(substr(basename(list_files), 12, nchar(basename(list_files))-4))
species_raster_GEDI <- GEDI
richness <- sum(GEDI, na.rm = T)
richness_map_GEDI <- richness

setwd("D:/uni/thesis/doctoral thesis/proposal for mediterranean north of Iran/GEDI/analysis/result analysis based community/prediction/worldclimate/")
list_files <- list.files(pattern = ".tif",recursive = TRUE, full.names = TRUE)
worldclimate <- rast(list_files)
layer_names_worldclimate <- basename(substr(basename(list_files), 12, nchar(basename(list_files))-4))
species_raster_worldclimate <- worldclimate
richness <- sum(worldclimate, na.rm = T)
richness_map_worldclimate <- richness

setwd("D:/uni/thesis/doctoral thesis/proposal for mediterranean north of Iran/GEDI/analysis/result analysis based community/prediction/togather/")
list_files <- list.files(pattern = ".tif",recursive = TRUE, full.names = TRUE)
together <- rast(list_files)
layer_names_together <- basename(substr(basename(list_files), 12, nchar(basename(list_files))-4))
species_raster_together <- together
richness <- sum(together, na.rm = T)
richness_map_together <- richness



library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "Species Distribution and Richness Dashboard", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Raster Data", tabName = "raster_data", icon = icon("map")),
      menuItem("Richness Data", tabName = "richness_data", icon = icon("pagelines")),
      
      # Select Inputs for Raster Data
      selectInput("species_GEDI", "Select Species (GEDI):", choices = layer_names_GEDI),
      selectInput("species_worldclimate", "Select Species (Worldclimate):", choices = layer_names_worldclimate),
      selectInput("species_together", "Select Species (GEDI & Worldclimate):", choices = layer_names_together)
    )
  ),
  
  dashboardBody(
    tabItems(
      # Raster Data Page
      tabItem(tabName = "raster_data",
              fluidRow(
                tabBox(title = "Species Distribution Maps", width = 12,
                       tabPanel("GEDI",
                                leafletOutput("map_GEDI"),
                                textOutput("species_info_GEDI"),
                                title = "GEDI Species Distribution Map"),
                       tabPanel("Worldclimate",
                                leafletOutput("map_worldclimate"),
                                textOutput("species_info_worldclimate"),
                                title = "Worldclimate Species Distribution Map"),
                       tabPanel("GEDI & Worldclimate",
                                leafletOutput("map_together"),
                                textOutput("species_info_together"),
                                title = "GEDI & Worldclimate Species Distribution Map")
                )
              )
      ),
      
      # Richness Data Page
      tabItem(tabName = "richness_data",
              fluidRow(
                tabBox(title = "Species Richness Maps", width = 12,
                       tabPanel("GEDI Richness",
                                leafletOutput("richness_map_GEDI"),
                                title = "GEDI Species Richness Map"),
                       tabPanel("Worldclimate Richness",
                                leafletOutput("richness_map_worldclimate"),
                                title = "Worldclimate Species Richness Map"),
                       tabPanel("GEDI & Worldclimate Richness",
                                leafletOutput("richness_map_together"),
                                title = "GEDI & Worldclimate Species Richness Map")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # GEDI Raster Dataset
  output$species_info_GEDI <- renderText({
    paste(input$species_GEDI, ": Distribution and related information for GEDI Dataset")
  })
  output$map_GEDI <- renderLeaflet({
    selected_layer <- species_raster_GEDI[[which(layer_names_GEDI == input$species_GEDI)]]
    leaflet() %>%
      addTiles() %>%
      addRasterImage(selected_layer, 
                     colors = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                values = values(selected_layer), 
                title = "Species Distribution (GEDI)")
  })
  
  # Worldclimate Raster Dataset
  output$species_info_worldclimate <- renderText({
    paste(input$species_worldclimate, ": Distribution and related information for Worldclimate Dataset")
  })
  output$map_worldclimate <- renderLeaflet({
    selected_layer <- species_raster_worldclimate[[which(layer_names_worldclimate == input$species_worldclimate)]]
    leaflet() %>%
      addTiles() %>%
      addRasterImage(selected_layer, 
                     colors = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                values = values(selected_layer), 
                title = "Species Distribution (Worldclimate)")
  })
  
  # GEDI & Worldclimate Together Raster Dataset
  output$species_info_together <- renderText({
    paste(input$species_together, ": Distribution and related information for GEDI & Worldclimate Dataset")
  })
  output$map_together <- renderLeaflet({
    selected_layer <- species_raster_together[[which(layer_names_together == input$species_together)]]
    leaflet() %>%
      addTiles() %>%
      addRasterImage(selected_layer, 
                     colors = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                values = values(selected_layer), 
                title = "Species Distribution (GEDI & Worldclimate)")
  })
  
  # GEDI Richness Map
  output$richness_map_GEDI <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(richness_map_GEDI, 
                     colors = colorNumeric("magma", values(richness_map_GEDI), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("magma", values(richness_map_GEDI), na.color = "transparent"), 
                values = values(richness_map_GEDI), 
                title = "Species Richness (GEDI)")
  })
  
  # Worldclimate Richness Map
  output$richness_map_worldclimate <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(richness_map_worldclimate, 
                     colors = colorNumeric("magma", values(richness_map_worldclimate), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("magma", values(richness_map_worldclimate), na.color = "transparent"), 
                values = values(richness_map_worldclimate), 
                title = "Species Richness (Worldclimate)")
  })
  
  # GEDI & Worldclimate Richness Map
  output$richness_map_together <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(richness_map_together, 
                     colors = colorNumeric("magma", values(richness_map_together), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("magma", values(richness_map_together), na.color = "transparent"), 
                values = values(richness_map_together), 
                title = "Species Richness (GEDI & Worldclimate)")
  })
}

shinyApp(ui = ui, server = server)



















library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "Enhanced Species Dashboard", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Species Maps", tabName = "species_maps", icon = icon("globe")),
      menuItem("Species Richness", tabName = "species_richness", icon = icon("leaf")),
      hr(),
      
      # Selection Inputs for Raster Data
      h4("Species Selection"),
      selectInput("species_GEDI", "GEDI:", choices = layer_names_GEDI),
      selectInput("species_worldclimate", "Worldclimate:", choices = layer_names_worldclimate),
      selectInput("species_together", "GEDI & Worldclimate:", choices = layer_names_together)
    )
  ),
  
  dashboardBody(
    tabItems(
      # Species Maps Tab
      tabItem(tabName = "species_maps",
              fluidRow(
                h2("Species Distribution Maps"),
                
                column(4,
                       box(
                         title = "GEDI Map", status = "primary", solidHeader = TRUE, width = NULL,
                         leafletOutput("map_GEDI", height = 250),
                         textOutput("species_info_GEDI")
                       )
                ),
                
                column(4,
                       box(
                         title = "Worldclimate Map", status = "primary", solidHeader = TRUE, width = NULL,
                         leafletOutput("map_worldclimate", height = 250),
                         textOutput("species_info_worldclimate")
                       )
                ),
                
                column(4,
                       box(
                         title = "GEDI & Worldclimate Map", status = "primary", solidHeader = TRUE, width = NULL,
                         leafletOutput("map_together", height = 250),
                         textOutput("species_info_together")
                       )
                )
              )
      ),
      
      # Species Richness Tab
      tabItem(tabName = "species_richness",
              fluidRow(
                h2("Species Richness Maps"),
                
                column(4,
                       box(
                         title = "GEDI Richness", status = "success", solidHeader = TRUE, width = NULL,
                         leafletOutput("richness_map_GEDI", height = 250)
                       )
                ),
                
                column(4,
                       box(
                         title = "Worldclimate Richness", status = "success", solidHeader = TRUE, width = NULL,
                         leafletOutput("richness_map_worldclimate", height = 250)
                       )
                ),
                
                column(4,
                       box(
                         title = "GEDI & Worldclimate Richness", status = "success", solidHeader = TRUE, width = NULL,
                         leafletOutput("richness_map_together", height = 250)
                       )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # GEDI Raster Dataset
  output$species_info_GEDI <- renderText({
    paste(input$species_GEDI, ": Distribution and related information for GEDI Dataset")
  })
  output$map_GEDI <- renderLeaflet({
    selected_layer <- species_raster_GEDI[[which(layer_names_GEDI == input$species_GEDI)]]
    leaflet() %>%
      addTiles() %>%
      addRasterImage(selected_layer, 
                     colors = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                values = values(selected_layer), 
                title = "Species Distribution (GEDI)")
  })
  
  # Worldclimate Raster Dataset
  output$species_info_worldclimate <- renderText({
    paste(input$species_worldclimate, ": Distribution and related information for Worldclimate Dataset")
  })
  output$map_worldclimate <- renderLeaflet({
    selected_layer <- species_raster_worldclimate[[which(layer_names_worldclimate == input$species_worldclimate)]]
    leaflet() %>%
      addTiles() %>%
      addRasterImage(selected_layer, 
                     colors = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                values = values(selected_layer), 
                title = "Species Distribution (Worldclimate)")
  })
  
  # GEDI & Worldclimate Together Raster Dataset
  output$species_info_together <- renderText({
    paste(input$species_together, ": Distribution and related information for GEDI & Worldclimate Dataset")
  })
  output$map_together <- renderLeaflet({
    selected_layer <- species_raster_together[[which(layer_names_together == input$species_together)]]
    leaflet() %>%
      addTiles() %>%
      addRasterImage(selected_layer, 
                     colors = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                values = values(selected_layer), 
                title = "Species Distribution (GEDI & Worldclimate)")
  })
  
  # GEDI Richness Map
  output$richness_map_GEDI <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(richness_map_GEDI, 
                     colors = colorNumeric("magma", values(richness_map_GEDI), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("magma", values(richness_map_GEDI), na.color = "transparent"), 
                values = values(richness_map_GEDI), 
                title = "Species Richness (GEDI)")
  })
  
  # Worldclimate Richness Map
  output$richness_map_worldclimate <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(richness_map_worldclimate, 
                     colors = colorNumeric("magma", values(richness_map_worldclimate), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("magma", values(richness_map_worldclimate), na.color = "transparent"), 
                values = values(richness_map_worldclimate), 
                title = "Species Richness (Worldclimate)")
  })
  
  # GEDI & Worldclimate Richness Map
  output$richness_map_together <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(richness_map_together, 
                     colors = colorNumeric("magma", values(richness_map_together), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("magma", values(richness_map_together), na.color = "transparent"), 
                values = values(richness_map_together), 
                title = "Species Richness (GEDI & Worldclimate)")
  })
}

shinyApp(ui = ui, server = server)

































ui <- dashboardPage(
  dashboardHeader(title = "Enhanced Species Dashboard", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Species Maps", tabName = "species_maps", icon = icon("globe")),
      menuItem("Species Richness", tabName = "species_richness", icon = icon("leaf")),
      menuItem("Richness Plot", tabName = "richness_plot", icon = icon("chart-line")), # تب جدید برای نمودار
      hr()
    )
  ),
  
  dashboardBody(
    # نوار پایین species selection فقط برای تب species_maps نمایش داده می‌شود
    conditionalPanel(
      condition = "input.sidebar == 'species_maps'", # نمایش فقط در تب Species Maps
      fluidRow(
        h4("Species Selection"),
        column(4, selectInput("species_GEDI", "GEDI:", choices = layer_names_GEDI)),
        column(4, selectInput("species_worldclimate", "Worldclimate:", choices = layer_names_worldclimate)),
        column(4, selectInput("species_together", "GEDI & Worldclimate:", choices = layer_names_together))
      ),
      hr()
    ),
    
    tabItems(
      # Species Maps Tab
      tabItem(tabName = "species_maps",
              fluidRow(
                h2("Species Distribution Maps"),
                
                column(4,
                       box(
                         title = "GEDI Map", status = "primary", solidHeader = TRUE, width = NULL,
                         leafletOutput("map_GEDI", height = 250),
                         textOutput("species_info_GEDI")
                       )
                ),
                
                column(4,
                       box(
                         title = "Worldclimate Map", status = "primary", solidHeader = TRUE, width = NULL,
                         leafletOutput("map_worldclimate", height = 250),
                         textOutput("species_info_worldclimate")
                       )
                ),
                
                column(4,
                       box(
                         title = "GEDI & Worldclimate Map", status = "primary", solidHeader = TRUE, width = NULL,
                         leafletOutput("map_together", height = 250),
                         textOutput("species_info_together")
                       )
                )
              )
      ),
      
      # Species Richness Tab
      tabItem(tabName = "species_richness",
              fluidRow(
                h2("Species Richness Maps"),
                
                column(4,
                       box(
                         title = "GEDI Richness", status = "success", solidHeader = TRUE, width = NULL,
                         leafletOutput("richness_map_GEDI", height = 250)
                       )
                ),
                
                column(4,
                       box(
                         title = "Worldclimate Richness", status = "success", solidHeader = TRUE, width = NULL,
                         leafletOutput("richness_map_worldclimate", height = 250)
                       )
                ),
                
                column(4,
                       box(
                         title = "GEDI & Worldclimate Richness", status = "success", solidHeader = TRUE, width = NULL,
                         leafletOutput("richness_map_together", height = 250)
                       )
                )
              )
      ),
      
      # Richness Plot Tab (تب جدید برای نمودار)
      tabItem(tabName = "richness_plot",
              fluidRow(
                h2("Distribution of Richness across 3 Scenarios of SDM"),
                
                box(
                  title = "Richness Plot", status = "info", solidHeader = TRUE, width = 12,
                  plotOutput("richness_plot_output", height = 400) # خروجی نمودار
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # GEDI Raster Dataset
  output$species_info_GEDI <- renderText({
    paste(input$species_GEDI, ": Distribution and related information for GEDI Dataset")
  })
  output$map_GEDI <- renderLeaflet({
    selected_layer <- species_raster_GEDI[[which(layer_names_GEDI == input$species_GEDI)]]
    leaflet() %>%
      addTiles() %>%
      addRasterImage(selected_layer, 
                     colors = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                values = values(selected_layer), 
                title = "Species Distribution (GEDI)")
  })
  
  # Worldclimate Raster Dataset
  output$species_info_worldclimate <- renderText({
    paste(input$species_worldclimate, ": Distribution and related information for Worldclimate Dataset")
  })
  output$map_worldclimate <- renderLeaflet({
    selected_layer <- species_raster_worldclimate[[which(layer_names_worldclimate == input$species_worldclimate)]]
    leaflet() %>%
      addTiles() %>%
      addRasterImage(selected_layer, 
                     colors = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                values = values(selected_layer), 
                title = "Species Distribution (Worldclimate)")
  })
  
  # GEDI & Worldclimate Together Raster Dataset
  output$species_info_together <- renderText({
    paste(input$species_together, ": Distribution and related information for GEDI & Worldclimate Dataset")
  })
  output$map_together <- renderLeaflet({
    selected_layer <- species_raster_together[[which(layer_names_together == input$species_together)]]
    leaflet() %>%
      addTiles() %>%
      addRasterImage(selected_layer, 
                     colors = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("viridis", values(selected_layer), na.color = "transparent"), 
                values = values(selected_layer), 
                title = "Species Distribution (GEDI & Worldclimate)")
  })
  
  # GEDI Richness Map
  output$richness_map_GEDI <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(richness_map_GEDI, 
                     colors = colorNumeric("magma", values(richness_map_GEDI), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("magma", values(richness_map_GEDI), na.color = "transparent"), 
                values = values(richness_map_GEDI), 
                title = "Species Richness (GEDI)")
  })
  
  # Worldclimate Richness Map
  output$richness_map_worldclimate <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(richness_map_worldclimate, 
                     colors = colorNumeric("magma", values(richness_map_worldclimate), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("magma", values(richness_map_worldclimate), na.color = "transparent"), 
                values = values(richness_map_worldclimate), 
                title = "Species Richness (Worldclimate)")
  })
  
  # GEDI & Worldclimate Richness Map
  output$richness_map_together <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addRasterImage(richness_map_together, 
                     colors = colorNumeric("magma", values(richness_map_together), na.color = "transparent"), 
                     opacity = 0.7) %>%
      addLegend(pal = colorNumeric("magma", values(richness_map_together), na.color = "transparent"), 
                values = values(richness_map_together), 
                title = "Species Richness (GEDI & Worldclimate)")
  })
  
  # Richness Plot (کد ggplot برای نمودار)
  output$richness_plot_output <- renderPlot({
    ggplot(df_long, aes(x = 1:nrow(df_long), y = Value, color = Variable)) +
      geom_point(aes(shape = Variable)) +           
      geom_smooth(method = "lm", se = FALSE) +      
      labs(x = "Plot", y = "Richness", title = "Distribution of Richness across 3 Scenarios of SDM") +
      scale_x_continuous(breaks = breaks, labels = labels) +
      scale_color_manual(values = c("red", "blue", "green", "purple"),
                         labels = c("plot", "GEDI", "Worldclimate", "GEDI & worldclimate"),
                         name = "Prediction") +
      scale_shape_manual(values = c(16, 17, 18, 19),
                         labels = c("plot", "GEDI", "Worldclimate", "GEDI & worldclimate"),
                         name = "Prediction") +
      geom_richtext(data = m, 
                    aes(x = x_position, y = mean_value, 
                        label = sprintf("Total Mean Richness: **%.2f**", round(mean_value, 2))),
                    color = "black", size = 4, hjust = 0.5,
                    fill = NA, label.color = NA) +
      theme(plot.background = element_blank(),
            panel.background = element_blank(),
            legend.title = element_text(face = "bold"),
            legend.box.background = element_blank())
  })
}

shinyApp(ui = ui, server = server)




























































