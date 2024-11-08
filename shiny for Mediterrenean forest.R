ui <- dashboardPage(
  dashboardHeader(title = "Enhanced Species Dashboard", titleWidth = 300),
  
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Species Maps", tabName = "species_maps", icon = icon("globe")),
      menuItem("Species Richness", tabName = "species_richness", icon = icon("leaf")),
      menuItem("Richness Plot", tabName = "richness_plot", icon = icon("chart-line")), # 
      hr()
    )
  ),
  
  dashboardBody(
    conditionalPanel(
      condition = "input.sidebar == 'species_maps'", # 
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
      
      # Richness Plot Tab 
      tabItem(tabName = "richness_plot",
              fluidRow(
                h2("Distribution of Richness across 3 Scenarios of SDM"),
                
                box(
                  title = "Richness Plot", status = "info", solidHeader = TRUE, width = 12,
                  plotOutput("richness_plot_output", height = 400)),
                box(
                  title = "Model Evaluation Metrics", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("cor_results"),
                  verbatimTextOutput("aic_bic_results")
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
  output$cor_results <- renderPrint({
    cor_pearson_sdm1 <- cor(r9$richness, r9$richness_pred, method = "pearson")
    cor_pearson_sdm2 <- cor(r9$richness, r9$comparison_w.richness_pred, method = "pearson")
    cor_pearson_sdm3 <- cor(r9$richness, r9$comparison_t.richness_pred, method = "pearson")
    
    cat("Pearson Correlation Coefficients of Observed Richness VR :\n")
    cat("GEDI:", cor_pearson_sdm1, "\n")
    cat("Worldclimate:", cor_pearson_sdm2, "\n")
    cat("GEDI & Worldclimate:", cor_pearson_sdm3, "\n")
  })
  
  output$aic_bic_results <- renderPrint({
    model_sdm1 <- lm(richness ~ richness_pred, data = r9)
    model_sdm2 <- lm(richness ~ comparison_w.richness_pred, data = r9)
    model_sdm3 <- lm(richness ~ comparison_t.richness_pred, data = r9)
    
    aic_sdm1 <- AIC(model_sdm1)
    aic_sdm2 <- AIC(model_sdm2)
    aic_sdm3 <- AIC(model_sdm3)
    
    bic_sdm1 <- BIC(model_sdm1)
    bic_sdm2 <- BIC(model_sdm2)
    bic_sdm3 <- BIC(model_sdm3)
    
    cat("AIC and BIC Metrics:\n")
    cat("GEDI - AIC:", aic_sdm1, "BIC:", bic_sdm1, "\n")
    cat("Worldclimate - AIC:", aic_sdm2, "BIC:", bic_sdm2, "\n")
    cat("GEDI & Worldclimate - AIC:", aic_sdm3, "BIC:", bic_sdm3, "\n")
  })
  
  # Richness Plot
  output$richness_plot_output <- renderPlot({
    ggplot(df_long, aes(x = 1:nrow(df_long), y = Value, color = Variable)) +
      geom_point(aes(shape = Variable)) +           
      geom_smooth(method = "lm", se = FALSE) +      
      labs(x = "Plot", y = "Richness", title = "Distribution of Richness across 3 Scenarios of SDM ver main plot") +
      scale_x_continuous(breaks = breaks, labels = labels) +
      
      scale_color_manual(
        values = c("red", "blue", "green", "purple"),
        labels = c("plot", "GEDI", "Worldclimate", "GEDI & worldclimate"),
        name = "prediction"
      ) +
      
      scale_shape_manual(
        values = c(16, 17, 18, 19),
        labels = c("plot", "GEDI", "Worldclimate", "GEDI & worldclimate"),
        name = "prediction"
      ) +
      
      geom_richtext(
        data = m, 
        aes(
          x = x_position, 
          y = mean_value, 
          label = sprintf("Total Mean Richness: **%.2f**", round(mean_value, 2))
        ),
        color = "black", size = 4, hjust = 0.5,
        fill = NA,           
        label.color = NA     
      ) +
      
      theme(
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(face = "bold"),
        legend.box.background = element_blank()
      )
  })
}

shinyApp(ui = ui, server = server)


