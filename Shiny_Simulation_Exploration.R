#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(dplyr)

library(magrittr)

library(here)

library(ggplot2)

full_sim <- read.csv(here("Simulation Data/Sim80000_rma_df.csv"))

sim_aggregates <- read.csv(here("Data/Shiny Data/Sim80000_rma_agg_df.csv"))


# Define UI for application that draws a histogram
ui <- navbarPage(

  # Application title
  "Erroneous Generalization - Simulation Data", 
  

  # First panel - estimates
    tabPanel(
      "Simulation Results - estimates", 
      
        sidebarPanel(
          ##### FACET SCATTER PLOT - P1
          
          ### X-variable selection
          selectInput(inputId = "X_variable_p1",
                      label = "Select row-variable.",
                      choices = c("sim. tau E" = "tau_varE",
                                  "sim. tau T" = "tau_varT",
                                  "sim. tau X" = "tau_varX",
                                  "sim. tau rel" = "pred.tau_rel"),
                      selected = "tau_varE"),
          
          ### Y-variable selection
          selectInput(inputId = "Y_variable_p1",
                      label = "Select row-variable.",
                      choices = c("est. tau E" = "tau_E",
                                  "est. tau T" = "tau_T",
                                  "est. tau X" = "tau_X",
                                  "est. tau rel" = "tau_rel"),
                      selected = "tau_E"),
          
          ### row-variable selection
          selectInput(inputId = "row_variable_p1",
                      label = "Select row-variable.",
                      choices = c("CVT",
                                  "CVE",
                                  "reliability" = "rel"),
                      selected = "CVT"),
          
          ### column-variable selection
          selectInput(inputId = "column_variable_p1",
                      label = "Select column-variable.",
                      choices = c("CVT",
                                  "CVE",
                                  "reliability" = "rel"),
                      selected = "rel"),
          
          ### colour-variable selection
          selectInput(inputId = "colour_variable_p1",
                      label = "Select colour-variable.",
                      choices = c("CVT",
                                  "CVE",
                                  "reliability" = "rel"),
                      selected = "CVE"), 
          
          ### boolean choice whether aggregates or all estimates are displayed
          checkboxInput(inputId = "aggregates_boolean_p1",
                        label = "Use aggregates",
                        value = TRUE)
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("p1")
        )
        
    ),
  
  # second panel - violins
  tabPanel(
    "Simulation Results - violin plots", 
    
    sidebarPanel(
      ##### FACET VIOLIN PLOT - P2
      
      ### X-variable selection
      selectInput(inputId = "X_variable_p2",
                  label = "Select row-variable.",
                  choices = c("sim. tau E" = "tau_varE",
                              "sim. tau T" = "tau_varT",
                              "sim. tau X" = "tau_varX",
                              "sim. tau rel" = "pred.tau_rel"),
                  selected = "tau_varT"),
      
      ### Y-variable selection
      selectInput(inputId = "Y_variable_p2",
                  label = "Select row-variable.",
                  choices = c("est. tau E" = "tau_E",
                              "est. tau T" = "tau_T",
                              "est. tau X" = "tau_X",
                              "est. tau rel" = "tau_rel"),
                  selected = "tau_T"),
      
      ### row-variable selection
      selectInput(inputId = "row_variable_p2",
                  label = "Select row-variable.",
                  choices = c("CVT",
                              "CVE",
                              "reliability" = "rel"),
                  selected = "CVE"),
      
      ### column-variable selection
      selectInput(inputId = "column_variable_p2",
                  label = "Select column-variable.",
                  choices = c("CVT",
                              "CVE",
                              "reliability" = "rel"),
                  selected = "rel"),
      
      ### colour-variable selection
      selectInput(inputId = "colour_variable_p2",
                  label = "Select colour-variable.",
                  choices = c("CVT",
                              "CVE",
                              "reliability" = "rel"),
                  selected = "CVT")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("p2")
    )
    
  )
  
)





server <- function(input, output) {

  
  # select the correct data-set, depending on whether aggregated data, or all estimates should be displayed
    p1_data <- reactive({
      if(input$aggregates_boolean_p1){
        sim_aggregates
      }else{
        full_sim
      }
    })
  
  
    output$p1 <- renderPlot({
      
      # identify correct labels for axes, subtitle, caption etc.
      p1_labels <- data.frame(x = c("sim. tau E", "sim. tau T", "sim. tau X", "sim. tau reliability")[c("tau_varE", "tau_varT", "tau_varX","pred.tau_rel") %in% input$X_variable_p1],
                              y = c("est. tau E", "est. tau T", "est. tau X", "est. tau reliability")[c("tau_E", "tau_T", "tau_X","tau_rel") %in% input$Y_variable_p1],
                              row = c("CVT", "CVE", "reliability")[c("CVT", "CVE", "rel") %in% input$row_variable_p1],
                              column = c("CVT", "CVE", "reliability")[c("CVT", "CVE", "rel") %in% input$column_variable_p1],
                              colour = c("CVT", "CVE", "reliability")[c("CVT", "CVE", "rel") %in% input$colour_variable_p1])

      # initialise plot, draw empty grid
      p1 <- ggplot(p1_data()) +
        facet_grid(rows = vars(.data[[input$row_variable_p1]]),
                   cols = vars(.data[[input$column_variable_p1]]))
        
      # if aggregated data-set is used
      if(input$aggregates_boolean_p1){
       p1 <- p1 + 
         
         # add error-bars/estimation interval to estimates on grid
         geom_segment(aes(x = .data[[input$X_variable_p1]], 
                          xend = .data[[input$X_variable_p1]],
                          y = .data[[paste0(input$Y_variable_p1, "_ll")]], 
                          yend = .data[[paste0(input$Y_variable_p1, "_ul")]],
                          colour = as.factor(.data[[input$colour_variable_p1]])),
                      linewidth = 1.5) +
         
         # add mean points to grid
         geom_point(aes(x = .data[[input$X_variable_p1]], 
                        y = .data[[input$Y_variable_p1]],
                        colour = as.factor(.data[[input$colour_variable_p1]])),
                    size = 2) +
         
         # add correct labels to axes etc.
         labs(x = paste0("mean ", p1_labels$x),
              y = paste0("mean ", p1_labels$y),
              subtitle = paste0("rows: ", p1_labels$row, ", ", "columns: ", p1_labels$column),
              caption = "bars indicating 95%-estimation interval surrounding mean estimates within conditions",
              colour = p1_labels$colour)
       
       # if the full data-set is used
      }else{
        p1 <- p1 +
          
          # add all estimates to grid, with some jitter
          geom_point(aes(x = .data[[input$X_variable_p1]], 
                         y = .data[[input$Y_variable_p1]],
                         colour = as.factor(.data[[input$colour_variable_p1]])),
                     position = position_jitter(height = 0),
                     alpha = .1,
                     size = 2) +
          
          # add correct labels to axes etc.
          labs(x = p1_labels$x,
               y = p1_labels$y,
               subtitle = paste0("rows: ", p1_labels$row, ", ", "columns: ", p1_labels$column),
               colour = p1_labels$colour,
               caption = "estimates have been jittered on x-axis, to visualise the distribution more clearly")
      }
      
      p1 + guides(colour = guide_legend(override.aes = list(alpha = 1))) #+
      #   theme(legend.position = "none", 
      #         panel.background = element_rect(fill = "transparent"), 
      #         plot.background = element_rect(fill = "transparent", colour = "transparent"), 
      #         panel.grid.major.y = element_line(colour = "black"),
      #         panel.grid.major.x = element_line(colour = "black"),
      #         panel.grid.minor.x = element_line(colour = "transparent"),
      #         panel.grid.minor.y = element_line(colour = "transparent"),
      #         axis.ticks = element_line(colour = "black"),
      #         strip.background = element_rect(fill = "transparent"),
      #         axis.line.y = element_line(colour = "transparent"),
      #         legend.key = element_rect(fill = "transparent"),
      #         axis.title.y = element_blank(),
      #         axis.text = element_text(colour = "black", size = 15),
      #         axis.title.x = element_text(colour = "black", size = 15))
      # 
      
    })
    
    output$p2 <- renderPlot({
      
      # identify correct labels for axes, subtitle, caption etc.
      p2_labels <- data.frame(x = c("sim. tau E", "sim. tau T", "sim. tau X", "sim. tau reliability")[c("tau_varE", "tau_varT", "tau_varX","pred.tau_rel") %in% input$X_variable_p2],
                              y = c("est. tau E", "est. tau T", "est. tau X", "est. tau reliability")[c("tau_E", "tau_T", "tau_X","tau_rel") %in% input$Y_variable_p2],
                              row = c("CVT", "CVE", "reliability")[c("CVT", "CVE", "rel") %in% input$row_variable_p2],
                              column = c("CVT", "CVE", "reliability")[c("CVT", "CVE", "rel") %in% input$column_variable_p2],
                              colour = c("CVT", "CVE", "reliability")[c("CVT", "CVE", "rel") %in% input$colour_variable_p2])
      
      # initialise plot, draw empty grid
      p2 <- ggplot(full_sim) +
        facet_grid(rows = vars(.data[[input$row_variable_p2]]),
                   cols = vars(.data[[input$column_variable_p2]]),
                   scales = "free") +
      
      # add violins to grid
        geom_violin(aes(x = as.factor(.data[[input$X_variable_p2]]), 
                      y = .data[[input$Y_variable_p2]],
                      fill = as.factor(.data[[input$colour_variable_p2]]),
                      colour = as.factor(.data[[input$colour_variable_p2]])),
                  alpha = .2) +
        
        # add boxplots to grid
        geom_boxplot(aes(x = as.factor(.data[[input$X_variable_p2]]), 
                         y = .data[[input$Y_variable_p2]],
                         fill = as.factor(.data[[input$colour_variable_p2]]),
                         colour = as.factor(.data[[input$colour_variable_p2]])),
                     width = .1) +
        
        # add correct labels to axes etc.
        labs(x = paste0("mean ", p2_labels$x),
             y = paste0("mean ", p2_labels$y),
             subtitle = paste0("rows: ", p2_labels$row, ", ", "columns: ", p2_labels$column),
             colour = p2_labels$colour, 
             fill = p2_labels$colour)
      
      p2 #+
      #   theme(legend.position = "none", 
      #         panel.background = element_rect(fill = "transparent"), 
      #         plot.background = element_rect(fill = "transparent", colour = "transparent"), 
      #         panel.grid.major.y = element_line(colour = "black"),
      #         panel.grid.major.x = element_line(colour = "black"),
      #         panel.grid.minor.x = element_line(colour = "transparent"),
      #         panel.grid.minor.y = element_line(colour = "transparent"),
      #         axis.ticks = element_line(colour = "black"),
      #         strip.background = element_rect(fill = "transparent"),
      #         axis.line.y = element_line(colour = "transparent"),
      #         legend.key = element_rect(fill = "transparent"),
      #         axis.title.y = element_blank(),
      #         axis.text = element_text(colour = "black", size = 15),
      #         axis.title.x = element_text(colour = "black", size = 15))
      # 
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
