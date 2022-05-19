#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# library loading and installing as necessary
# relevant R packages
packages <- c("metafor", "tidyverse", "here", "data.table", "lavaan", "gridExtra")

# check, whether library already installed or not - install and load as needed:
apply(as.matrix(packages), MARGIN = 1, FUN = function(x) {
    
    pkg_avail <- nzchar(system.file(package = x))   # check if library is installed on system
    
    if(pkg_avail){
        require(x, character.only = TRUE)             # load the library, if already installed
        
    }else{
        install.packages(x)                           # install the library, if missing
        require(x, character.only = TRUE)             # load after installation
    }
})



source(here("ReliabilityFunctions_RG.R"))

# only run if you wish to recompute the reliability estimates!

# currently does not work! Produces errors, while running script directly does not (unclear why)
#source(here("Generalization_Initialization_HEXACO.R"))
source(here("Loading_Estimates_HEXACO.R"))





# Define UI for application that draws a histogram
ui <- navbarPage(
        
    "Reliability Generalization HEXACO-60",
    
    tabPanel(
        "Box-Plots",
        
        sidebarPanel(
            
            selectInput(inputId = "DimensionBP",
                        label = "Personality Dimension",
                        choices = c("Honesty-Humility", "Emotionality",
                                    "Extraversion", "Agreeableness",
                                    "Conscientiousness", "Openness to Experiences")),
            
            selectInput(inputId = "RelMeasBP",
                        label = "Reliability Measure",
                        choices = c("Cronbach's Alpha", "McDonald's Omega",
                                    "Bonett-transf. Alpha", "Bonett-transf. Omega"))
        ),
        
        mainPanel(
            h4("Box-Plots for Selected Personality Dimension"),
            plotOutput(outputId = "boxplot")
        )
    ),
    
    tabPanel(
        
        "Individual Forest Plots",
        
        sidebarPanel(
            
            selectInput(inputId = "DimensionFP",
                        label = "Personality Dimension",
                        choices = c("Honesty-Humility", "Emotionality",
                                    "Extraversion", "Agreeableness",
                                    "Conscientiousness", "Openness to Experiences")),
            
            selectInput(inputId = "RelMeasFP",
                        label = "Reliability Measure",
                        choices = c("Cronbach's Alpha", "McDonald's Omega",
                                    "Bonett-transf. Alpha", "Bonett-transf. Omega"))
        ),
        
        mainPanel(
            h4("Forest Plots for Selected Personality Dimension & Reliability Measure"),
            plotOutput(outputId = "forsplot")
        )
        
    ),
    
    tabPanel(
        "Random-Effects MA-Model",
        
        sidebarPanel(
            
            selectInput(inputId = "DimensionBP",
                        label = "Personality Dimension",
                        choices = c("Honesty-Humility", "Emotionality",
                                    "Extraversion", "Agreeableness",
                                    "Conscientiousness", "Openness to Experiences")),
            
            selectInput(inputId = "RelMeasBP",
                        label = "Reliability Measure",
                        choices = c("Cronbach's Alpha", "McDonald's Omega",
                                    "Bonett-transf. Alpha", "Bonett-transf. Omega"))
        ),
        
        mainPanel(
            h4("Random Effects Meta-Analytic Models"),
            verbatimTextOutput(outputId = "REMA")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$forsplot <- renderPlot({
        
        if(input$RelMeasFP == "Cronbach's Alpha"){
            if(input$DimensionFP == "Honesty-Humility"){
                data <- AlphaHex_HH
            }
            if(input$DimensionFP == "Emotionality"){
                data <- AlphaHex_EM
            }
            if(input$DimensionFP == "Extraversion"){
                data <- AlphaHex_EX
            }
            if(input$DimensionFP == "Agreeableness"){
                data <- AlphaHex_AG
            }
            if(input$DimensionFP == "Conscientiousness"){
                data <- AlphaHex_CO
            }
            if(input$DimensionFP == "Openness to Experiences"){
                data <- AlphaHex_OX
            }
        }
        if(input$RelMeasFP == "McDonald's Omega"){
            if(input$DimensionFP == "Honesty-Humility"){
                data <- OmegaHex_HH
            }
            if(input$DimensionFP == "Emotionality"){
                data <- OmegaHex_EM
            }
            if(input$DimensionFP == "Extraversion"){
                data <- OmegaHex_EX
            }
            if(input$DimensionFP == "Agreeableness"){
                data <- OmegaHex_AG
            }
            if(input$DimensionFP == "Conscientiousness"){
                data <- OmegaHex_CO
            }
            if(input$DimensionFP == "Openness to Experiences"){
                data <- OmegaHex_OX
            }
        }
        if(input$RelMeasFP == "Bonett-transf. Alpha"){
            if(input$DimensionFP == "Honesty-Humility"){
                data <- BonettAlphaHex_HH
            }
            if(input$DimensionFP == "Emotionality"){
                data <- BonettAlphaHex_EM
            }
            if(input$DimensionFP == "Extraversion"){
                data <- BonettAlphaHex_EX
            }
            if(input$DimensionFP == "Agreeableness"){
                data <- BonettAlphaHex_AG
            }
            if(input$DimensionFP == "Conscientiousness"){
                data <- BonettAlphaHex_CO
            }
            if(input$DimensionFP == "Openness to Experiences"){
                data <- BonettAlphaHex_OX
            }
        }
        if(input$RelMeasFP == "Bonett-transf. Omega"){
            if(input$DimensionFP == "Honesty-Humility"){
                data <- BonettOmegaHex_HH
            }
            if(input$DimensionFP == "Emotionality"){
                data <- BonettOmegaHex_EM
            }
            if(input$DimensionFP == "Extraversion"){
                data <- BonettOmegaHex_EX
            }
            if(input$DimensionFP == "Agreeableness"){
                data <- BonettOmegaHex_AG
            }
            if(input$DimensionFP == "Conscientiousness"){
                data <- BonettOmegaHex_CO
            }
            if(input$DimensionFP == "Openness to Experiences"){
                data <- BonettOmegaHex_OX
            }
        }

        
        forsplot <- my_forest_plot(rel_rma(data), data, main.title = paste0("Forest Plot ", input$DimensionFP),
                                   x.lab = input$RelMeasFP)
        
        print(forsplot)
    
    })
    
    

    output$boxplot <- renderPlot({
        
        if(input$RelMeasBP == "Cronbach's Alpha"){
            if(input$DimensionBP == "Honesty-Humility"){
                data <- AlphaHex_HH
            }
            if(input$DimensionBP == "Emotionality"){
                data <- AlphaHex_EM
            }
            if(input$DimensionBP == "Extraversion"){
                data <- AlphaHex_EX
            }
            if(input$DimensionBP == "Agreeableness"){
                data <- AlphaHex_AG
            }
            if(input$DimensionBP == "Conscientiousness"){
                data <- AlphaHex_CO
            }
            if(input$DimensionBP == "Openness to Experiences"){
                data <- AlphaHex_OX
            }
        }
        if(input$RelMeasBP == "McDonald's Omega"){
            if(input$DimensionBP == "Honesty-Humility"){
                data <- OmegaHex_HH
            }
            if(input$DimensionBP == "Emotionality"){
                data <- OmegaHex_EM
            }
            if(input$DimensionBP == "Extraversion"){
                data <- OmegaHex_EX
            }
            if(input$DimensionBP == "Agreeableness"){
                data <- OmegaHex_AG
            }
            if(input$DimensionBP == "Conscientiousness"){
                data <- OmegaHex_CO
            }
            if(input$DimensionBP == "Openness to Experiences"){
                data <- OmegaHex_OX
            }
        }
        if(input$RelMeasBP == "Bonett-transf. Alpha"){
            if(input$DimensionBP == "Honesty-Humility"){
                data <- BonettAlphaHex_HH
            }
            if(input$DimensionBP == "Emotionality"){
                data <- BonettAlphaHex_EM
            }
            if(input$DimensionBP == "Extraversion"){
                data <- BonettAlphaHex_EX
            }
            if(input$DimensionBP == "Agreeableness"){
                data <- BonettAlphaHex_AG
            }
            if(input$DimensionBP == "Conscientiousness"){
                data <- BonettAlphaHex_CO
            }
            if(input$DimensionBP == "Openness to Experiences"){
                data <- BonettAlphaHex_OX
            }
        }
        if(input$RelMeasBP == "Bonett-transf. Omega"){
            if(input$DimensionBP == "Honesty-Humility"){
                data <- BonettOmegaHex_HH
            }
            if(input$DimensionBP == "Emotionality"){
                data <- BonettOmegaHex_EM
            }
            if(input$DimensionBP == "Extraversion"){
                data <- BonettOmegaHex_EX
            }
            if(input$DimensionBP == "Agreeableness"){
                data <- BonettOmegaHex_AG
            }
            if(input$DimensionBP == "Conscientiousness"){
                data <- BonettOmegaHex_CO
            }
            if(input$DimensionBP == "Openness to Experiences"){
                data <- BonettOmegaHex_OX
            }
        }
        
        bplot <- ggplot(data = data, aes(x = reliability)) + 
            geom_histogram(bins = 10, fill = "Orange", color = "black") +
            labs(title = paste0("Boxplot ", input$DimensionBP), y = "Frequency", 
                 x = input$RelMeasBP)
        
        bplot
        
    })
    
    
    output$REMA <- renderText({
        
        if(input$RelMeasBP == "Cronbach's Alpha"){
            if(input$DimensionBP == "Honesty-Humility"){
                data <- AlphaHex_HH
            }
            if(input$DimensionBP == "Emotionality"){
                data <- AlphaHex_EM
            }
            if(input$DimensionBP == "Extraversion"){
                data <- AlphaHex_EX
            }
            if(input$DimensionBP == "Agreeableness"){
                data <- AlphaHex_AG
            }
            if(input$DimensionBP == "Conscientiousness"){
                data <- AlphaHex_CO
            }
            if(input$DimensionBP == "Openness to Experiences"){
                data <- AlphaHex_OX
            }
        }
        if(input$RelMeasBP == "McDonald's Omega"){
            if(input$DimensionBP == "Honesty-Humility"){
                data <- OmegaHex_HH
            }
            if(input$DimensionBP == "Emotionality"){
                data <- OmegaHex_EM
            }
            if(input$DimensionBP == "Extraversion"){
                data <- OmegaHex_EX
            }
            if(input$DimensionBP == "Agreeableness"){
                data <- OmegaHex_AG
            }
            if(input$DimensionBP == "Conscientiousness"){
                data <- OmegaHex_CO
            }
            if(input$DimensionBP == "Openness to Experiences"){
                data <- OmegaHex_OX
            }
        }
        if(input$RelMeasBP == "Bonett-transf. Alpha"){
            if(input$DimensionBP == "Honesty-Humility"){
                data <- BonettAlphaHex_HH
            }
            if(input$DimensionBP == "Emotionality"){
                data <- BonettAlphaHex_EM
            }
            if(input$DimensionBP == "Extraversion"){
                data <- BonettAlphaHex_EX
            }
            if(input$DimensionBP == "Agreeableness"){
                data <- BonettAlphaHex_AG
            }
            if(input$DimensionBP == "Conscientiousness"){
                data <- BonettAlphaHex_CO
            }
            if(input$DimensionBP == "Openness to Experiences"){
                data <- BonettAlphaHex_OX
            }
        }
        if(input$RelMeasBP == "Bonett-transf. Omega"){
            if(input$DimensionBP == "Honesty-Humility"){
                data <- BonettOmegaHex_HH
            }
            if(input$DimensionBP == "Emotionality"){
                data <- BonettOmegaHex_EM
            }
            if(input$DimensionBP == "Extraversion"){
                data <- BonettOmegaHex_EX
            }
            if(input$DimensionBP == "Agreeableness"){
                data <- BonettOmegaHex_AG
            }
            if(input$DimensionBP == "Conscientiousness"){
                data <- BonettOmegaHex_CO
            }
            if(input$DimensionBP == "Openness to Experiences"){
                data <- BonettOmegaHex_OX
            }
        }
        
        rmarel <- rel_rma(data)
        
        
        
    })
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
