

packages <- c("tidyverse", "here", "shiny")

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

# source(here("RG_function-library.R"))







vis.df <- read.csv(here("Notes/vis_df.csv"), sep = " ")

vis.df_summarised <- vis.df %>% 
  group_by(CVT, CVE, rel) %>%
  summarise(T_m = mean(tau_T, na.rm = T),
            E_m = mean(tau_E, na.rm = T),
            T_sd = sd(tau_T, na.rm = T),
            E_sd = sd(tau_E, na.rm = T),
            T_ll = quantile(tau_T, .025, na.rm = T),
            T_ul = quantile(tau_T, .975, na.rm = T),
            E_ll = quantile(tau_E, .025, na.rm = T),
            E_ul = quantile(tau_E, .975, na.rm = T),
            VT.MAE_m = mean(varT.MAE, na.rm = T),
            VE.MAE_m = mean(varE.MAE, na.rm = T),
            VT.MAE_ll = quantile(varT.MAE, .025, na.rm = T),
            VT.MAE_ul = quantile(varT.MAE, .975, na.rm = T),
            VE.MAE_ll = quantile(varE.MAE, .025, na.rm = T),
            VE.MAE_ul = quantile(varE.MAE, .975, na.rm = T),
            VT.M_m = mean(varT.M, na.rm = T),
            VE.M_m = mean(varE.M, na.rm = T),
            VT.M_ll = quantile(varT.M, .025, na.rm = T),
            VT.M_ul = quantile(varT.M, .975, na.rm = T),
            VE.M_ll = quantile(varE.M, .025, na.rm = T),
            VE.M_ul = quantile(varE.M, .975, na.rm = T),
            rel.M_m = mean(rel.M, na.rm = T),
            rel.M_ll = quantile(rel.M, .025, na.rm = T),
            rel.M_ul = quantile(rel.M, .975, na.rm = T),
            rel.MAE_m = mean(rel.MAE, na.rm = T),
            rel.MAE_ll = quantile(rel.MAE, .025, na.rm = T),
            rel.MAE_ul = quantile(rel.MAE, .975, na.rm = T),
            tau_rel_m = mean(tau_rel, na.rm = T),
            tau_rel_ll = quantile(tau_rel, .025, na.rm = T),
            tau_rel_ul = quantile(tau_rel, .975, na.rm = T),
            tau_rel_cT_m = mean(tau_rel_cT, na.rm = T),
            tau_rel_cT_ll = quantile(tau_rel_cT, .025, na.rm = T),
            tau_rel_cT_ul = quantile(tau_rel_cT, .975, na.rm = T),
            tau_rel_cE_m = mean(tau_rel_cE, na.rm = T),
            tau_rel_cE_ll = quantile(tau_rel_cE, .025, na.rm = T),
            tau_rel_cE_ul = quantile(tau_rel_cE, .975, na.rm = T),
            tau_rel_cTE_m = mean(tau_rel_cTE, na.rm = T),
            tau_rel_cTE_ll = quantile(tau_rel_cTE, .025, na.rm = T),
            tau_rel_cTE_ul = quantile(tau_rel_cTE, .975, na.rm = T))

var_names <- colnames(vis.df)
var_summarised_names <- colnames(vis.df_summarised)



x_var_agg_df <- data.frame(CVT = vis.df_summarised$CVT,
                           CVE = vis.df_summarised$CVE,
                           rel = vis.df_summarised$rel,
                           sim_tau_varT = vis.df_summarised$CVT * vis.df_summarised$rel * 10,
                           sim_tau_varE = vis.df_summarised$CVE * (1-vis.df_summarised$rel) * 10)

x_var_agg_choices <- colnames(x_var_agg_df)

y_var_agg_choices <- c("tau_varT", "tau_varE", "tau_rel")

y_var_agg_correspondence <- c("T_m", "E_m", "tau_rel_m")

col_var_agg_choices <- c("CVT", "CVE", "rel")




x_var_disagg_df <- data.frame(CVT = vis.df$CVT,
                              CVE = vis.df$CVE,
                              rel = vis.df$rel,
                              sim_tau_varT = vis.df$CVT * vis.df$rel * 10,
                              sim_tau_varE = vis.df$CVE * (1-vis.df$rel) * 10)

x_var_disagg_choices <- colnames(x_var_disagg_df)

y_var_disagg_choices <- c("tau_varT", "tau_varE", "tau_rel")

y_var_disagg_correspondence <- c("tau_T", "tau_E", "tau_rel")

col_var_disagg_choices <- c("CVT", "CVE", "rel")




vis.df_summarized2 <- vis.df %>%
  group_by(CVT, CVE, rel) %>%
  summarise(bias_varT.M = mean(varT.M - rel*10, na.rm = T),
            bias_varT.MAE = mean(varT.MAE - rel*10, na.rm = T),
            bias_varE.M = mean(varE.M - (1-rel)*10, na.rm = T),
            bias_varE.MAE = mean(varE.MAE - (1-rel)*10, na.rm = T),
            bias_rel.M = mean(rel.M - rel, na.rm = T),
            bias_rel.MAE = mean(rel.MAE-rel, na.rm = T),
            bias_tauT = mean(tau_T - CVT*rel*10, na.rm = T),
            bias_tauE = mean(tau_E - CVE*(1-rel)*10, na.rm = T),
            MSE_varT.M = mean((varT.M - rel*10)^2, na.rm = T),
            MSE_varT.MAE = mean((varT.MAE - rel*10)^2, na.rm = T),
            MSE_varE.M = mean((varE.M - (1-rel)*10)^2, na.rm = T),
            MSE_varE.MAE = mean((varE.MAE - (1-rel)*10)^2, na.rm = T),
            MSE_rel.M = mean((rel.M - rel)^2, na.rm = T),
            MSE_rel.MAE = mean((rel.MAE-rel)^2, na.rm = T),
            MSE_tauT = mean((tau_T - CVT*rel*10)^2, na.rm = T),
            MSE_tauE = mean((tau_E - CVE*(1-rel)*10), na.rm = T)^2)







x_var_bm_df <- data.frame(CVT = vis.df_summarized2$CVT,
                          CVE = vis.df_summarized2$CVE,
                          rel = vis.df_summarized2$rel,
                          sim_tau_varT = vis.df_summarized2$CVT * vis.df_summarized2$rel * 10,
                          sim_tau_varE = vis.df_summarized2$CVE * (1-vis.df_summarized2$rel) * 10)

x_var_bm_choices <- colnames(x_var_bm_df)

y_var_bm_choices <- c("tau_varT", "tau_varE", "rel", "varT", "varE")

y_var_bm_correspondence <- c("tauT", "tauE", "rel", "varT", "varE")

col_var_bm_choices <- c("CVT", "CVE", "rel")




vis.df2 <- vis.df %>%
  mutate(bias_varT.M = (varT.M - rel*10),
         bias_varT.MAE = (varT.MAE - rel*10),
         bias_varE.M = (varE.M - (1-rel)*10),
         bias_varE.MAE = (varE.MAE - (1-rel)*10),
         bias_rel.M = (rel.M - rel),
         bias_rel.MAE = (rel.MAE-rel),
         bias_tauT = (tau_T - CVT*rel*10),
         bias_tauE = (tau_E - CVE*(1-rel)*10)
  )

x_var_bm_da_df <- data.frame(CVT = vis.df2$CVT,
                          CVE = vis.df2$CVE,
                          rel = vis.df2$rel,
                          sim_tau_varT = vis.df2$CVT * vis.df2$rel * 10,
                          sim_tau_varE = vis.df2$CVE * (1-vis.df2$rel) * 10)

x_var_bm_da_choices <- colnames(x_var_bm_da_df)

y_var_bm_da_choices <- c("tau_varT", "tau_varE", "rel", "varT", "varE")

y_var_bm_da_correspondence <- c("tauT", "tauE", "rel", "varT", "varE")

col_var_bm_da_choices <- c("CVT", "CVE", "rel")




# Define UI for application
ui <- navbarPage(
  
  "",

    # Application title
    tabPanel(
      "Simulation Results - aggregates",
      
      
      sidebarPanel(
        selectInput(inputId = "x_variable_agg",
                    label = "Select X-variable.",
                    choices = x_var_agg_choices,
                    selected = "CVT"),
        
        selectInput(inputId = "y_variable_agg",
                    label = "Select Y-variable.",
                    choices = y_var_agg_choices,
                    selected = "tau_varT"),
        
        selectInput(inputId = "row_variable_agg",
                    label = "Select row-variable.",
                    choices = col_var_agg_choices,
                    selected = "CVE"),
        
        selectInput(inputId = "col_variable_agg",
                    label = "Select column-variable.",
                    choices = col_var_agg_choices,
                    selected = "rel")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4("Aggregated Results of Simulation Study on Reliability Heterogeneity"),
        plotOutput("grid_plot_aggregate")
      )
    ),
    
    tabPanel(
      "Simulation Results - disaggregated",
      
      
      
      sidebarPanel(
        selectInput(inputId = "x_variable_disagg",
                    label = "Select X-variable.",
                    choices = x_var_disagg_choices,
                    selected = "CVT"),
        
        selectInput(inputId = "y_variable_disagg",
                    label = "Select Y-variable.",
                    choices = y_var_disagg_choices,
                    selected = "tau_varT"),
        
        selectInput(inputId = "row_variable_disagg",
                    label = "Select row-variable.",
                    choices = col_var_disagg_choices,
                    selected = "CVE"),
        
        selectInput(inputId = "col_variable_disagg",
                    label = "Select column-variable.",
                    choices = col_var_disagg_choices,
                    selected = "rel")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("grid_plot_disaggregate")
      )
    ),
  
    tabPanel(
      "Simulation Results - averaged Bias and MSE",
      
      sidebarPanel(
        
        selectInput(inputId = "bias_mse_choice",
                    label = "Choose bias or MSE.",
                    choices = c("bias", "MSE")),
        
        selectInput(inputId = "x_variable_bm",
                    label = "Select X-variable.",
                    choices = x_var_bm_choices,
                    selected = "CVT"),
        
        selectInput(inputId = "y_variable_bm",
                    label = "Select Y-variable.",
                    choices = y_var_bm_choices,
                    selected = "tau_varT"),
        
        selectInput(inputId = "row_variable_bm",
                    label = "Select row-variable.",
                    choices = col_var_bm_choices,
                    selected = "CVE"),
        
        selectInput(inputId = "col_variable_bm",
                    label = "Select column-variable.",
                    choices = col_var_bm_choices,
                    selected = "rel"),
        
        uiOutput("aggregate_kind_UI")
      ),
      
      mainPanel(h4("Visualising Bias and MSE - aggregated"),
                plotOutput("biasMSEplot"))
    
    ), 
  
    tabPanel(
      
      
      "Simulation Results - Individual Bias",
      
      sidebarPanel(
        
        selectInput(inputId = "x_variable_bm_da",
                    label = "Select X-variable.",
                    choices = x_var_bm_da_choices,
                    selected = "CVT"),
        
        selectInput(inputId = "y_variable_bm_da",
                    label = "Select Y-variable.",
                    choices = y_var_bm_da_choices,
                    selected = "tau_varT"),
        
        selectInput(inputId = "row_variable_bm_da",
                    label = "Select row-variable.",
                    choices = col_var_bm_da_choices,
                    selected = "CVE"),
        
        selectInput(inputId = "col_variable_bm_da",
                    label = "Select column-variable.",
                    choices = col_var_bm_da_choices,
                    selected = "rel"),
        
        uiOutput("aggregate_kind_UI_da")
      ),
      
      mainPanel(h4("Visualising Bias and MSE"),
                plotOutput("biasMSEplot_da"))
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$grid_plot_aggregate <- renderPlot({
        # generate bins based on input$bins from ui.R
      
      y_var <- y_var_agg_correspondence[which(y_var_agg_choices == input$y_variable_agg)]
      y_var_ll <- paste0(substr(y_var, start = 1, stop = nchar(y_var)-1), "ll")
      y_var_ul <- paste0(substr(y_var, start = 1, stop = nchar(y_var)-1), "ul")
      
      ggplot(data = vis.df_summarised) +
        geom_ribbon(aes(x = x_var_agg_df[,input$x_variable_agg],
                        ymin = unlist(vis.df_summarised[,y_var_ll]),
                        ymax = unlist(vis.df_summarised[,y_var_ul])),
                    alpha = .1, fill = "blue") +
        geom_point(aes(x = x_var_agg_df[,input$x_variable_agg], 
                       y = unlist(vis.df_summarised[,y_var]))) +
        geom_line(aes(x = x_var_agg_df[,input$x_variable_agg], 
                      y = unlist(vis.df_summarised[,y_var]))) +
        facet_grid(paste0(input$row_variable_agg, " ~ ", input$col_variable_agg))
    })
    
    output$grid_plot_disaggregate <- renderPlot({
      
      y_var <- y_var_disagg_correspondence[which(y_var_disagg_choices == input$y_variable_disagg)]

      ggplot(vis.df) +
        geom_point(aes(x = x_var_disagg_df[,input$x_variable_disagg], 
                       y = unlist(vis.df[,y_var]),
                       colour = as.factor(x_var_disagg_df[,input$x_variable_disagg])),
                   position = position_jitter(width = .04, height = 0),
                   alpha = .2) +
        facet_grid(paste0(input$row_variable_disagg, " ~ ", input$col_variable_disagg))
      
      
      
    })
    
    
    output$aggregate_kind_UI <- renderUI({
      if(input$y_variable_bm %in% c("rel", "varT", "varE")){
        
        sidebarPanel(
          selectInput(inputId = "aggregate_kind",
                      label = "Choose model.",
                      choices = c("mean", "meta-analytic mean")),
          width = 8
          
        )
        
        
      }
    })
    
    output$aggregate_kind_UI_da <- renderUI({
      if(input$y_variable_bm_da %in% c("rel", "varT", "varE")){
        
        sidebarPanel(
          selectInput(inputId = "aggregate_kind_da",
                      label = "Choose model.",
                      choices = c("mean", "meta-analytic mean")),
          width = 8
          
        )
        
        
      }
    })
    
    
    output$biasMSEplot <- renderPlot({
      
      y_v <- y_var_bm_correspondence[which(y_var_bm_choices == input$y_variable_bm)]
      
      y_var <- paste0(ifelse(input$bias_mse_choice == "bias", "bias_", "MSE_"),
                      y_v)
      
      if(input$y_variable_bm %in% c("rel", "varT", "varE")){
        y_var <- paste0(y_var, ifelse(input$aggregate_kind == "mean", ".M", ".MAE"))
      }
        
      
      ggplot(vis.df_summarized2) +
        geom_point(aes(x = x_var_bm_df[,input$x_variable_bm],
                       y = unlist(vis.df_summarized2[,y_var]))) +
        geom_line(aes(x = x_var_bm_df[,input$x_variable_bm],
                       y = unlist(vis.df_summarized2[,y_var]))) +
        # geom_hline(yintercept = 0) +
        facet_grid(paste0(input$row_variable_bm, " ~ ", input$col_variable_bm))
      
    })
    
    
    output$biasMSEplot_da <- renderPlot({
      
      y_v <- y_var_bm_da_correspondence[which(y_var_bm_da_choices == input$y_variable_bm_da)]
      
      y_var <- paste0("bias_", y_v)
      
      if(input$y_variable_bm_da %in% c("rel", "varT", "varE")){
        y_var <- paste0(y_var, ifelse(input$aggregate_kind_da == "mean", ".M", ".MAE"))
      }
      
      
      ggplot(vis.df2) +
        geom_hline(yintercept = 0, colour = "grey") +
        geom_point(aes(x = x_var_bm_da_df[,input$x_variable_bm_da],
                       y = unlist(vis.df2[,y_var]),
                       colour = as.factor(x_var_bm_da_df[,input$x_variable_bm_da])),
                   position = position_jitter(width = .03, height = 0), alpha = .2) +
        facet_grid(paste0(input$row_variable_bm_da, " ~ ", input$col_variable_bm_da))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
