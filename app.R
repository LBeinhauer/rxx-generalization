

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

x_var_agg_labels <- c("Coeff.Var. - True Score Variance",
                      "Coeff.Var. - Error Variance",
                      "Reliability",
                      "Sim - tau True Score Variance",
                      "Sim - tau Error Variance")

y_var_agg_choices <- c("tau_varT", "tau_varE", "tau_rel")

y_var_agg_labels <- c("Est. tau True Score Variance",
                      "Est. tau Error Variance",
                      "Est. tau Reliability")

y_var_agg_correspondence <- c("T_m", "E_m", "tau_rel_m")

col_var_agg_choices <- c("CVT", "CVE", "rel")

col_var_agg_labels <- c("Coeff.Var. - True Score Variance",
                        "Coeff.Var. - Error Variance",
                        "Reliability")




x_var_disagg_df <- data.frame(CVT = vis.df$CVT,
                              CVE = vis.df$CVE,
                              rel = vis.df$rel,
                              sim_tau_varT = vis.df$CVT * vis.df$rel * 10,
                              sim_tau_varE = vis.df$CVE * (1-vis.df$rel) * 10)

x_var_disagg_choices <- colnames(x_var_disagg_df)

x_var_disagg_labels <- c("Coeff.Var. - True Score Variance",
                         "Coeff.Var. - Error Variance",
                         "Sim - Reliability",
                         "Sim - tau True Score Variance",
                         "Sim - tau Error Variance")

y_var_disagg_choices <- c("tau_varT", "tau_varE", "tau_rel")

y_var_disagg_labels <- c("Est. tau True Score Variance",
                         "Est. tau Error Variance",
                         "Est. tau Reliability")


y_var_disagg_correspondence <- c("tau_T", "tau_E", "tau_rel")

col_var_disagg_choices <- c("CVT", "CVE", "rel")

col_var_disagg_labels <- c("Coeff.Var. - True Score Variance",
                           "Coeff.Var. - Error Variance",
                           "Reliability")



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

x_var_bm_labels <- c("Coeff.Var. - True Score Variance",
                     "Coeff.Var. - Error Variance",
                     "Sim - Reliability",
                     "Sim - tau True Score Variance",
                     "Sim - tau Error Variance")

y_var_bm_choices <- c("tau_varT", "tau_varE", "rel", "varT", "varE")

y_var_bm_labels <- c(" - Est. tau True Score Variance",
                     " - Est. tau Error Variance",
                     " - Est. Reliability ",
                     " - Est. True Score Variance ",
                     " - Est. Error Variance ")

y_var_bm_correspondence <- c("tauT", "tauE", "rel", "varT", "varE")

col_var_bm_choices <- c("CVT", "CVE", "rel")

col_var_bm_labels <- c("Coeff.Var. - True Score Variance",
                       "Coeff.Var. - Error Variance",
                       "Reliability")



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

x_var_bm_da_labels <- c("Coeff.Var. - True Score Variance",
                        "Coeff.Var. - Error Variance",
                        "Sim - Reliability",
                        "Sim - tau True Score Variance",
                        "Sim - tau Error Variance")

y_var_bm_da_choices <- c("tau_varT", "tau_varE", "rel", "varT", "varE")

y_var_bm_da_labels <- c(" - Est. tau True Score Variance",
                        " - Est. tau Error Variance",
                        " - Est. Reliability ",
                        " - Est. True Score Variance ",
                        " - Est. Error Variance ")

y_var_bm_da_correspondence <- c("tauT", "tauE", "rel", "varT", "varE")

col_var_bm_da_choices <- c("CVT", "CVE", "rel")

col_var_bm_da_labels <- c("Coeff.Var. - True Score Variance",
                          "Coeff.Var. - Error Variance",
                          "Reliability")


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
                    selected = "rel"),
        
        uiOutput("correction_UI"),
        
        width = 2
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4("Aggregated Results of Simulation Study on Reliability Heterogeneity"),
        plotOutput("grid_plot_aggregate"),
        
        width = 10
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
                    selected = "rel"),
        
        checkboxInput(inputId = "density_disagg",
                      label = "Visualise Density",
                      value = FALSE),
        
        width = 2
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4("Disaggregated Results of Simulation Study on Reliability Heterogeneity"),
        plotOutput("grid_plot_disaggregate"),
        
        width = 10
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
        
        uiOutput("aggregate_kind_UI"),
        
        width = 2
      ),
      
      mainPanel(h4("Visualising Bias and MSE - aggregated"),
                plotOutput("biasMSEplot"),
                
                width = 10)
    
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
        
        uiOutput("aggregate_kind_UI_da"),
        
        width = 2
      ),
      
      mainPanel(h4("Visualising Bias and MSE"),
                plotOutput("biasMSEplot_da"),
                
                width = 10)
    
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$grid_plot_aggregate <- renderPlot({
        # generate bins based on input$bins from ui.R
      
      y_var <- y_var_agg_correspondence[which(y_var_agg_choices == input$y_variable_agg)]
      
      if(input$y_variable_agg == "tau_rel"){
        if(input$correction_agg == "true score"){
          y_var <- paste0(substr(y_var, start = 1, stop = nchar(y_var)-2), "_cT_m")
        }
        if(input$correction_agg == "error"){
          y_var <- paste0(substr(y_var, start = 1, stop = nchar(y_var)-2), "_cE_m")
        }
        if(input$correction_agg == "both"){
          y_var <- paste0(substr(y_var, start = 1, stop = nchar(y_var)-2), "_cTE_m")
        }
      }
      
      y_var_ll <- paste0(substr(y_var, start = 1, stop = nchar(y_var)-1), "ll")
      y_var_ul <- paste0(substr(y_var, start = 1, stop = nchar(y_var)-1), "ul")
      
      label_x <- x_var_agg_labels[which(x_var_agg_choices == input$x_variable_agg)]
      label_y <- y_var_agg_labels[which(y_var_agg_choices == input$y_variable_agg)]
      label_col <- col_var_agg_labels[which(col_var_agg_choices == input$col_variable_agg)]
      label_row <- col_var_agg_labels[which(col_var_agg_choices == input$row_variable_agg)]
      
      
      if(input$y_variable_agg == "tau_rel"){
        if(input$correction_agg == "none"){
          label_y <- paste0(label_y, " (corr: ", input$correction_agg, ")")
        }else{
          label_y <- paste0(label_y, " (corr: ", input$correction_agg, " var.)")
        }
      }
      
      
      ggplot(data = vis.df_summarised) +
        geom_ribbon(aes(x = x_var_agg_df[,input$x_variable_agg],
                        ymin = unlist(vis.df_summarised[,y_var_ll]),
                        ymax = unlist(vis.df_summarised[,y_var_ul])),
                    alpha = .1, fill = "blue") +
        geom_point(aes(x = x_var_agg_df[,input$x_variable_agg], 
                       y = unlist(vis.df_summarised[,y_var]))) +
        geom_line(aes(x = x_var_agg_df[,input$x_variable_agg], 
                      y = unlist(vis.df_summarised[,y_var]))) +
        facet_grid(paste0(input$row_variable_agg, " ~ ", input$col_variable_agg)) +
        labs(x = label_x, y = label_y, 
             subtitle = paste0("Rows = ", label_row, "; Columns = ", label_col)) +
        theme(text = element_text(size = 15))
      
      
    }, height = function() {
      session$clientData$output_grid_plot_aggregate_width*.5
    })
    
    output$grid_plot_disaggregate <- renderPlot({
      
      if(!input$density_disagg){
        y_var <- y_var_disagg_correspondence[which(y_var_disagg_choices == input$y_variable_disagg)]
        
        label_f <- x_var_disagg_labels[which(x_var_disagg_choices == input$x_variable_disagg)]
        label_fill <- paste(str_split(label_f, " - ")[[1]], collapse = " \n")
        label_x <- x_var_disagg_labels[which(x_var_disagg_choices == input$x_variable_disagg)]
        label_y <- y_var_disagg_labels[which(y_var_disagg_choices == input$y_variable_disagg)]
        label_col <- col_var_disagg_labels[which(col_var_disagg_choices == input$col_variable_disagg)]
        label_row <- col_var_disagg_labels[which(col_var_disagg_choices == input$row_variable_disagg)]
        
        ggplot(vis.df) +
          geom_point(aes(x = x_var_disagg_df[,input$x_variable_disagg], 
                         y = unlist(vis.df[,y_var]),
                         colour = as.factor(x_var_disagg_df[,input$x_variable_disagg])),
                     position = position_jitter(width = .04, height = 0),
                     alpha = .2) +
          facet_grid(paste0(input$row_variable_disagg, " ~ ", input$col_variable_disagg)) +
          labs(x = label_x, y = label_y, colour = label_fill,
               subtitle = paste0("Rows = ", label_row, "; Columns = ", label_col)) +
          theme(text = element_text(size = 15))
      }else{
        y_var <- y_var_disagg_correspondence[which(y_var_disagg_choices == input$y_variable_disagg)]
        
        label_f <- x_var_disagg_labels[which(x_var_disagg_choices == input$x_variable_disagg)]
        label_fill <- paste(str_split(label_f, " - ")[[1]], collapse = " \n")
        label_x <- y_var_disagg_labels[which(y_var_disagg_choices == input$y_variable_disagg)]
        label_col <- col_var_disagg_labels[which(col_var_disagg_choices == input$col_variable_disagg)]
        label_row <- col_var_disagg_labels[which(col_var_disagg_choices == input$row_variable_disagg)]
        
        ggplot(vis.df) +
          geom_density(aes(fill = as.factor(x_var_disagg_df[,input$x_variable_disagg]), 
                           x = unlist(vis.df[,y_var]),
                           group = as.factor(x_var_disagg_df[,input$x_variable_disagg])),
                       alpha = .3) +
          facet_grid(paste0(input$row_variable_disagg, " ~ ", input$col_variable_disagg)) +
          labs(x = label_x, y = "Density", fill = label_fill, group = label_fill,
               subtitle = paste0("Rows = ", label_row, "; Columns = ", label_col)) +
          theme(text = element_text(size = 15))
      }
      
      
      
      
      
    }, height = function() {
      session$clientData$output_grid_plot_disaggregate_width*.5
    })
    
    
    
    output$correction_UI <- renderUI({
      if(input$y_variable_agg == "tau_rel"){
        
        sidebarPanel(
          selectInput(inputId = "correction_agg",
                      label = "Choose Variance \ncorrection mechanism.",
                      choices = c("none", "true score", "error", "both")),
          width = 8
          
        )
        
        
      }
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
      
      label_x <- x_var_bm_labels[which(x_var_bm_choices == input$x_variable_bm)]
      label_y <- paste0(ifelse(input$bias_mse_choice == "bias", "Bias", "MSE"), 
                        y_var_bm_labels[which(y_var_bm_choices == input$y_variable_bm)])
      label_col <- col_var_bm_labels[which(col_var_bm_choices == input$col_variable_bm)]
      label_row <- col_var_bm_labels[which(col_var_bm_choices == input$row_variable_bm)]
      
      if(input$y_variable_bm %in% c("rel", "varT", "varE")){
        y_var <- paste0(y_var, ifelse(input$aggregate_kind == "mean", ".M", ".MAE"))
        
        label_y <- paste0(label_y, ifelse(input$aggregate_kind == "mean", "(mean)", "(meta-analytic)"))
      }
        
      
      ggplot(vis.df_summarized2) +
        geom_hline(yintercept = 0, colour = "grey") +
        geom_point(aes(x = x_var_bm_df[,input$x_variable_bm],
                       y = unlist(vis.df_summarized2[,y_var]))) +
        geom_line(aes(x = x_var_bm_df[,input$x_variable_bm],
                       y = unlist(vis.df_summarized2[,y_var]))) +
        facet_grid(paste0(input$row_variable_bm, " ~ ", input$col_variable_bm)) +
        labs(x = label_x, y = label_y, 
             subtitle = paste0("Rows = ", label_row, "; Columns = ", label_col)) +
        theme(text = element_text(size = 15))
      
    }, height = function() {
      session$clientData$output_biasMSEplot_width*.5
    })
    
    
    output$biasMSEplot_da <- renderPlot({
      
      y_v <- y_var_bm_da_correspondence[which(y_var_bm_da_choices == input$y_variable_bm_da)]
      
      y_var <- paste0("bias_", y_v)
      
      label_f <- x_var_bm_da_labels[which(x_var_bm_da_choices == input$x_variable_bm_da)]
      label_fill <- paste(str_split(label_f, " - ")[[1]], collapse = " \n")
      label_x <- x_var_bm_da_labels[which(x_var_bm_da_choices == input$x_variable_bm_da)]
      label_y <- paste0("Bias", y_var_bm_da_labels[which(y_var_bm_da_choices == input$y_variable_bm_da)])
      label_col <- col_var_bm_da_labels[which(col_var_bm_da_choices == input$col_variable_bm_da)]
      label_row <- col_var_bm_da_labels[which(col_var_bm_da_choices == input$row_variable_bm_da)]
      
      
      if(input$y_variable_bm_da %in% c("rel", "varT", "varE")){
        y_var <- paste0(y_var, ifelse(input$aggregate_kind_da == "mean", ".M", ".MAE"))
        
        label_y <- paste0(label_y, ifelse(input$aggregate_kind_da == "mean", "(mean)", "(meta-analytic)"))
      }
      
      
      ggplot(vis.df2) +
        geom_hline(yintercept = 0, colour = "grey") +
        geom_point(aes(x = x_var_bm_da_df[,input$x_variable_bm_da],
                       y = unlist(vis.df2[,y_var]),
                       colour = as.factor(x_var_bm_da_df[,input$x_variable_bm_da])),
                   position = position_jitter(width = .03, height = 0), alpha = .2) +
        facet_grid(paste0(input$row_variable_bm_da, " ~ ", input$col_variable_bm_da)) +
        labs(x = label_x, y = label_y, colour = label_fill,
             subtitle = paste0("Rows = ", label_row, "; Columns = ", label_col)) +
        theme(text = element_text(size = 15))
      
    }, height = function() {
      session$clientData$output_biasMSEplot_da_width*.5
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
