#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# to dos ####
# methods or about tab (simulation conditions, more info on what )
# side panel of perf metrics (helpful flavor text on each page, probs under title of each box)
# ^^ could use a separate box (htmlOutput to dynamically update text; use renderText)
# fit model and pull AR estimates and add to IIV indicator
# MC error tab with MC results lol --> w7 from sim class
# References table
# maybe some type of reveal option by (none, sample size, or by sampling frequency) --> animated graphic
#   could create tabs (static, dynamic-1 sample size, dynamic-2 sf); have tabs on left side
#   or focus on a means of comparison between indicators (check box for each one to display)

# packages ####
# could create a "source script" and call it in with source("file-name.R") and it will run all those packages!
library(shiny) # app
library(shinydashboard) # will be nice for creating tabs (MC error, raw data tables, models fit, etc.)
library(shinyWidgets)
library(tidyverse) # piping and ggplot
library(ghibli)
library(DT) # data tables

#naming --> slide 39 in week 9
# 

# data ####
load("data\\results-data-arima.RData")
load("data\\outcome-data-arima.RData")
results.long <- results.plot |> 
  rename("ar" = "dt-ar (phi)") |> 
  pivot_longer(cols = ivar:ar,
               names_to = "indicator")
outcome.long <- outcome.plot |> 
    pivot_longer(
        cols = c(5:11)
    ) |> 
    filter(data == "sampled") |> 
  mutate(indicator = factor(indicator,
                            levels = c("ivar", "isd", "imean", "auto.cor", "dt-ar (phi)"),
                            labels = c("ivar", "isd", "imean", "auto.cor", "ar")))

# custom theme for plots ####
mls_theme <- function(){
  theme_bw() %+replace%
    
    theme(
      
      # text elements
      plot.title = element_text(
        size = rel(1.5),
        face = 'bold',
        hjust = 0.5,
        vjust = 3),
      plot.subtitle = element_text(
        size = rel(1.1)),
      axis.title = element_text(
        size = rel(1.3),
        face = 'bold'),
      axis.text = element_text(
        size = rel(1.3)),
      
      # panel background
      
      panel.background = element_rect(
        fill = "#FFFFFF",
        color = "#534C53FF"),
      
      # legend
      
      legend.position	= "bottom",
      legend.title = element_text(face = "bold",
                                  size = rel(1.1)),
      legend.text = element_text(size = rel(1.1)),
      
      # facet grid
      
      strip.text.x = element_text(
        size = rel(1.3),
        face = "bold",
        color = "white"),
      strip.text.y.right = element_text(
        size = rel(1.3),
        face = "bold",
        color = "white"
        ),
      strip.background.x =
              element_rect(
                fill = "#1F273EFF",
                color = "#1F273EFF"
      ),
      strip.background.y = element_rect(
        fill = "#3D4F7DFF",
        color = "#3D4F7DFF"
      )
      )
}
gg.fill <- scale_fill_manual(values = c("sampled" = "#3D4F7DFF", "dgp" = "#B3B8B1FF"),
                             labels = c("Hourly DGP", "Sampled"))
gg.color <- scale_color_manual(values = c("ivar" = "#E48C2AFF", "imean" = "#B3B8B1FF",
                                          "isd" = "#EAD890FF", "auto.cor" = "#3D4F7DFF",
                                          "ar" = "#657060FF"))

ghibli_palettes$MononokeMedium

# ui dashboard ####
sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(" Method", tabName = "method", icon = icon("list-ol")),
      menuItem(" Results", tabName = "results", icon = icon("dashboard")),
      menuItem(" Metrics Table", icon = icon("table"), tabName = "datatable")
    )
  )

body <- dashboardBody(
  tabItems(
    tabItem("method",
            fluidRow(
              box(title = "Simulation Approach",
                htmlOutput("sim.method"),  
                width=12)
              )
            ),
    
    tabItem("results",
      fluidRow(
               box(
          title = "Indicator Box",
          selectInput(inputId = "indicator",
                          label = "Intraindividual Variability Indicator",
                          choices = c("iVAR" = "ivar",
                                      "iSD" = "isd",
                                      "iMean" = "imean",
                                      "Autocorrelation" = "auto.cor",
                                      "AR(1)" = "ar"),
                          selected = "iVar"),
          selectizeInput(inputId = "underlying.data",
                         label="Data Used",
                         choices = c("Hourly Data Generating Process" = "dgp",
                                     "Sampled Data Points" = "sampled"),
                         select = "Sampled Data Points",
                         multiple = TRUE,
                         options = list(placeholder = 'Click here to select', 
                                'plugins' = list('remove_button'))
                 ),
          width = 4), # overall width is 12
          box(
            title = "Distribution of Statistics (N=1000)",
            plotOutput("distPlot"),
              width = 8)
          ),
      fluidRow(
        box(
          title = "Performance Metrics",
          selectizeInput(inputId = "indicator.perf",
                         label="Indicator (select multiple):",
                  choices = c("iVAR" = "ivar",
                              "iSD" = "isd",
                              "iMean" = "imean",
                              "Autocorrelation" = "auto.cor",
                              "AR(1)" = "ar"),
                  selected = "iVAR",
                 multiple = TRUE,
                 options = list(placeholder = 'Click here to select', 
                                'plugins' = list('remove_button'))
                 ),
          selectInput(inputId = "name",
                          label = "Performance Metrics",
                          choices = c("Absolute Bias" = "abs.bias",
                                      "Absolute Relative Bias" = "abs.rel.bias",
                                      "Relative Bias" = "rel.bias",
                                      "Mean Squared Error" = "mse",
                                      "Root MSE" = "rmse",
                                      "Efficiency" = "var.eff",
                                      "Mean" = "mean"),
                          selected = "Absolute Relative Bias"),
          # uiOutput('perfFormula'),
          width=4),
        box(
          title = "Performance Metrics",
          plotOutput("perfPlot"),
            width = 8)
        )
      ),
  
    tabItem("datatable",
    fluidRow(
      box(
        title = "Data Table",
        dataTableOutput("outcomeTable"),
          width = 12)
      )
    )
  )
)
ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Intraindividual Variability",
                  titleWidth = 300),
  sidebar,
  body
)

# server ####
server <- function(input, output) {
  
    selected_dist <- reactive({
      results.long |> 
        filter(indicator == input$indicator,
               data %in% input$underlying.data)
      })
    output$distPlot <- renderPlot({
      
          ggplot(data = selected_dist(),
                 aes(x=value, fill=data)) +
            geom_vline(
              data = results.long |> 
                filter(data == "dgp"),
              xintercept = mean(input$indicator),
                       color = "#CD4F38FF"
                       ) +
            stat_density(
              mapping = aes(), 
              alpha = .7,
              geom = "area",
                position = "identity") + # identity needed to distinguish groups
            scale_y_continuous(breaks = c(0, 10)) +
              coord_cartesian(ylim = c(0, 20))  +
        facet_grid(n.size ~ samp.freq) +
            mls_theme() +
            gg.fill +
            labs(title = paste0("Distribution of ", input$indicator, " Estimates")) +
            xlab("Estimate") + 
            ylab("Density")
       
            })
    
    selected_data <- reactive({
      outcome.long |> 
        filter(indicator %in% input$indicator.perf,
               name %in% input$name) |> 
        mutate(n.size = factor(n.size,
                               levels = c("n=30", "n=60", "n=90", "n=120", "n=150"),
                               labels = c("30", "60", "90", "120", "150")))
      })
          
    output$perfPlot <- renderPlot({

      ggplot(data = selected_data(), 
               aes(x=n.size, y=value,
                   color=indicator, group=indicator)) +
        geom_line(linewidth =2) +
        geom_point(size = 3) +
        gg.color+
        facet_grid(~samp.freq,
                   scales = "free") +
        mls_theme() +
        labs(
          title = paste0(input$name),
          y = "Value",
          x = "Sample Size")

    })
    
    # output$perfFormula <- renderUI({
    # 
    #   if(input$name == "abs.bias"){
    #   withMathJax(
    #     HTML("Absolute Bias Formula: $\frac{1}{n_{reps}}\sum_{i=1}^{n_{reps}} |\hat{\theta_i} - \theta|$")
    #     )
    #   }
    # 
    #   if(input$name == "abs.rel.bias"){
    #   withMathJax(
    #     HTML("Absolute Relative Bias Formula: $\frac{1}{n_{reps}}\sum_{i=1}^{n_{reps}} \frac{|\hat{\theta_i} - \theta|}{\theta}$")
    #     )
    #   }
    # 
    #   if(input$name == "rel.bias"){
    #   withMathJax(
    #     HTML("Relative Bias Formula: $\frac{1}{n_{reps}}\sum_{i=1}^{n_{reps}} \frac{(\hat{\theta_i} - \theta)}{\theta}$")
    #     )
    #   }
    # 
    #   if(input$name == "mse"){
    #   withMathJax(
    #     HTML("Mean Squared Error Formula: $\frac{1}{n_{reps}}\sum_{i=1}^{n_{reps}} (\hat{\theta_i} - \theta)^2$")
    #     )
    #   }
    # 
    #   if(input$name == "rmse"){
    #   withMathJax(
    #     HTML("Root Mean Squared Error Formula: $\sqrt{MSE}$")
    #     )
    #   }
    # 
    #   if(input$name == "var.eff"){
    #   withMathJax(
    #     HTML("Raw Efficiency Formula: $var(\hat{\theta_i}) = \frac{1}{n_[reps}-1} \sum_i^{n_{reps}}(\hat{\theta_i} - \bar{\hat{\theta}})^2$")
    #     )
    #   }
    # 
    #   if(input$name == "mean"){
    #   withMathJax(
    #     HTML("Mean Formula: $\frac{\sum_i^{n_{reps}} \hat{\theta_i}}{n_[reps}}$")
    #     )
    #   }
    # })
    
    output$outcomeTable <- DT::renderDataTable({
  
      DT::datatable(outcome.plot |> 
                          mutate(abs.bias = round(abs.bias, digits = 3),
                                 abs.rel.bias = round(abs.rel.bias, digits = 3),
                                 rel.bias = round(rel.bias, digits = 3),
                                 mse = round(mse, digits = 3),
                                 rmse = round(rmse, digits = 3),
                                 var.eff = round(var.eff, digits = 3),
                                 mean = round(mean, digits = 3))
      , class = 'cell-border stripe', filter = 'top', rownames = FALSE)
        })
    
    output$sim.method <- renderText({
      HTML("
<article>
  <p>
  For each of the below indicators of intraindividual variability, a 5(Sampling Frequency) x 5(Number of Observations) combination of conditions were ran.
  </p>
<br>
<table>
  <tr>
    <th>IIV Indicators</th>
    <th>Sampling Frequency</th>
    <th>Number of Observations</th>
  </tr>
  <tr>
    <td>Variance</td>
    <td>3x Daily</td>
    <td>30</td>
  </tr>
  <tr>
    <td>SD</td>
    <td>2x Daily</td>
    <td>60</td>
  </tr>
  <tr>
    <td>Mean</td>
    <td>1x Daily</td>
    <td>90</td>
  </tr>
  <tr>
    <td>Autocorrelation</td>
    <td>2x Weekly</td>
    <td>120</td>
  </tr>
  <tr>
    <td>AR(1) Estimate</td>
    <td>1x Weekly</td>
    <td>150</td>
  </tr>
</article>
           ")
    })
}

# Run the application ####
shinyApp(ui = ui, server = server)


