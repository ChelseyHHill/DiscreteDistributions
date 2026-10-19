#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

parameter_tabs <- tabsetPanel(
  id = "params",
  type = "hidden",
  tabPanel("Binomial",
           numericInput("n", "Number of trials (n)", value = 5, min = 1, step = 1),
           numericInput("pi", "Probability of Event of Interest (Pi) ", min = 0, max = 1, value = 0.3, step = 0.1)
  ),
  tabPanel("Poisson", 
           numericInput("lambda", "Mean (Lambda)", value = 3, step = 1, min = 0),
  )
)

# Define UI for application that draws a histogram
ui <- fluidPage(fluidPage(
  navbarPage("Discrete Distributions",
             #tabPanel("Discrete Probability Distributions",
             #          sidebarPanel(
             #            selectInput("freq_type", "Frequency Type", 
             #                        choices = c("Absolute", "Relative")),
             #            numericInput("maximum", "Maximum Discrete Value", value = 5, min = 1, step = 1),
             #            DT::DTOutput("x1"),
             #          ), 
             #          mainPanel(
             #            
             #          )),
             tabPanel("Binomial & Poisson Distributions",
                      sidebarPanel(
                        selectInput("dist", "Distribution", 
                                    choices = c("Binomial", "Poisson")),
                        parameter_tabs),
                      mainPanel(
                        h4("Directions"),
                        p("First, choose the discrete probability distribution (Binomial or Poisson) from the drop-down on the left. Then, fill in the necessary information about the distribution to obtain characteristics and probability, and plot the distribution"),
                        br(),
                        fluidRow(
                          h4("Distribution Characteristics"),
                          column(12, tableOutput("parms"))
                        ),
                        br(),
                        fluidRow(
                          column(4, h4("Probability Distribution"),
                                 tableOutput("disttable")),
                          column(8, h4("Distribution Plot"),
                                 plotOutput("distplot")))
                      )
             ))))


server <- function(input, output){
  
  #freq_ty <- reactive({
  #  switch(input$freq_type,
  #         Absolute = rep(1, length(0:input$maximum)),
  #         Relative = rep(1/length(0:input$maximum), length(0:input$maximum))
  #         )
  #  
  #})
  
  
  #x = data.frame(x = 0:(length(freq_ty())-1), `P(X = x)` = freq_ty(), check.names = FALSE)
  
  #y <- reactiveValues(data = x)
  
  # dtab <- reactive({
  #    switch(input$freq_type,
  #           Absolute = data.frame(x = 0:input$maximum, Frequency = rep(1, length(0:input$maximum)), check.names = FALSE),
  #           Relative = data.frame(x = 0:input$maximum, Probability = round(rep(1/length(0:input$maximum), length(0:input$maximum)), 4), check.names = FALSE)
  #    )
  #  })
  
  #  xy = dtab()
  #  
  #  y <- reactiveValues(data = xy)
  #  output$x1 <- renderDT(
  #    xy,
  #      #data.frame(x = 0:(length(freq_ty())-1), get(col_nms()) = paste(col_nms()[2]) = round(freq_ty(), 4), check.names = FALSE),
  #      selection = "none",
  #      editable = list(target = "cell"), 
  #      rownames = FALSE,
  #      options = list(dom = 't', ordering = FALSE)
  #      )
  
  
  #  proxy = dataTableProxy('x1')
  
  
  #  observeEvent(input$x1_cell_edit, {
  #    info = input$x1_cell_edit
  #    str(info)
  #    y$data <<- editData(y$data, info, rownames = TRUE)
  #    replaceData(proxy, y$data, resetPaging = FALSE)})
  
  
  
  observeEvent(input$dist, {
    updateTabsetPanel(inputId = "params", selected = input$dist)
  }) 
  
  distribution <- reactive({
    switch(input$dist,
           Poisson = dpois(0:20, input$lambda, log = FALSE),
           Binomial = dbinom(0:input$n, input$n, input$pi)
    )
  })
  
  chars <- reactive({
    switch(input$dist,
           Poisson = c(input$lambda, input$lambda, sqrt(input$lambda)),
           Binomial = c((input$n*input$pi), (input$n*input$pi*(1-input$pi)), sqrt((input$n*input$pi*(1-input$pi))))
    )
  })
  
  cum_distribution <- reactive({
    switch(input$dist,
           Poisson = ppois(0:20, input$lambda, log.p = FALSE),
           Binomial = pbinom(0:input$n, input$n, input$pi, log.p = FALSE)
    )
  })
  
  output$distplot <- renderPlot({
    barplot(distribution(), xlab = "", ylab = "", names.arg = 0:(length(distribution())-1), cex.names = 0.75)
  })
  
  output$disttable <- renderTable({
    
    DF <- data.frame(x = 0:(length(distribution())-1), `P(X = x)` = distribution(), CD = cum_distribution(), check.names = FALSE)
    names(DF)[3] <- paste0("P(X ", "\u2264", " x)")
    DF
    
  }, digits = 4)
  
  output$parms <- renderTable({
    data.frame(Parameters = c("Mean (Expected Value)", "Variance", "Standard Deviation"),
               Values = chars(),
               check.names = FALSE)
    
  }, digits = 4)
  
}

# 3. App Launch (This binds the UI and Server together)
shinyApp(ui = ui, server = server)
