library(DT)
library(gior)
library(shiny)

data("country_data")

ui <- fluidPage(
  title = "gior",
  theme = shinythemes::shinytheme("paper"),
  fluidRow(
    column(
      8,
      h2("gior"), br(),
      p("Shiny demo")
    ),
    column(
      4,
      br(),
      tags$a(
        href = "https://github.com/JohnCoene/gior-shiny",
        target = "_blank",
        icon("code"),
        "Source code"
      )
    )
  ),
  fluidRow(
    column(
      2,
      h5("Proxies"), br(),
      actionButton("add", "add"), br(), br(),
      actionButton("clear", "clear"), br(), br(),
      uiOutput("sw")
    ),
    column(10, giorOutput("gior"))
  ),
  br(),
  "Callback",
  fluidRow(
    column(8, h5("Related countries"), DTOutput("rel")),
    column(4, h5("Selected country"), verbatimTextOutput("sel"))
  )
)

server <- function(input, output, session){
  
  #plot
  output$gior <- renderGior({
    country_data %>%
      gior() %>%
      g_data(from, to, value)
  })
  
  #proxies
  output$sw <- renderUI({
    selectInput("selectCountry", NULL, choices = unique(country_data$from))
  })
  
  observeEvent(input$selectCountry, {
    giorProxy("gior") %>%
      g_switch_p(input$selectCountry)
  })
  
  observeEvent(input$clear, {
    giorProxy("gior") %>%
      g_clear_p()
  })
  
  observeEvent(input$add, {
    giorProxy("gior") %>%
      g_data_p(country_data, from, to, value)
  })
  
  # callback
  output$sel <- renderPrint({
    input$gior_selected
  })
  
  output$rel <- renderDT({
    DT::datatable(input$gior_related)
  })
}

shinyApp(ui, server)