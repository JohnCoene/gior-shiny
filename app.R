library(DT)
library(gior)
library(shiny)

data("country_data")

ui <- fluidPage(
  title = "gior",
  theme = shinythemes::shinytheme("lumen"),
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
      p("Change or clear the data"),
      actionButton("add", "change"), br(), br(),
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
  
  cns <- unique(country_data$from)
  
  new_data <- function(){
    data.frame(
      from = sample(cns, 750, replace = TRUE),
      to = sample(cns, 750, replace = TRUE),
      value = runif(750, 100165, 7995879)
    ) %>% 
      dplyr::group_by(from, to) %>% 
      dplyr::summarise(value = sum(value))
  }
  
  #plot
  output$gior <- renderGior({
    country_data %>%
      gior(init.country = "PE") %>%
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
  
  observeEvent(input$rel_rows_selected, {
    if(!is.null(input$rel_rows_selected)){
      x <- input$gior_related %>% 
        dplyr::slice(input$rel_rows_selected) %>% 
        dplyr::pull(ISOCode)
      
      giorProxy("gior") %>%
        g_switch_p(x)
    }
  })
  
  observeEvent(input$clear, {
    giorProxy("gior") %>%
      g_clear_p()
  })
  
  observeEvent(input$add, {
    giorProxy("gior") %>%
      g_data_p(new_data(), from, to, value)
  })
  
  # callback
  output$sel <- renderPrint({
    input$gior_selected
  })
  
  output$rel <- renderDT({
    DT::datatable(input$gior_related, selection = "single")
  })
}

shinyApp(ui, server)