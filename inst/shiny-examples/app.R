library(shiny)
library(shinydashboard)
library(tibble)
library(dplyr)
library(glue)

login <- shinylogin.tequila::login()

ui <- dashboardPage(
  dashboardHeader(
    title = "shinylogin",
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      login$logoutUI()
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        icon("github"),
        href = "https://github.com/epfl-si/shinylogin.tequila",
        title = "See the code on github"
      )
    )
  ),
  dashboardSidebar(
    collapsed = TRUE,
    div(textOutput("welcome"), style = "padding: 20px")
  ),
  dashboardBody(
    login$loginUI(),
    uiOutput("testUI")
  )
)

server <- function(input, output, session) {
  server <- login$loginServer()

  observe({
    if (server$user()$logged_in) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })

  user_info <- reactive(server$user()$info)

  user_is_admin <- reactive(TRUE)   ## TODO: make up some access control here, e.g. guests vs. regular accounts

  user_data <- reactive({
    req(info <- user_info())

    if (user_is_admin()) {
      dplyr::starwars[, 1:10]
    } else {
      dplyr::storms[, 1:11]
    }
  })

  output$welcome <- renderText({
    req(info <- user_info())

    glue("Welcome {info$username}")
  })

  output$testUI <- renderUI({
    req(info <- user_info())

    fluidRow(
      column(
        width = 12,
        tags$h2(glue("Your data is: {ifelse(user_is_admin(), 'Starwars', 'Storms')}.")),
        box(
          width = NULL,
          status = "primary",
          title = ifelse(user_is_admin(), "Starwars Data", "Storms Data"),
          DT::renderDT(user_data(), options = list(scrollX = TRUE))
        )
      )
    )
  })
}

shiny::shinyApp(ui, server, options = list(port = 3000))
