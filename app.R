library(shiny)

ui <- fluidPage(
    dataTableOutput("bugs")
)

server <- function(input, output, session) {
    bugs <- open_bugs()
    bugs$bug_id <- link_web(bugs$bug_id)

    output$bugs <- renderDataTable(bugs, options = list(pageLength = 20),
                                   escape = FALSE)
}


shinyApp(ui, server)
