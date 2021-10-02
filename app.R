library(shiny)

last_dump <- "2021-03-25"

ui <- fluidPage(
    "Last time this was updated was:",
    last_dump,
    "\n",
    dateRangeInput("period", "Bugs within these dates",
                   start = "2012-07-22",
                   min = "2012-07-22",
                   end = last_dump,
                   max = last_dump,
                   weekstart = 1),
    dataTableOutput("bugs")
)

server <- function(input, output, session) {
    bugs <- open_bugs()
    bugs$bug_id <- link_web(bugs$bug_id)

    output$bugs <- renderDataTable(bugs, options = list(pageLength = 20),
                                   escape = FALSE)
}


shinyApp(ui, server)
