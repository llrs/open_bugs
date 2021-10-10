library(shiny)

last_dump <- "2021-03-25"

ui <- fluidPage(
    "Open bugs on ", last_dump, "\n",

    dateRangeInput("period", "Bugs within these dates",
                   start = "2012-07-22",
                   min = "2012-07-22",
                   end = last_dump,
                   max = last_dump,
                   weekstart = 1),
    fluidRow(
        splitLayout(plotOutput("cc"),
                    plotOutput("sv"),
                    plotOutput("sa"),
                    cellWidths = c("30%", "30%", "40%"))
    ),
    dataTableOutput("bugs")
)

server <- function(input, output, session) {
    bugs <- open_bugs() |>
        arrange(bug_id) |>
        mutate(bug_id = link_web(bug_id))
    output$cc <- renderPlot(plot_component_core(bugs),
                            alt = "Tile plot with components and if R core has answered.")
    output$sv <- renderPlot(plot_status_version(bugs),
                            alt = "Tile plot with status and the version of R.")
    output$sa <- renderPlot(plot_system_attachment(bugs),
                            alt = "Tile plot with the OS used and if it has attachments.")
    output$bugs <- renderDataTable(bugs, options = list(pageLength = 20), escape = -1)
}


shinyApp(ui, server)
