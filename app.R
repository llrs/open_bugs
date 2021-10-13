library(shiny)

last_dump <- "2021-03-25"
library("plotly")

ui <- fluidPage(
    "Open bugs on ", last_dump, "\n",

    dateRangeInput("period", "Bugs within these dates",
                   start = "2012-07-22",
                   min = "2012-07-22",
                   end = last_dump,
                   max = last_dump,
                   weekstart = 1),
    fluidRow(
        splitLayout(plotlyOutput("cc"),
                    plotlyOutput("sv"),
                    plotlyOutput("sa"),
                    cellWidths = c("30%", "30%", "40%"))
    ),
    dataTableOutput("bugs")

)

server <- function(input, output, session) {
    bugs <- open_bugs() |>
        arrange(bug_id) |>
        mutate(bug_id = link_web(bug_id))
    library("plotly")
    bugs_filtered <- reactive(filter(bugs,
                            opened >= input$period[1],
                            opened <= input$period[2]))

    bugs_sel <- reactiveVal(bugs$bug_id)

    # On hover, the key field of the event data contains the car name
    # Add that name to the set of all "selected" cars
    observeEvent(event_data("plotly_click"), {
        bugs_sel <- event_data("plotly_click")$customdata
        bugs_sel_old_new <- c(bugs_sel(), bugs_sel)
        bugs_sel(unique(bugs_sel_old_new))
    })

    # clear the set of cars when a double-click occurs
    observeEvent(event_data("plotly_doubleclick"), {
        bugs_sel(NULL)
    })

    output$cc <- renderPlotly(
        bugs_filtered() |>
            filter(bug_id %in% bug_sel()) |>
            plot_component_core()
        )
                            # alt = "Tile plot with components and if R core has answered.")
    output$sv <- renderPlotly(plot_status_version(bugs_filtered()))
                            # alt = "Tile plot with status and the version of R.")
    output$sa <- renderPlotly(plot_system_attachment(bugs_filtered()))
                            # alt = "Tile plot with the OS used and if it has attachments.")
    output$bugs <- renderDataTable(bugs_filtered(), options = list(pageLength = 20), escape = -1)
}


shinyApp(ui, server)
