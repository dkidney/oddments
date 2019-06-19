
#' @rdname csvFileInput
#' @name csvFileInput
#' @title csvFileInput shiny module
#' @description Example shiny module for importing csv files. Uses \link[utils]{read.csv}
#'   with default arguments.
#'
#' For more info on making shiny modules see:
#' \url{http://shiny.rstudio.com/articles/modules.html}
#' @param id the namespace for the module
#' @param label widget label
#' @param width widget width
#' @param input standard argument to shiny server functions (does not require a default value)
#' @param output standard argument to shiny server functions (does not require a default value)
#' @param session standard argument to shiny server functions (does not require a default value)
#' @export
#' @examples
#' \dontrun{
#'
#' library(shiny)
#'
#' ui = fluidPage(
#'   pageWithSidebar(
#'     headerPanel("Example shiny module: csvFileInput"),
#'     sidebarPanel(csvFileInput("datafile", label = "a label")),
#'     mainPanel(tableOutput("table"))
#'   )
#' )
#'
#' server = function(input, output, session){
#'   datafile = callModule(csvFile, "datafile")
#'   output$table = renderTable(datafile())
#' }
#'
#' shinyApp(ui, server)
#' }
csvFileInput = function(id, label = NULL, width = NULL){
    ns = NS(id)
    tagList(
        fileInput(
            ns("csv_file"),
            label    = label,
            multiple = FALSE,
            accept   = c("text/csv","text/comma-separated-values,text/plain",".csv"),
            width    = width
        )
    )
}

#' @rdname csvFileInput
#' @name csvFile
#' @export
csvFile = function(input, output, session){
    userFile = reactive({
        validate(need(input$csv_file, message = FALSE))
        input$csv_file
    })
    reactive(utils::read.csv(userFile()$datapath))
}
