
#' @rdname shpFileInput
#' @name shpFileInput
#' @title shpFileInput shiny module
#' @description Example shiny module for importing shp files. Uses \link[raster]{shapefile}
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
#' @examples
#' \dontrun{
#'
#' library(shiny)
#'
#' ui = fluidPage(
#'   pageWithSidebar(
#'     headerPanel("Example shiny module: shpFileInput"),
#'     sidebarPanel(shpFileInput("shpfile", label = "A *.shp file")),
#' #    mainPanel(textOutput("plot"))
#'     mainPanel(tableOutput("plot"))
#' #    mainPanel(plotOutput("plot"))
#'   )
#' )
#'
#' server = function(input, output, session){
#'   sp.object = callModule(shpFile, "shpfile")
#' #  output$plot = renderText(sp.object())
#'   output$plot = renderTable(sp.object())
#' #  output$plot = renderPlot(plot(sp.object()))
#' }
#'
#' shinyApp(ui, server)
#' }
#' @importFrom raster shapefile
#' @export
shpFileInput = function(id, label = NULL, width = NULL){
    ns = NS(id)
    tagList(
        fileInput(
            ns("shp_file"),
            label    = label,
            multiple = TRUE,
            accept   = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj"),
            width    = width
        )
    )
}

#' @rdname shpFileInput
#' @name shpFile
#' @export
shpFile = function(input, output, session){
    userFiles = reactive({
        validate(need(input$shp_file, message = FALSE))
        input$shp_file
    })

    observe({
        file.names = userFiles()$name
        dir.names = userFiles()$datapath
        i = stringr::str_detect(file.names, "\\.shp$")
        shp.path = file.path(dir.names[i], file.names[i])
        cat(shp.path, sep = "\n")
    })
    #
    # reactive(shapefile(userFile()$datapath[i]))

    reactive(userFiles()$datapath)
    # reactive(userFiles()$name)
    # reactive(userFiles()$size)
    # reactive(userFiles()$type)
    # reactive({
    #     file.names = userFiles()$name
    #     dir.names = userFiles()$datapath
    #     i = stringr::str_detect(file.names, "\\.shp$")
    #     shp.path = file.path(dir.names[i], file.names[i])
    #     shapefile(shp.path)
    # })
    # reactive({
    #     paste(
    #         userFiles()$datapath,
    #         userFiles()$name,
    #         sep = "/"
    #     )
    # })
    # reactive(userFiles())
}



