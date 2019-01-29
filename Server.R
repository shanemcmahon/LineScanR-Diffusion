# Define server logic required to draw a histogram ----
source("libLineScanR.R")
server <- function(input, output, session) {
    library(stringr)
    observeEvent(eventExpr = input$fileInputInfo, handlerExpr = HandleFileInput(input, output, session))
    observeEvent(eventExpr = input$LineScanImageBrush, handlerExpr = HandleLsImageBrush(input, output, session))
    observeEvent(eventExpr = input$SpatialNorm, handlerExpr=SpatialNormClick(input,output,session))
    observeEvent(eventExpr = input$LineScanNormBrush, handlerExpr = HandleLsNormBrush(input, output, session))
    observeEvent(eventExpr = input$CalculateDiffusion, handlerExpr = HandleCalculateDiffusion(input, output, session))
    observeEvent(eventExpr = input$xProfileBrush, handlerExpr = HandleXProfileBrush(input, output, session))
    observeEvent(eventExpr = input$tProfileBrush, handlerExpr = HandletProfileBrush(input, output, session))
    observeEvent(eventExpr = input$tProfileNormBrush, handlerExpr = HandletProfileNormBrush(input, output, session))
    observeEvent(eventExpr = input$InputPars, handlerExpr = HandleInputPars(input, output, session))
}

