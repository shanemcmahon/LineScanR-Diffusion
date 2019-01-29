library(shiny); library(tiff)
# Define UI for app
row1 <- fluidRow(
  column(fileInput(inputId="fileInputInfo",label = "Linescan tiff file", placeholder = "linescan.tif", buttonLabel = "Open Linescan",accept = c("image/tiff" )), width = 4),
  # column(downloadButton(outputId = "downloadData"),width = 4)
  column(downloadLink("downloadData"),width = 4)
)
ui <- fluidPage(
  # App title ----
  row1,
  fluidRow(
    column(numericInput("n.channels", "n channels", 1, min = 1, max = NA, step = 1),width = 2),
    column(numericInput("LinePeriod", "Line Period (ms)", 1.05006577086101, min = 0, max = NA),width = 2),
    column(numericInput("PixelSize", "Pixel Size (nm)", 77.783203125, min = 0, max = NA),width = 2),
    column(actionButton("ResetX", "Reset x range"),width = 2)
  ),
  # fluidRow(plotOutput("LineScanImage",brush="LineScanImageBrush",click = "LineScanImageClick",dblclick = "LineScanImageDblClick",hover = "LineScanImageHover")),
  fluidRow(plotOutput("LineScanImage",brush=brushOpts(id="LineScanImageBrush",delayType = "debounce",delay = .Machine$integer.max),click = "LineScanImageClick",dblclick = "LineScanImageDblClick",hover = "LineScanImageHover")),
  fluidRow(
    column((imageOutput(outputId = "xProfilePlot",brush = brushOpts(id="xProfileBrush",delayType = "debounce",delay = .Machine$integer.max,resetOnNew = TRUE,direction = "x"),click = "xProfileClick", dblclick = "xProfileDblClick")),width = 6),
    column(imageOutput(outputId = "tProfilePlot",brush = brushOpts(id="tProfileBrush",delayType = "debounce",delay = .Machine$integer.max,resetOnNew = TRUE,direction = "x")),width = 6)
  ),
  fluidRow(actionButton("SpatialNorm","Normalize Fluorescence Profile")),
  fluidRow(plotOutput("LineScanNorm",brush=brushOpts(id="LineScanNormBrush",delayType = "debounce",delay = .Machine$integer.max),click = "LineScanNormClick",dblclick = "LineScanNormDblClick",hover = "LineScanNormHover")),
  fluidRow(
    column((imageOutput(outputId = "xProfileNorm")),width = 6),
    column(imageOutput(outputId = "tProfileNorm",brush=brushOpts(id="tProfileNormBrush",delayType = "debounce",delay = .Machine$integer.max,resetOnNew = TRUE,direction = "x")),width = 6)
  ),
  fluidRow(
    column(numericInput("SpatialWindow", "Spatial Window", 1, min = 1, max = NA, step = 1),width = 2),
    column(numericInput("TemporalWindow", "Time Window", 1, min = 1, max = NA, step = 1),width = 2),
    fluidRow(actionButton("CalculateDiffusion","Calculate Diffusion Properties"))
           ),
  fluidRow(shiny::textInput(inputId = "InputPars",label = "InputPars")),
  # fluidRow(plotOutput("LSDiffusion")),
  fluidRow(
    column((imageOutput(outputId = "xProfileDiffusion")),width = 6),
    column(imageOutput(outputId = "tProfileDiffusion"),width = 6)
  ),
  # fluidRow(textOutput(outputId = "D_app")),
  fluidRow(column((imageOutput(outputId = "DiffCoefs")),width = 6)),
  fluidRow(tableOutput("DiffCoefTable")),
  # fluidRow(verbatimTextOutput(outputId = "dput")),
  padding = 0
)

