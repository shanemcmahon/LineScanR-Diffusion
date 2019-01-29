HandleFileInput <- function(input, output, session) {
  
  LSimage <<- readTIFF(input$fileInputInfo$datapath, info = TRUE, as.is = TRUE)
  output.csv <<- (str_replace(string = input$fileInputInfo$name, pattern = "tif", replacement = "csv"))
  # render LineScanImage
  output$LineScanImage <- renderPlot({
    image(LSimage, col = rainbow(256), axes = FALSE)
  })
  
  xProfile <<- colMeans(LSimage)
  output$xProfilePlot <- renderPlot({
    plot(xProfile, ylab = "F", ylim = c(0, 1.1 * max(xProfile)))
  })
  tProfile <<- rowMeans(LSimage)
  output$tProfilePlot <- renderPlot({
    plot(tProfile, type = "l", ylab = "F")
  })
  
  output$downloadData <- downloadHandler(filename = function() {
    output.csv
  }, content = function(con) {
    write.csv(tProfile, con)
  })
}


HandleLsImageBrush <- function(input, output, session) {
  selectedData <- unlist(input$LineScanImageBrush)
  t.win0 <<- floor(max(((as.numeric(selectedData[1]) - input$LineScanImageBrush$domain$left) * nrow(LSimage)), 1))
  t.win1 <<- floor(min((as.numeric(selectedData[2]) - input$LineScanImageBrush$domain$left) * nrow(LSimage), nrow(LSimage)))
  x.win0 <<- floor(max((as.numeric(selectedData[3]) - input$LineScanImageBrush$domain$bottom) * ncol(LSimage), 1))
  x.win1 <<- floor(min((as.numeric(selectedData[4]) - input$LineScanImageBrush$domain$bottom) * ncol(LSimage), ncol(LSimage)))
  if (t.win1 <= t.win0) {
    return(NULL)
  }
  if (x.win1 <= x.win0) {
    return(NULL)
  }
  
  xProfile <<- colMeans(LSimage[t.win0:t.win1, x.win0:x.win1])
  output$xProfilePlot <- renderPlot({
    plot(x.win0:x.win1, xProfile, ylab = "F", ylim = c(0, 1.1 * max(xProfile)))
  })
  tProfile <<- rowMeans(LSimage[t.win0:t.win1, x.win0:x.win1])
  output$tProfilePlot <- renderPlot({
    plot(t.win0:t.win1, tProfile, type = "l", ylab = "F")
  })
  
  output$downloadData <- downloadHandler(filename = function() {
    output.csv
  }, content = function(con) {
    write.csv(tProfile, con)
  })
}

SpatialNormClick <- function(input, output, session) {
  LSNorm <<- t(t(LSimage[, x.win0:x.win1])/xProfile)
  
  output$LineScanNorm <- renderPlot({
    image(LSNorm, col = rainbow(256), axes = FALSE)
  })
  xProfileNorm <<- colMeans(LSNorm)
  output$xProfileNorm <- renderPlot({
    plot(xProfileNorm, ylab = "F")
  })
  tProfileNorm <<- rowMeans(LSNorm)
  output$tProfileNorm <- renderPlot({
    plot(tProfileNorm, type = "l", ylab = "F")
  })
}

HandleLsNormBrush <- function(input, output, session) {
  selectedData <- unlist(input$LineScanNormBrush)
  t.win0 <- floor(max(((as.numeric(selectedData[1]) - input$LineScanNormBrush$domain$left) * nrow(LSNorm)), 1))
  t.win1 <- floor(min((as.numeric(selectedData[2]) - input$LineScanNormBrush$domain$left) * nrow(LSNorm), nrow(LSNorm)))
  x.win0 <- floor(max((as.numeric(selectedData[3]) - input$LineScanNormBrush$domain$bottom) * ncol(LSNorm), 1))
  x.win1 <- floor(min((as.numeric(selectedData[4]) - input$LineScanNormBrush$domain$bottom) * ncol(LSNorm), ncol(LSNorm)))
  if (t.win1 <= t.win0) {
    return(NULL)
  }
  if (x.win1 <= x.win0) {
    return(NULL)
  }
  xProfileNorm <<- colMeans(LSNorm[t.win0:t.win1, ])
  output$xProfileNorm <- renderPlot({
    plot(x.win0:x.win1, xProfileNorm[x.win0:x.win1], ylab = "F")
  })
  tProfileNorm <<- rowMeans(LSNorm[, x.win0:x.win1])
  x.mid <- floor(mean(x.win0:x.win1))
  tProfileNorm2 <<- rowMeans(LSNorm[, (x.mid - input$SpatialWindow):(x.mid + input$SpatialWindow)])
  output$tProfileNorm <- renderPlot({
    # plot(t.win0:t.win1, tProfileNorm[t.win0:t.win1], type = 'l', ylab = 'F') lines(t.win0:t.win1,tProfileNorm2[t.win0:t.win1],
    # type='l', col='red')
    # plot(t.win0:t.win1, tProfileNorm2[t.win0:t.win1], type = "l", col = "red", ylab = "F")
    # lines(t.win0:t.win1, tProfileNorm[t.win0:t.win1], type = "l")
    # plot(smooth.spline(t.win0:t.win1,tProfileNorm2[t.win0:t.win1]),type = "l")
    # plot(t.win0:t.win1, tProfileNorm[t.win0:t.win1], type = 'l', ylab = 'F') lines(t.win0:t.win1,tProfileNorm2[t.win0:t.win1],
    # type='l', col='red')
    plot(t.win0:t.win1, tProfileNorm2[t.win0:t.win1], type = "l", col = "red", ylab = "F")
    tProfileNorm3 <- tProfileNorm2[t.win0:t.win1]
    t3 <- t.win0:t.win1
    Profile3Fit <<- lm(tProfileNorm3 ~ t3)
    lines(t.win0:t.win1,predict(Profile3Fit))
    # lines(t.win0:t.win1, tProfileNorm[t.win0:t.win1], type = "l")
    # plot(smooth.spline(t.win0:t.win1,tProfileNorm2[t.win0:t.win1]),type = "l")
  })
  t.win0D <<- t.win0
  t.win1D <<- t.win1
  x.win0D <<- x.win0
  x.win1D <<- x.win1
}


HandleCalculateDiffusion <- function(input, output, session) {
  if(!exists("x.win1D")){
    x.win1D <- ncol(LSNorm)
    x.win0D <- 1
  }
  if(!exists("t.win1D")){
    t.win1D <- nrow(LSNorm)
    t.win0D <- 1
  }
  LSDiffusion <<- LSNorm[t.win0D:t.win1D, x.win0D:x.win1D]
  # output$LSDiffusion <- renderPlot({
  #   image(LSDiffusion, col = rainbow(256), axes = FALSE)
  # })
  
  xProfileDiffusion <<- colMeans(LSDiffusion)
  xDiffusion <<- seq(xProfileDiffusion) * input$PixelSize/1000
  xDiffusion <<- xDiffusion - (median(xDiffusion))
  xDiffusion2 <<- xDiffusion^2
  FitDiffusionProfile <<- lm(xProfileDiffusion ~ xDiffusion + xDiffusion2)
  
  output$xProfileDiffusion <- renderPlot({
    plot(xDiffusion, xProfileDiffusion, ylab = "F")
    lines(xDiffusion, predict(FitDiffusionProfile))
  })
  # x.mid <- as.integer(mean(seq(ncol(LSDiffusion))))
  x.0 <- as.integer(mean(seq(ncol(LSDiffusion))))
  LinePeriod <- input$LinePeriod
  # SpatialWindow <- input$SpatialWindow
  
  # min(nrow(LSDiffusion)-x.0,x.0-1)
  
  # SpatialWindow  
  DiffCoefs <<- 1:min(ncol(LSDiffusion)-x.0,x.0-1)
  SigmaDiffCoefs <<- DiffCoefs
  DiffCoefTable <- NULL
  # LinFitSSE <<- DiffCoefs
  for(SpatialWindow in 1:min(ncol(LSDiffusion)-x.0,x.0-1)){
    tProfileDiffusion <<- rowMeans(LSDiffusion[, ((x.0 - SpatialWindow):(x.0 + SpatialWindow))])
    tDiffusion <- seq(tProfileDiffusion) * LinePeriod/1000
    FitDiffusionTime <- lm(tProfileDiffusion ~ tDiffusion)
    
    DFDt <- summary(FitDiffusionTime)$coefficients[2,1]
    DDFDt <- summary(FitDiffusionTime)$coefficients[2,2]
    # DFDt <<- coef(FitDiffusionTime)[2]  # /s
    
    
    # D2FDx2 <<- 2 * coef(FitDiffusionProfile)[3]  # 
    D2FDx2 <<- 2 * summary(FitDiffusionProfile)$coefficients[3,1]
    DD2FDx2 <<- summary(FitDiffusionProfile)$coefficients[3,2]
    D_app <<- DFDt/(3 * D2FDx2)
    # print(D_app)  
    DiffCoefs[SpatialWindow] <- D_app
    SigmaDiffCoefs[SpatialWindow] <- ((DDFDt/(3*D2FDx2))^2 + ((DFDt*DD2FDx2)/(3*D2FDx2^2))^2)^0.5
    # output$tProfileDiffusion <- renderPlot({
    # if(SpatialWindow==1){
      plot(tDiffusion, tProfileDiffusion, type = "l", ylab = "F")
      lines(tDiffusion, predict(FitDiffusionTime))
    # }else{
    #   lines(tDiffusion, tProfileDiffusion, type = "l", ylab = "F",col=SpatialWindow)
    #   lines(tDiffusion, predict(FitDiffusionTime),col=SpatialWindow)
    # }
      
    # })
      DiffCoefTable <- rbind(DiffCoefTable,c(SpWindow=SpatialWindow,DiffCoef=DiffCoefs[SpatialWindow],dDiffCoef=SigmaDiffCoefs[SpatialWindow],Xwin0=x.win0,Xwin1=x.win1,Twin0=t.win0,Twin1=t.win1,
                                             Xwin0D=x.win0D,Xwin1D=x.win1D,Twin0D=t.win0D,Twin1D=t.win1D,Fnorm=mean(tProfileDiffusion),LinePeriod = input$LinePeriod, PixelSize = input$PixelSize))
  }
  DiffCoefs <<- DiffCoefs
  DiffCoefTable <<- DiffCoefTable
  SigmaDiffCoefs <<- SigmaDiffCoefs
  # LinFitSSE <<- LinFitSSE
  output$tProfileDiffusion <- renderPlot({
    plot(tDiffusion, tProfileDiffusion, type = "l", ylab = "F")
    lines(tDiffusion, predict(FitDiffusionTime))
  })
  
  # output$D_app <- renderText(DiffCoefs)
  output$DiffCoefs <- renderPlot({
    plot(DiffCoefs,ylim = c(min(DiffCoefs-SigmaDiffCoefs),max(DiffCoefs+SigmaDiffCoefs)))
    lines(DiffCoefs+SigmaDiffCoefs)
    lines(DiffCoefs-SigmaDiffCoefs)
  })
  output$DiffCoefTable <- renderTable(DiffCoefTable[input$SpatialWindow:(input$SpatialWindow+1),],digits = 5)
  # output$dput <- shiny::renderText(dput(DiffCoefTable[input$SpatialWindow,]))
}

HandleXProfileBrush <- function(input, output, session) {
  x.win0 <<- (max(floor(unlist(input$xProfileBrush$xmin)), 1))
  x.win1 <<- (min((ncol(LSimage)), floor(unlist(input$xProfileBrush$xmax))))
  if(!exists("t.win1")){
    t.win1 <- nrow(LSimage)
    t.win0 <- 1
  }
  if (t.win1 <= t.win0) {
    return(NULL)
  }
  if (x.win1 <= x.win0) {
    return(NULL)
  }
  xProfile <<- colMeans(LSimage[t.win0:t.win1, x.win0:x.win1])
  output$xProfilePlot <- renderPlot({
    plot(x.win0:x.win1, xProfile, ylab = "F", ylim = c(0, 1.1 * max(xProfile)))
  })
  
  tProfile <<- rowMeans(LSimage[t.win0:t.win1, x.win0:x.win1])
  output$tProfilePlot <- renderPlot({
    plot(t.win0:t.win1, tProfile, type = "l", ylab = "F")
  })
}


HandletProfileBrush <- function(input, output, session) {
  t.win0 <<- max(1, floor(input$tProfileBrush$xmin))
  t.win1 <<- min(floor(input$tProfileBrush$xmax), nrow(LSimage))
  if(!exists("x.win1")){
    x.win1 <- ncol(LSimage)
    x.win0 <- 1
  }
  if (t.win1 <= t.win0) {
    return(NULL)
  }
  if (x.win1 <= x.win0) {
    return(NULL)
  }
  xProfile <<- colMeans(LSimage[t.win0:t.win1, x.win0:x.win1])
  output$xProfilePlot <- renderPlot({
    plot(x.win0:x.win1, xProfile, ylab = "F", ylim = c(0, 1.1 * max(xProfile)))
  })
  
  tProfile <<- rowMeans(LSimage[t.win0:t.win1, x.win0:x.win1])
  output$tProfilePlot <- renderPlot({
    plot(t.win0:t.win1, tProfile, type = "l", ylab = "F")
  })
  
}

HandletProfileNormBrush <- function(input, output, session) {
  t.win0 <- max(1, floor(input$tProfileNormBrush$xmin))
  t.win1 <- min(floor(input$tProfileNormBrush$xmax), nrow(LSNorm))
  if(!exists("x.win1D")){
    x.win1D <- ncol(LSNorm)
    x.win0D <- 1
  }
  if (t.win1 <= t.win0) {
    return(NULL)
  }
  if (x.win1 <= x.win0) {
    return(NULL)
  }
  
  xProfileNorm <<- colMeans(LSNorm[t.win0:t.win1, ])
  output$xProfileNorm <- renderPlot({
    plot(x.win0D:x.win1D, xProfileNorm[x.win0D:x.win1D], ylab = "F")
  })
  tProfileNorm <<- rowMeans(LSNorm[, x.win0D:x.win1D])
  x.mid <- floor(mean(x.win0D:x.win1D))
  tProfileNorm2 <<- rowMeans(LSNorm[, (x.mid - input$SpatialWindow):(x.mid + input$SpatialWindow)])
  output$tProfileNorm <- renderPlot({
    # plot(t.win0:t.win1, tProfileNorm[t.win0:t.win1], type = 'l', ylab = 'F') lines(t.win0:t.win1,tProfileNorm2[t.win0:t.win1],
    # type='l', col='red')
    plot(t.win0:t.win1, tProfileNorm2[t.win0:t.win1], type = "l", col = "red", ylab = "F")
    tProfileNorm3 <- tProfileNorm2[t.win0:t.win1]
    t3 <- t.win0:t.win1
    Profile3Fit <<- lm(tProfileNorm3 ~ t3)
    lines(t.win0:t.win1,predict(Profile3Fit))
    # lines(t.win0:t.win1, tProfileNorm[t.win0:t.win1], type = "l")
    # plot(smooth.spline(t.win0:t.win1,tProfileNorm2[t.win0:t.win1]),type = "l")
  })
  t.win0D <<- t.win0
  t.win1D <<- t.win1
  
}

HandleInputPars <- function(input, output, session){
  if(input$InputPars == ""){return(0)}
  InputParameters <<- as.double(unlist(lapply(X = (strsplit(input$InputPars,"\t")),FUN = as.numeric)))
  SpWindow <<- InputParameters[1]
  x.win0 <<- InputParameters[4]
  x.win1 <<- InputParameters[5]
  t.win0 <<- InputParameters[6]
  t.win1 <<- InputParameters[7]
  x.win0D <<- InputParameters[8]
  x.win1D <<- InputParameters[9]
  t.win0D <<- InputParameters[10]
  t.win1D <<- InputParameters[11]
  LinePeriod <<- InputParameters[13]
  PixelSize <<- InputParameters[14]
  xProfile <<- colMeans(LSimage[t.win0:t.win1, x.win0:x.win1]) 
  LSNorm <<- t(t(LSimage[, x.win0:x.win1])/xProfile)
  LSDiffusion <<- LSNorm[t.win0D:t.win1D, x.win0D:x.win1D]
  
  xProfileDiffusion <<- colMeans(LSDiffusion)
  xDiffusion <<- seq(xProfileDiffusion) * PixelSize/1000
  xDiffusion <<- xDiffusion - (median(xDiffusion))
  xDiffusion2 <<- xDiffusion^2
  FitDiffusionProfile <<- lm(xProfileDiffusion ~ xDiffusion + xDiffusion2)
  
  output$xProfileDiffusion <- renderPlot({
    plot(xDiffusion, xProfileDiffusion, ylab = "F")
    lines(xDiffusion, predict(FitDiffusionProfile))
  })
  
  x.0 <- as.integer(mean(seq(ncol(LSDiffusion))))

  # tProfileDiffusion <<- rowMeans(LSDiffusion[, ((x.0 - SpatialWindow):(x.0 + SpatialWindow))])
  tProfileDiffusion <<- rowMeans(LSDiffusion[, ((x.0 - SpWindow):(x.0 + SpWindow))])  
  tDiffusion <- seq(tProfileDiffusion) * LinePeriod/1000
  FitDiffusionTime <- lm(tProfileDiffusion ~ tDiffusion)
  
  DFDt <- summary(FitDiffusionTime)$coefficients[2,1]
  DDFDt <- summary(FitDiffusionTime)$coefficients[2,2]
  # DFDt <<- coef(FitDiffusionTime)[2]  # /s
  
  
  # D2FDx2 <<- 2 * coef(FitDiffusionProfile)[3]  # 
  D2FDx2 <<- 2 * summary(FitDiffusionProfile)$coefficients[3,1]
  DD2FDx2 <<- summary(FitDiffusionProfile)$coefficients[3,2]
  D_app <<- DFDt/(3 * D2FDx2)
  SigmaDapp <<- ((DDFDt/(3*D2FDx2))^2 + ((DFDt*DD2FDx2)/(3*D2FDx2^2))^2)^0.5
  
  
  DiffCoefTable <- rbind(c(SpWindow=SpWindow,DiffCoef=D_app,dDiffCoef=SigmaDapp,Xwin0=x.win0,Xwin1=x.win1,Twin0=t.win0,Twin1=t.win1,
                                         Xwin0D=x.win0D,Xwin1D=x.win1D,Twin0D=t.win0D,Twin1D=t.win1D,Fnorm=mean(tProfileDiffusion),LinePeriod = LinePeriod, PixelSize = PixelSize))
  DiffCoefTable <- rbind(DiffCoefTable,DiffCoefTable)
  output$DiffCoefTable <- renderTable(DiffCoefTable,digits = 5)
}