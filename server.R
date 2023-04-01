#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinyjs)
library(deSolve)
library(Cairo)
library(ggplot2)
library(oce)
library(insol)
library(ggrepel)
library(leaflet)
library(leaflet.extras)
source('wcsmR2_functions.R')
source('wcsmR2.R')
coeff = read.csv('input_coefficients_ddfln.csv',header=F)
coeff[is.na(coeff[,3]),3] = 0
#coeff[is.na(coeff[,4]),4] = 370
coeff[is.na(coeff[,5]),5] = 50
coeff[is.na(coeff[,6]),6] = 13.5
coeff[is.na(coeff[,7]),7] = "unknown"
coeff[is.na(coeff[,8]),8] = "unknown"

# Define server logic required to draw a histogram
shinyServer(function(input,output,session) {
  val <- reactiveValues(data=NULL,x_coord=0,plotTitle='Norstar')
  out <- reactive({
#    req(input$seedDate)
#    req(input$lat)
#    req(input$lon)
#    req(input$vernReq)
#    req(input$photoCoeff)
#    req(input$photoCritical)
#    req(input$ltd)
#    req(input$LT50c)
  
    daylengths = daylengths()
    DT <- 1
    time <- 1:length(val$data[,1])
    temps = data.frame(t=time,crownTemp=val$data[,2])
    Y <-
      c(
        LT50raw = -3,
        minLT50 = -3,
        dehardAmt = 0,
        dehardAmtStress = 0,
        mflnFraction = 0,
        photoReqFraction = 0,
        accAmt = 0,
        vernDays = 0,
        vernProg = 0,
        respProg = 0
#        LTDdays = 0,
#        LTDprog = 0
    )
    ode(
      func = model,
      y = Y,
      times = time,
      parms = c(
        photoCoeff = input$photoCoeff,
        photoCritical = input$photoCritical,
        vernReq = input$vernReq,
#        LTDreq = input$ltd,
        minDD = input$minDD,
        initLT50 = -3.0,
        LT50c = input$LT50c
      ),
      method = 'euler',
      daylengths=daylengths,
      crownTemps = temps
    )
  })
  daylengths <- reactive({
    req(input$lat) & req(input$lon)
    jdays = JD(strptime(val$data[,1],format='%Y-%m-%d'))
    daylength(input$lat,input$lon,jdays,-6)[,3]
  })
  output$LT50 <- renderPlot({
    out <- out()
#    out = cbind(out, temperature = val$data[,2],daylength=daylengths())
    out = as.data.frame(out)
    out$LT50raw = sapply(out$LT50raw,min,-3)
    # Respiration areas
    resp = out[,'respProg']
    respSlope = sapply(1:length(resp),function(i){ifelse(i == 1,0,resp[i]-resp[i-1])})
    resp[respSlope > 0] = 1
    resp[respSlope == 0] = 0
    resp[respSlope < 0] = 1
    inds <- diff(c(0, resp))
    start <- out$time[inds == 1]
    end <- out$time[inds == -1]
    if (length(start) > length(end))
      end <- c(end, tail(out$time, 1))
    rects <- data.frame(start = start,
      end = end,
      group = seq_along(start))
    # Annotations
    out$annotations = ""

    vernannot = which.max(sapply(out[, 'vernProg'], min, 1))
    photoannot = which.max(sapply(out[, 'photoReqFraction'], min, 1))
    winterkillannot = which(floor(out[,'temperature']-out[,'LT50raw']) <= 0)[1]
    out$annotations[photoannot] = "Max Photoperiod\nSaturation"


    
    if(input$vernReq == 0) {
      out$annotation[vernannot] = ""
    } else if(vernannot == photoannot) {
      out$annotations[vernannot] = "Max Photoperiod and\nVernalization\nSaturation"
    } else {
      out$annotations[vernannot] = "Max Vernalization\nSaturation"
      out$annotations[photoannot] = "Max Photoperiod\nSaturation"
    }
    out$annotations[winterkillannot] = 'Winterkill'
    if(val$x_coord & val$x_coord > 0) {
      out$annotations[val$x_coord] = paste('Date:',as.Date(input$seedDate)+out[val$x_coord,'time'],
                                           '\nDaylength:',out[val$x_coord,'daylength'], ' hours',
                                           '\nCrown Temperature:',out[val$x_coord,'temperature'],
                                           '\nPredicted LT50:',signif(out[val$x_coord,'LT50raw'],2))
    } else if(val$x_coord & val$x_coord == winterkillannot) {
      out$annotations[val$x_coord] = paste( 'WINTERKILL!',
                                            '\nDate:',as.Date(input$seedDate)+out[val$x_coord,'time'],
                                           '\nDaylength:',out[val$x_coord,'daylength'], ' hours',
                                           '\nCrown Temperature:',out[val$x_coord,'temperature'],
                                           '\nPredicted LT50:',signif(out[val$x_coord,'LT50raw'],2))
    }
    out$annotations[out$annotations==""] = NA
    out$color = out$annotations
    out$color[!is.na(out$annotations)] = 'white'
    out$color[out$annotations=="Winterkill"] = 'red'

    if(!is.na(coeff[coeff$V1==input$selectVariety,'V4'])) {
      ltdannot = which.max(sapply(out[, 'mflnFraction'], min, 1))
      if(vernannot == ltdannot) {
        out$annotations[vernannot] = "Max MFLN progress and\nVernalization\nSaturation"
      } 
      out$annotations[ltdannot] = "Max MFLN Progress"
    }
    
    p = ggplot(out, aes(x = time))
    
    if(!is.na(coeff[coeff$V1==input$selectVariety,'V4'])) {
      p = p + geom_vline(xintercept = out[which.max(sapply(out[, 'mflnFraction'], min, 1)), 'time'], linetype = 3)
    }
    
    

    if(val$x_coord > 0) {
      p = p + geom_vline(xintercept=val$x_coord,linetype=3)
    }
    if(input$vernReq > 0) {
      p = p + geom_vline(xintercept = out[which.max(sapply(out[, 'vernProg'], min, 1)), 'time'], linetype = 3)
    }
    if(dim(rects)[1] > 0) {
      p = p + geom_rect(
        data=rects,
        inherit.aes=FALSE,
        aes(
          xmin=start,
          xmax=end,
          ymin=-Inf,
          ymax=Inf,
          group=group),
        color="transparent",
        fill="orange",alpha=0.3) +
        geom_text(
          data=rects,
          inherit.aes=FALSE,
          aes(x=start,y=-15,label='respiration'),
          angle=90,
          alpha=0.5,
          size=6,
          vjust=1)
    }
    p = p +  geom_line(
      aes(y = LT50raw,color='Predicted LT50'),
      size = 1,
      linetype = 2,
      show.legend = TRUE
    ) +
    geom_line(
      aes(y = temperature,color='Soil Temp. at Crown Depth'),
      size = 1,
      show.legend = TRUE
    ) +
    geom_vline(xintercept = out[which.max(sapply(out[, 'photoReqFraction'], min, 1)), 'time'], linetype = 3) +
    geom_label_repel(
      mapping=aes(label = annotations, y = LT50raw,fill=I(color)),
      box.padding = unit(1, "lines"),
      arrow = arrow(
        length = unit(0.03, "npc"),
        type = "closed",
        ends = "last"
      )
    )
    p = p +
    scale_colour_manual("",values=c("Soil Temp. at Crown Depth"="blue","Predicted LT50"="orange")) +
    theme_minimal() +
    ylab( "Temperature (°C)") + 
    xlab("Day") + 
    theme(
      axis.text=element_text(size=16),
      axis.title=element_text(size=16,face='bold'),
      plot.title = element_text(size=16,face='bold'),
      legend.position='bottom',
      legend.text = element_text(size=16),
      panel.border = element_rect(colour = "grey", fill=NA, linewidth=2),
      plot.background = element_rect(fill = "transparent",colour = NA)) + 
    ggtitle(val$plotTitle)
    p
  },bg='transparent')
  observe({
    input$temperatureData
    isolate({
      # save new points added
      # add new points to data
      inFile <- input$temperatureData
      if (!is.null(inFile)) {
        d <- read.csv(file=inFile$datapath,header=TRUE)
        val$data <- d
        updateNumericInput(session,inputId='lat',value = d[1,3])
        updateNumericInput(session,inputId='lon',value = d[1,4])
        updateDateInput(session,inputId='seedDate',value = strptime(d[1,1],format='%Y-%m-%d'))
        updateSelectInput(session,inputId='dataSet',selected = "Custom")
        val$x_coord=0
      }
    })
  })
  observe({
    input$resetCoeff
    scoeff = coeff[coeff$V1==input$selectVariety,]
    updateSliderInput(session,inputId='photoCoeff',value = scoeff$V5)
    updateSliderInput(session,inputId='photoCritical',value = scoeff$V6)
    updateSliderInput(session,inputId='vernReq',value = scoeff$V3)
    updateSliderInput(session,inputId='minDD',value = scoeff$V4)
    updateSliderInput(session,inputId='LT50c',value = scoeff$V2)
  })
  
  observe({
    input$plot_dblclick
    isolate({
      # save new points added
      # add new points to data
      if(!is.null(input$plot_dblclick$x)) {
        d <- val$data
        day_dist = abs(1:length(d[,1])-input$plot_dblclick$x)
        width = 20
        weight.min = 1
        weight.max = 10
        s = max(which.min(day_dist)-floor(width/2),1)
        e = min(which.min(day_dist)+floor(width/2),length(day_dist))
        width = e-s+1
        if(!as.logical(width %% 2)) {
          x.lim = (width+1)
        } else {
          x.lim = width
        }
        weights = (dnorm((1:x.lim)-ceiling(x.lim/2))+1)*2
        d[,2][s:e] = loess(y~x,data=data.frame(x=1:x.lim,y=c(d[,2][s:(s+(floor(width/2)-1)-2)],rep(input$plot_dblclick$y,5),d[,2][(e-(floor(width/2)-1)+2):e])),weights =weights,degree=2,span=0.5)$fitted
        val$x_coord <- 0
        val$data <- d
      }
    })
  })
  observe({
    input$selectVariety
    scoeff = coeff[coeff$V1==input$selectVariety,]
    if(is.na(scoeff$V4)) {
      shinyjs::disable(id='minDD')
    } else {
      shinyjs::enable(id='minDD')
      updateSliderInput(session,inputId='minDD',value = scoeff$V4)
    }
    val$plotTitle = paste(scoeff$V1,scoeff$V7)
    updateSliderInput(session,inputId='photoCoeff',value = scoeff$V5)
    updateSliderInput(session,inputId='photoCritical',value = scoeff$V6)
    updateSliderInput(session,inputId='vernReq',value = scoeff$V3)
    updateSliderInput(session,inputId='LT50c',value = scoeff$V2)
    val$x_coord = 0
  })
  observe({
    input$dataSet
    isolate({
      if(input$dataSet != "Custom") {
        val$data = read.csv(file=input$dataSet)
        updateNumericInput(session,'lat',value=val$data[1,3])
        updateNumericInput(session,'lon',value=val$data[1,4])
        updateDateInput(session,'seedDate',value = as.Date(val$data[1,1]))
        val$x_coord=0
      }
    })
  })
  observe({
    input$plot_brush
    isolate({
      # save new points added
      # add new points to data
      if(!is.null(input$plot_brush$xmin)) {
        d <- val$data
        t <- 1:length(val$data[,1])
        #brushedPoints(d,input$plot_brush)
        day_min = abs(t-input$plot_brush$xmin)
        day_max = abs(t-input$plot_brush$xmax)
        s = max(which.min(day_min)-2,1)
        e = min(which.min(day_max)+2,length(day_max))
        width = e-s+1
        if(!as.logical(input$respiration))
          d[,2][s:e] = loess(y~x,data=data.frame(x=1:width,y=d[,2][s:e]),degree=1,span=2)$fitted
        if(as.logical(input$respiration))
          d[,2][s:e] = 0
        val$x_coord <- 0
        val$data <- d
      }
    })
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("WCSM-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(cbind(date=val$data[,1],out(), temperature = val$data[,2],daylengths=daylengths()), file)
    }
  )
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    req(input$lon) & req(input$lat)
    leaflet() %>%
    addProviderTiles(providers$Esri.WorldImagery,
                    options = providerTileOptions(noWrap = TRUE) 
    ) %>%
    # THIS WMS DOES NOT WORK
    #addWMSTiles(
    #  "https://viewer.globalland.vgt.vito.be/geoserver/ows",
    #  layers = "CGS_S2_10M_BANDS",
    #  options = WMSTileOptions(format = "image/png", transparent = TRUE),
    #  attribution = "ESA @ 2021"
    #) %>%
    # THIS WMS WORKS
    #addWMSTiles(
    #  "https://services.terrascope.be/wms/v2",
    #  layers = "WORLDCOVER_2021_MAP",
    #  options = WMSTileOptions(format = "image/png", transparent = TRUE),
    #  attribution = "ESA @ 2021"
    #) %>%
    addProviderTiles(providers$Stamen.TonerLines,
                    options = providerTileOptions(opacity = 0.35)) %>%
    addProviderTiles(providers$Stamen.TonerLabels) %>%
    #addProviderTiles(providers$Stamen.Terrain,
    #                options = providerTileOptions(noWrap = TRUE)
    setView(lng = input$lon, lat=input$lat, zoom = 12) %>% 
    addMarkers(lng=input$lon,lat=input$lat) %>%
    addSearchOSM()
  })
  observe({
    click <- input$map_click
    if(is.null(click))
      return()
    updateNumericInput(session,'lat',value=click$lat)
    updateNumericInput(session,'lon',value=click$lng)
  })
  observe({
    input$plot_click
    isolate({
      # save new points added
      # add new points to data
      if(!is.null(input$plot_click$x)) {
        t = 1:length(val$data[,1])
        #day_dist = abs(val$data$t-input$plot_click$x)
        #val$x_coord = val$data$t[which.min(day_dist)]
        day_dist = abs(t-input$plot_click$x)
        val$x_coord = t[which.min(day_dist)]
      }
    })
  })
  
})
