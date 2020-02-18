#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyBS)
library(insol)
library(leaflet)
options(stringsAsFactors=F)
input_coefficients = read.csv('input_coefficients_ddfln.csv',header=F);
#input_coefficients[(is.na(input_coefficients[,3]) | input_coefficients[,3] == 0) & !is.na(input_coefficients[,4]),3] = input_coefficients[(is.na(input_coefficients[,3]) | input_coefficients[,3] == 0) & !is.na(input_coefficients[,4]),4]
input_coefficients[is.na(input_coefficients[,3]),3] = 0
#input_coefficients[is.na(input_coefficients[,4]),4] = 370
input_coefficients[is.na(input_coefficients[,5]),5] = 50
input_coefficients[is.na(input_coefficients[,6]),6] = 13.5
input_coefficients[is.na(input_coefficients[,7]),7] = "unknown"
input_coefficients[is.na(input_coefficients[,8]),8] = "unknown"



varGroups = unique(input_coefficients$V7)
optGroups = lapply(varGroups,function(grp){as.list(input_coefficients$V1[input_coefficients$V7==grp])})
names(optGroups) = varGroups
names(optGroups)[names(optGroups)==''] = 'Other/Unkown'

# Define UI for application that draws a histogram
shinyUI(fluidPage(useShinyjs(),theme=shinytheme('paper'),
  
  # Application title
  titlePanel(windowTitle='Winter Cereal Survival Model',title=div(img(src="uofs-logo.png"),"Winter Cereal Survival Model Research Tool")),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width=3,
        a(id='exampleData',target='_blank',href='./WCSM-example-data.csv','Example Data Format'),
       fileInput('temperatureData','Upload Data',accept=c('text/csv','text/comma-separated-values,text/plain', '.csv')),
       bsTooltip('exampleData',title='Download example data in required format (4 column comma separated table with headers). Table cannot have gaps.',placement='right'),
#       uiOutput(outputId='seedDate'),
       dateInput('seedDate','Seed Date'),
bsTooltip(id = "seedDate", title = "Uploaded data must have days (starting on day 1) in the first column,  format: 'YYYY-MM-DD', eg. '2017-09-01'",placement='right', 
          trigger = "hover"),
       selectInput('dataSet',label='Historical Temperature Data',multiple=FALSE,selected = 'Alameda_2013.csv',
                   choices=list(
                    'Custom'=list('Custom'='Custom'),
                    'North America'=list('Alameda, Saskatchewan 2013'='Alameda_2013.csv',
                                         'Radville, Saskatchewan 2009'='Radville_2009.csv',
                                         'Saskatoon, Saskatchewan 1995'='Saskatoon_1995.csv',
                                         'Saskatoon, Saskatchewan 2001'='Saskatoon_2001.csv',
                                         'Saskatoon, Saskatchewan 2002'='Saskatoon_2002.csv',
                                         'Saskatoon, Saskatchewan 2004'='Saskatoon_2004.csv',
                                         'Sherwood, North Dakota 2011'='Sherwood_2011.csv'),
                    'Europe'=list('Kromeriz, Czech Republic 2017'='Kromeriz_2017.csv',
                                  'Prague, Czech Republic 1990'='Prague_1990.csv',
                                  'Oppdal, Norway 2004'='Oppdal_2004.csv'
                                  ),
                    'Asia'=list('Maragheh, Iran 2003'='Maragheh_2003.csv')
                    
                    

                   )),
#      uiOutput(outputId='lat'),
#      uiOutput(outputId='lon'),
       numericInput('lat','Latitude',value=NULL),
       numericInput('lon','Longitude',value=NULL),
       selectInput(inputId='selectVariety',label='Variety Selector',selected='Norstar',multiple = FALSE,choices=optGroups),
       bsTooltip(id = "lon", title = "Longitudes west of Greenwich must be negative",placement='right',trigger='hover'), 
       sliderInput("LT50c",
                    "LT50c (°C)",
                    min = -35,
                    max = -3,
                    value = -24),
       bsTooltip(id = "LT50c", title = "The genetic coefficient of a variety is the minimum achievable LT50 in degrees celcius.",placement='right', 
                  trigger = "hover"),
       sliderInput("vernReq",
                   "Vernalization Requirement (Days)",
                   min = 0,
                   max = 100,
                   value = 49),
#       bsTooltip(id = "minDD", title = "Low temperature delay in the vegetative/reproductive transition of spring habit lines during acclimation",placement='right', 
#          trigger = "hover"),
        sliderInput("minDD",
            "Degree Days (DD) to Minimum Final Leaf Number (MFLN)",
            min = 320,
            max = 382,
            value = 370),
       bsTooltip(id = "vernReq", title = "Number of days at vernalization temperatures to satisfy vernalization requirement",placement='right', 
                 trigger = "hover"),
       # sliderInput("minDD",
       #             "Minimum Degree Days",
       #             min = 0,
       #             max = 500,
       #             value = 366),
       # bsTooltip(id = "minDD", title = "Minimum Degree Days to achieve minimum final leaf number under optimal conditions",placement='right', 
       #           trigger = "hover"),
       sliderInput("photoCritical",
                   "Critical Photoperiod (Hours)",
                   min = 0,
                   max = 24,
                   value = 13.5,
                   step=0.25),
       bsTooltip(id = "photoCritical", title = "Critical daylength above which photoperiod requirement is rapidly met",placement='right', 
                 trigger = "hover"),
       sliderInput("photoCoeff",
                   "Photoperiod Coefficient",
                   min = 0,
                   max = 60,
                   value = 50,
                   step = 1),
       bsTooltip(id = "photoCoeff", title = "Delay in vegetative reproductive transition (days) due to a photoperiod requirement",placement='right', 
                 trigger = "hover"),
      actionButton('resetCoeff','Reset Cultivar')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      div(h4('Simulation Results')),
      plotOutput("LT50", click='plot_click',dblclick="plot_dblclick",brush=brushOpts(id="plot_brush",delayType = 'debounce',resetOnNew=TRUE,delay=1000,direction='x')),
      downloadLink("downloadData", "Download Results"),
      div(h4('Instructions for interacting with plot')),
      tags$ul(
        tags$li(tags$strong('Click'),' plot to show values at selected time'),
        tags$li(tags$strong('Double click'),' plot to draw temperature spike'),
        tags$li(tags$strong('Click and Drag'),' to highlight range (See radio buttons below for highlighting behavior)')),
      radioButtons('respiration',label=NULL,choices=list('Highlighting smooths soil temperatures'=FALSE,'Highlighting produces a snow depth that maintains soil temperatures near 0°C and creates conditions favorable for respiration damage'=TRUE)),
      div(h4('Geographic Location')),
      div(p('Click on the map to select latitude and longitude to run simulation at. Locations can be searched by name using the magnifying glass icon.')),
      leafletOutput('map'),
      p('Read the paper ',a(href="./www/cs-54-6-2395.pdf",target="_blank","here")),
      p('Contact the authors: ',a(href='mailto:brian.fowler@usask.ca','Dr. Brian Fowler'),',',a(href='mailto:brookb@gmail.com','Brook Byrns'),', Dept Plant Sciences, University of Saskatchewan')
      


#       verbatimTextOutput("click_info")
       
    )
  )
))
