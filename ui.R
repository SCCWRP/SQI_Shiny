library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  theme = 'styles.css',

  # bio inputs
  fluidRow(
    
    column(width = 12, h3('Biology')),
    column(width = 4, 
        numericInput('csci', h5('CSCI score. Enter a number between 0 and ~1.4'), value = 0.45, min = 0, max = 1.5) 
        ), 
    column(width = 4, 
        numericInput('asci', h5('ASCI score (H20 for now). Enter a number between 0 and 100'), value = 25, min = 0, max = 100)    
        )
    
  ),
  
  # chem inputs
  fluidRow(
    
    column(width = 12, h3('Chemistry')),
    column(width = 4, 
           numericInput('tn', h5('Total N (mg/L). Enter a number greater than zero'), value = 10.1, min = 0) 
    ), 
    column(width = 4, 
           numericInput('tp', h5('Total P (mg/L). Enter a number greater than zero'), value = 10.001, min = 0)    
    ), 
    column(width = 4, 
           numericInput('cond', h5('Specific conductance (uS/cm). Enter a number greater than zero'), value = 450, min = 0)    
    )
    
  ),
  
  # hab inputs
  fluidRow(
    
    column(width = 12, h3('Habitat')),
    column(width = 4, 
           numericInput('cram', h5('CRAM score. Enter a number between 0 and 100'), value = 75, min = 0, max = 100) 
    ), 
    column(width = 4, 
           numericInput('safn', h5('% sands and fines. Enter a number between 0 and 100'), value = 35, min = 0, max = 100)    
    ), 
    column(width = 4, 
           numericInput('aqhab', h5('SW diversity of aquatic habitats. Enter a number between 0 and ~2.5'), value = 1.5, min = 0, max = 2.6)    
    ),
    column(width = 4, 
           numericInput('subnat', h5('SW diversity of streambed substrate. Enter a number between 0 and ~2.5'), value = 1.9, min = 0, max = 2.6) 
    ), 
    column(width = 4, 
           numericInput('flowhab', h5('Evenness of flow habitat types (e.g., riffles, pools). Enter a number between zero and 1'), value = 0.5, min = 0, max = 1)    
    ), 
    column(width = 4, 
           numericInput('veg', h5('Riparian veg cover. Enter a number from 0 to ~265'), value = 200, min = 0, max = 300)    
    )
    
  ),
  
  # text output
  fluidRow(
   
    column(width = 12, h3('Stream Health Index')), 
    column(width = 6, 
           uiOutput('overall'), 
           uiOutput('biolcon'), 
           uiOutput('strscon'),
           uiOutput('strsdet')
           ), 
    column(width = 6, 
           uiOutput('pchemhab'),
           uiOutput('pchem'), 
           uiOutput('phab') 
           )
  ),
  
  # plot outputs
  fluidRow(
    
    HTML('<p></p>'),
    column(width = 12, 
           h3('Plots'),
           h5('Selected values beyond those on the x and y axes are shown at the plot margins.')
           ),
    
    # chemistry plots
    column(width = 6, 
           
           h5('x-axis chemistry variable'),
           
           # first chem input
           selectInput("chem1", 
             label = '', 
             choices = list(
               "Total Nitrogen" = "TN2",
               "Total Phosphorus" = "TP",
               "Conductivity" = "Cond"
             ),
             selected = "TN2"
          ),
    
          h5('y-axis chemistry variable'),
          
          # second chem input
          selectInput("chem2", 
                      label = '', 
                      choices = list(
                        "Total Nitrogen" = "TN2",
                        "Total Phosphorus" = "TP",
                        "Conductivity" = "Cond"
                      ),
                      selected = "TP"
          ),
          plotOutput('plochem')
        ),
    
    # habitat plots
    column(width = 6, 
           
           h5('x-axis habitat variable'),
           
           # first hab input
           selectInput("hab1", 
                       label = '', 
                       choices = list(
                         "CRAM" = "indexscore_cram",
                         "% sands and fines" = "PCT_SAFN",
                         "SW diversity of aquatic habitats" = "H_AqHab",
                         "SW diversity of streambed substrates" = "H_SubNat",
                         "Evenness of flow habitat types" = "Ev_FlowHab",
                         "Riparian veg cover" = "XCMG"
                       ),
                       selected = "indescore_cram"
           ),
           
           h5('y-axis habitat variable'),
           
           # second hab input
           selectInput("hab2", 
                       label = '', 
                       choices = list(
                         "CRAM" = "indexscore_cram",
                         "% sands and fines" = "PCT_SAFN",
                         "SW diversity of aquatic habitats" = "H_AqHab",
                         "SW diversity of streambed substrates" = "H_SubNat",
                         "Evenness of flow habitat types" = "Ev_FlowHab",
                         "Riparian veg cover" = "XCMG"
                       ),
                       selected = "PCT_SAFN"
           ),
           
           plotOutput('plohab')
    )
    
  )

))


