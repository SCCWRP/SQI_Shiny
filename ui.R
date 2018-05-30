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
    column(width = 12, 
           textOutput('overall')
           ), 
    column(width = 12, 
           textOutput('biolcon')
           ), 
    column(width = 12, 
           textOutput('strscon')
           ), 
    column(width = 12, 
           textOutput('strsdet')
           )
  ),
  
  # plot outputs
  fluidRow(
    
    HTML('<p></p>'),
    column(width = 12, 
           plotOutput('plos')
           )
    
  )
  
))


