
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)


jscode <- "shinyjs.closeWindow = function() { window.close(); }"

options(shiny.maxRequestSize=300*1024^2)
shinyServer(function(input, output, session) {
 
   session$onSessionEnded(function() {
    stopApp()
  })
  
 
  
  output$status <- renderText({ 
    
    if (is.null(input$gfp_file)) "Select inputs" else "Hit Process" })
  
 
  # 
  observeEvent(input$run,{
    
    if (is.null( input$gfp_file) )
    {
    
      return()
       
    } else {
    
    #process data
    
      output$status <- renderText("Processing Date")
      
    suppressWarnings( main_script(input$gfp_file$datapath,input$total_file$datapath,
                input$map_file$datapath,input$path)
    )
      
      output$status <- renderText("Analysis Completed")
    }
      
      
    })
 
    
  
  
  })
    
    

