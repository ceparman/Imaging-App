
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)




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
      
    setwd(path.expand("~"))  
      
    suppressWarnings( main_script(input$gfp_file$datapath,input$total_file$datapath,
                input$map_file$datapath,getwd())
    )
      
    zip::zip("Results.zip", paste0(  setwd(path.expand("~")) , "/output"),recurse = TRUE)
    
      output$status <- renderText("Analysis Completed")
    }
      
      
    })
 
  
#### Download output  
  
 
    output$downloadData <- downloadHandler(
     
      
       filename = function() {
      #  paste0("Results-",Sys.time(),".zip")
         paste0("Results.zip")
      },
      content = function(file) {
        
        file.copy(paste0(path.expand("~"),"/Results.zip") , file)
        
      }
      ,contentType = "application/zip"
    )
    
       
   
    
    
  })
  
  
  
  
  

    
    

