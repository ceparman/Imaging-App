
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)




options(shiny.maxRequestSize=600*1024^2)
shinyServer(function(input, output, session) {
 
   session$onSessionEnded(function() {
     unlink("ImageAppoutput",recursive = TRUE)
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
      
    suppressWarnings( 
       withProgress(message = "Processing Data",value = 0,{
                       main_script(input$gfp_file$datapath,input$total_file$datapath,
                input$map_file$datapath,getwd())
       }
       )
    )
      
      zip::zip("results.zip","ImageAppoutput",recurse = TRUE)
    
      output$status <- renderText("Analysis Completed")
      
      enable("downloadData")
    }
      
      
    })
 
  
#### Download output  
  
 
    output$downloadData <- downloadHandler(
     
       filename = function() {
      
        paste0("Results-",format(Sys.time(),"%Y-%m-%d--%H:%M"),".zip")
      },
      content = function(file) {
        
        file.copy(from = "results.zip", to = file)
        
      }
      ,contentType = "application/zip"
    )
    
       
    
#observe file inputs
    
 file_exists <- reactive({ 
          list(input$gfp_file, input$total_file, input$map_file) 
 
            })
   
   
  observeEvent( file_exists(), { 
   
   
   if (!( is.null(input$gfp_file)  | is.null(input$total_file) |  is.null(input$map_file) ) ){
    
     enable("run")
  
    }
   
   })   
   
    
    
  })
  
  
  
  
  

    
    

