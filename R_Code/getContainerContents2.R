getContainerContents2<-function (creds, barcode, useVerbose = FALSE)
  
{
  
  #Get plate with cell ids
  
  
 
   resource <- "CONTAINER"
  
  #alist <- paste0(associations, collapse=",")
  
  query <- 
    paste0("('",barcode,"')/REV_IMPL_CONTAINER_CELL")
  
  
 
 # print(query)
  
    header <- c(Accept = "application/json;odata.metadata=full")

cells <-
    CoreAPIV2::apiGET(
      creds,
      resource = resource,
      query = query,
      headers = header,
      useVerbose = useVerbose
    )
  
  cellIDs <- unlist(lapply(cells$content, function(x) x$Id))
  
  nCells <- length(cellIDs)
  
 cellContents <- list()
 sampleLots  <- list()  
 sampleType <- list()
control <- list()

 resource <- "CELL"
 header <- c(Accept = "application/json;odata.metadata=full")
 
 
  for (i in 1:nCells)
    
  {
    
  #get each cell contents
     
  # print(i)
 
   query <- 
     paste0("(",cellIDs[i],")/CONTENT?$expand=IMPL_SAMPLE_LOT")
   
   
   
  # print(query)
   
   
   
   cell <-
     CoreAPIV2::apiGET(
       creds,
       resource = resource,
       query = query,
       headers = header,
       useVerbose = useVerbose
     )
   
    cellContents[[i]] <- cell$content[[1]]
     
   #get sample lot
   #ugly hack because cells don't report sample type
    
  
    prefix <- substr(cellContents[[i]]$IMPL_SAMPLE_LOT$Barcode,1,3)
    
      type <- switch(prefix,
           TRT = "TREATMENT_LOT",
           BLK = "BLANK_LOT"
           )
    
    
    lot <- CoreAPIV2::getEntityByBarcode(creds,type,cell$content[[1]]$IMPL_SAMPLE_LOT$Barcode,fullMetadata = TRUE)
    
    sampleLots[[i]] <- lot$entity
    sampleType [[i]] <- type
    
    Sys.sleep(.1)
    
  }  
 
 
 
 
sampleLot <- unlist( lapply(sampleLots, function(x) {x$Barcode} ) )
 
 

concentration <-unlist( lapply(sampleLots, function(x) {getConcentration(x)} ) )
var_compound <- unlist( lapply(sampleLots, function(x) {getVarcompound(x)} ) )
compound <-  unlist( lapply(sampleLots, function(x) {buildCompound(x)})) #will need to pass concentration and units if we change them 
content <-  unlist( lapply(sampleLots, function(x) {buildContents(x)}))

units <- rep("uM",nCells)

#Asume all controls are in row H 

control <- rep(NA,nCells)

lastrownames <-  unique(sampleLot[85:96])

lastrownames <- lastrownames[substr(lastrownames,1,3) != "BLK" ]

#add CTRL to content string
for (i in 85:96)
{
  
  if (sampleLot[i] %in% lastrownames ) {
    
    control[i] <- as.character(which(lastrownames == sampleLot[i]))
    
  }
}
 
#copy contnet to compound, var_compound, and concentration

compound[which(!is.na(control))] <- content[which(!is.na(control))]

var_compound[which(!is.na(control))] <- content[which(!is.na(control))]

content[which(!is.na(control))] <-content[which(!is.na(control))]

concentration[which(!is.na(control))] <- paste0("CTRL",control[which(!is.na(control))],"_",content[which(!is.na(control))])


#make var_compound  and concentration the same as compound for controls

#var_compound[which(!is.na(control))] 

#Deal with all blanks

units[ which(sampleType == "BLANK_LOT")] <- NA
content[ which(sampleType == "BLANK_LOT")] <- NA
var_compound[ which(sampleType == "BLANK_LOT")] <- NA
compound[ which(sampleType == "BLANK_LOT")] <- NA
concentration[ which(sampleType == "BLANK_LOT")] <- NA


p <- makePlate()

plate_map <- data.frame(  
                          content = content,
                          compound = compound,
                          var_compound = var_compound,
                          concentration =concentration,
                          units = units,
                         control  = control,
                         stringsAsFactors = FALSE
)
plate_map <- cbind(p,plate_map)
 
return(plate_map)  
  
}