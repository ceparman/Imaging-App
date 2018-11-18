

buildCompound <- function(samplelot)
{
if(substr(samplelot$Barcode,1,3) == "BLK" ){
  return(NA)
} else {
  
compoundName <- "GF"
for (i in 1:4)

{ if ( (!VariedCompound(samplelot,i)) & (existsCompound(samplelot,i)) ) {
  #convert units
  
  c <- eval(parse(text=paste0("samplelot$FREQ_COMPOUND",i) ))  
  a <- eval(parse(text=paste0("samplelot$FREQ_COMPOUND",i,"_CONC") ))  
  
  conc <- convertCompundConc(list(value = a, units = "nM"))
  
  # compoundName <- paste0(compoundName,"+",a," nM ",c) 
  compoundName <- paste0(compoundName,"+",conc$value," ",conc$unit," ",c)
}
}  



return(compoundName) 
} 
}


buildContents <- function(samplelot)
{
  if(substr(samplelot$Barcode,1,3) != "BLK" ){
  #convert units here
    
    conc <- convertCompundConc(list(value = samplelot$FREQ_VARIED_COMPOUND_CONC, units = "nM"))  
    
  contents <- paste0( buildCompound(samplelot),":",conc$value," ",conc$unit," ", samplelot$FREQ_VARIED_COMPOUND)
    
  #contents <- paste0( buildCompound(samplelot),":",samplelot$FREQ_VARIED_COMPOUND_CONC," nM ", samplelot$FREQ_VARIED_COMPOUND)
  } else contents <- NA
 
  contents
  
} 


getConcentration <- function(samplelot)
{
  if(is.null(samplelot$FREQ_VARIED_COMPOUND_CONC) ){
    concentration <- 0   
  } else concentration <-as.character(as.numeric(samplelot$FREQ_VARIED_COMPOUND_CONC)/1000)
  
  
  concentration
  
} 


getVarcompound <- function(samplelot)
{
  if(is.null(samplelot$FREQ_VARIED_COMPOUND) ){
    var_compound <- buildCompound(samplelot)    
  } else var_compound<-samplelot$FREQ_VARIED_COMPOUND
  
  var_compound <- paste0( var_compound, " (",str_split(samplelot$Barcode,"-")[[1]][1],")")
  
  var_compound
  
} 





VariedCompound <- function(samplelot,compoundNum)
{
if( eval(parse(text=paste0("is.null(samplelot$FREQ_COMPOUND",compoundNum,")")  ))  ) {
  return(FALSE)
  } 
    
    
return(
  eval(parse(text =  paste0("samplelot$FREQ_COMPOUND",compoundNum, " == ",
                         " samplelot$FREQ_VARIED_COMPOUND" )
  ))

)
  
}

existsCompound <- function(samplelot,compoundNum)
{
  if( eval(parse(text=paste0("is.null(samplelot$FREQ_COMPOUND",compoundNum,")")  ))  ) {
    return(FALSE)
  } else return(TRUE) 
  
  
  
}


convertCompundConc <- function(conc)
  
{
##input is list(value = 100, unit = nM)  
##output same format  

  if( conc$unit != "nM") stop("I can only process nM inputs")
 
  
  if( conc$value < 10){ #return in nM
   
    
    
    
     return(list( value = conc$value, unit = conc$unit) ) 
    
    
  }
  
  if( conc$value < 1000000){ #return in uM
    value <- as.character(as.numeric(conc$value)/1000)
    
    return(list( value = value, unit = 'uM') )   
    
  } else {   #return mM
    
    value <- as.character(as.numeric(conc$value)/1000000)
    
    return(list( value = value, unit = 'mM') )   
    
    
    
  }
  
  
  
}

#convertCompundConc(conc1)
#convertCompundConc(conc2 )
#convertCompundConc(conc3 )
#convertCompundConc(conc4 )
      