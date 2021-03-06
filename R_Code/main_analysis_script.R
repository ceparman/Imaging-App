 
main_script <- function(gfp_file,total_file,plate_map_file,filePath)
  
{

### create directory if it does not exist
  
filePath <- paste0(filePath,"/ImageAppoutput")  
  
  if(!dir.exists(filePath)) dir.create(filePath)   
  

  
plate <- makePlate()


plate_map <- parse_plate_map2(plate_map_file )



incProgress(.1)

data1 <- processReaderFile(gfp_file,plate) 

incProgress(.2)
data2 <- processReaderFile(total_file,plate) 

incProgress(.2)
#Combine data


all_data <- data.frame(Well=data1$Well,gfp_area=data1$`Total Area`,gfp_count=data1$`Total Count`,
                       allcells_area=data2$`Total Area`,allcells_count=data2$`Total Count`,
                       percent = 100*data1$`Total Area`/ data2$`Total Area`)
### Write output

incProgress(.1)

m<- merge(plate_map,all_data,by="Well")

m <- m[order(m$index),]

write.csv(m,file=paste0(filePath,"/All_data.csv"),row.names = FALSE)

incProgress(.1)

#calculate mean and STD of replicates
averaged <- m %>% filter(!is.na(var_compound)) %>%
       group_by(var_compound,concentration) %>%
  summarise(mean_gfp_area = mean(gfp_area,na.rm = TRUE), std_gfp_area = sd(gfp_area,na.rm = TRUE),
            mean_all_area =mean(allcells_area,na.rm = TRUE), std_all_area =sd(allcells_area,na.rm = TRUE),
            mean_percent = mean(percent,na.rm = TRUE)  , std_percent = sd(percent,na.rm = TRUE)
            ) 


content <-  plate_map %>% filter(!is.na(var_compound)) %>% select(content, var_compound, control, concentration, units) %>% unique

incProgress(.1)

s<- left_join(averaged,content,by=c("var_compound","concentration"))

s<- s[order(s$var_compound,s$concentration),]

incProgress(.1)
write.csv(s,file=paste0(filePath,"/summary_data.csv"),row.names = FALSE)


processed_data <- list(summary=s,well=m)



make_plots(processed_data,filePath)

incProgress(.1)



}




