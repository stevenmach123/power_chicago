## Category: Project
## Link
  - https://posit.cloud/content/6791509
      - the initial data processing require high CPU,memory, which exceed free tier, it need *25$/month* for higher processor, if you need see my project in action, contact me via [Linkedin](https://www.linkedin.com/in/khang-mach-b1b343164/) , I might keep it for another months :). Thank you for understanding!
  - Give it around 1-2min for download csv data
  - Demos on YouTube
## Languages 
  - Languages: R, Excel 
  - Library: Leaflet Renderer, MapView Renderer, Shiny UI.
## What is the project ?
  - Modeled interactive heatmap interface with R/Excel that allows user dynamically control properties filter, such as Electricity, Area, Months to observe & analyze specific 77 Chicago Neighborhood areas 
  - Processed two adjacent screen with their filtering controls in a single panel, allowing users to  compare quantitative properties of two communities.
## Tasks 
  <h3>&nbsp;&nbsp;<ins>Data Processing</ins></h3>
  
  - After connected to API keys, and get_decennial with `tinycensus` lib, we can import census dataframe that includes, *blocks vertices* and *GEO_ID*. Then, we can inner_join with `read.table` from `energy.cvs` file
  ```r
  census_api_key("6b39cc5661b7cefd31443142a5e20fcd31fc72bb",install=TRUE,overwrite=TRUE)
  readRenviron("~/.Renviron")
  evl1<-read.table(file = "energy11(v3).csv", sep = ",",skipNul="TRUE",header = TRUE)
  # get out some weird GEOID,and some community has long name
  names(evl1)[names(evl1)=="census_blocks"]  <- "GEOID"
  evl1 <- subset(evl1, !(is.na(GEOID)) & building_type !="")
  evl1$com  <- ifelse(str_detect(evl1$com,","),"Ohare",evl1$com)

  aa<-get_decennial(geography= "block",state= "IL", variables = "P001001",county ="Cook", geometry=TRUE)
  ay2 <- inner_join(aa,evl21,"GEOID")  %>% select(-c("value","variable","NAME")) #merge with shapefile
  ```
  <h3>&nbsp;&nbsp;<ins>MapView with data filtering</ins></h3>
  
  - Base in `ShinyUI`, we can select dropdown to filter by properties, including Gas, Electricity, Total population, etc.
  - Similarly, we can filter down through the pipeline across *Months*(1-12) and *Building Types* (Residents, Commercial, Industry)
  - By the default, the dropdown select *Electricity* and *All* on months and building types.
  - **Note: Only Gas and Electricity can be filtered down to *Months*(1-12), while other properties can be filtered with *Building Types***
  - Finally, we use MapView with `ay1` indicate the filtered dataframe, `zcol` indicate quantitative columns, `col.region` indicate color mapping  based on zcol intensity,
  ```r
  output$my_map <- renderLeaflet({
  
    evl11 <-set_type()
   
    
    if(input$property =="Electricity"){
      if(input$month == "1"){
        ho <- "kwh_1"
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_1 = sum(kwh_1)) 
      }
      #...
      if( input$month == "All"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type),total_kwh= sum(total_kwh) )
        ho <- "total_kwh"
      }
      
    }

   if(input$property == "Gas"){
      
      if(input$month == "1"){
        evl11 <-  evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_1 = sum(the_1)) 
        ho  <- "the_1"
         }
      #...
       if(input$month == "All"){ 
        evl11 <-  evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,total_the = sum(total_the)) 
        ho  <- "total_the"
      }
      
  }
  if(input$property == "Building Type"){
      evl11 <-  evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type) ) 
      ho <- "building_type"
    }

  ay1 <- inner_join(aa,evl11,"GEOID")  %>% select(-c("value","variable","NAME"))
  fcor <- find_cor(1)
  ftitle <- find_title(ho)
  mapview(ay1,zcol=ho,col.regions =fcor,layer.name = ftitle  )@map 
  })
  ```
  <h3>&nbsp;&nbsp;<ins>Plots for Sum Electricity and Gas across 12 months</ins></h3>
   
  - Have options to view particular sum of kWh and Gas within particular months 
  ```r
    output$sum_kwh_plot22  <- renderPlot({  # sum electricity  plot of screen2
    
    if(input$type2 != "All Buildings"){
      evl21 <- subset(evl1, com == input$com2 & building_type == input$type2  )
    }
    else{
      evl21  <- subset(evl1, com == input$com2)
    }   
    evl21  <- evl21  %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
    evl25<-by_list(evl21)
    evl25 <- data.frame(matrix(unlist(evl25), nrow=length(evl25), byrow=TRUE))
    
    names(evl25)  <- c("total")
    sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    sum_month$total <-evl25$total 
    
    evl271 <-sum_month   
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Kwh",title = paste("Sum Kwh 12 months for ",input$type2,"in",input$com2))
    }) 
  ```
  <h3>&nbsp;&nbsp;<ins>Task 2 (Adjacent screen comparison )</ins></h3>
  
  - This tasks we introduce an additional feature dropdown *Community* (contribute to previous data pipeline processing )
  - Use similar strategies, split window into 2 screen, so we can compare properties across *months*, *building* types between 2 *community*. Enhance comparison efficiency

  ```r
  task2_sti <- function(property,month,type,select_com,gr){
    #... similar to data filtering above
  }

  output$my_map22  <-renderLeaflet({ # map of screen2
    ay2 <- task2_sti(input$property2,input$month2,input$type2,input$com2,2)
    ay22 <-ay2[[1]]
    View(ay22)   
    fcoy2 <-find_coy2(input$color2)
    fcor2  <- find_cor(input$color2) 
    input$reset2
    op2 <- ay2[[2]]
    ftitle2  <- find_title(op2)
    ftitle2 <- paste(ftitle2,".",step="")
    
    if(op2=="building_type"){
      mapview(ay2[[1]],zcol=op2,col.regions =fcor2,layer.name= ftitle2 )@map
    }else {
      
      mapview(ay2[[1]],zcol =op2,col.regions =fcoy2,layer.name =ftitle2   )@map
    }
  })
  output$my_map21  <-renderLeaflet({ # map of screen1
    ay1 <- task2_sti(input$property1,input$month1,input$type1,input$com1,2)
    #...
    mapview(ay1[[1]],zcol=op1,col.regions =fcor1,layer.name= ftitle1 )@map

  })
  ```

## Step to Reproduce
 - [Here](https://docs.google.com/document/d/1rWl1_SQR6yItsA5kduAP07sGiJFFJH4xm76Z8LsdPIo/edit)
  ## 3.Run Project
  1. When at "power_chicago" directory, click pro3_1.R to view up the file code
  2. Click "Run" on Shiny App. Then just wait for data to process and view.
  * [image](./images/pic5.png)
