#KHANG MACH  PROJECT3 CS424


library(shiny)
library(shinydashboard)
library(shinyBS)
library(leaflet)
library(ggplot2)
library(lubridate)
library(reshape2)

library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

library(dplyr)
library(mapview)
library(tigris)
library(tidycensus)
library(curl)
library(httr)
library(leafpop)
library(RColorBrewer)
library(stringr)
library(data.table)
library(rlang)
library(rsconnect)

set.seed(5)



census_api_key("6b39cc5661b7cefd31443142a5e20fcd31fc72bb",install=TRUE,overwrite=TRUE)
readRenviron("~/.Renviron")


evl1<-read.table(file = "energy11(v3).csv", sep = ",",skipNul="TRUE",header = TRUE)
### if file fail to load, write "~/Documents/SP2021/CS424/work4/energy11(v3).csv"


evl1$census_blocks <-substring(evl1$census_blocks,2)

print("give time to download file")
#####~~~~~~ THOSE LINE FOR DOWNLOAD BLOCK AND TRACT,I HAVE SEPARATE FILE CALLED prepare.r, YOU can have those as GLOBAL assigned before everything, and delete those aa,aa31 here to start faster 
#aa <-get_decennial(geography = "block", state ="IL", variables = "P001001", county="Cook",geometry = TRUE)
#aa31 <-get_acs(geography= "tract",state= "IL", variables = "B01003_001",county ="Cook", geometry=TRUE)
#aa31  <- aa31  %>% select(-c("moe","estimate","variable","NAME"))

aa<-get_decennial(geography= "block",state= "IL", variables = "P001001",county ="Cook", geometry=TRUE)




# edit data for task1-2
evl1$kwh_1 <- ifelse(is.na(evl1$kwh_1),0,evl1$kwh_1) 
evl1$kwh_2 <- ifelse(is.na(evl1$kwh_2),0,evl1$kwh_2) 
evl1$kwh_3 <- ifelse(is.na(evl1$kwh_3),0,evl1$kwh_3) 
evl1$kwh_4 <- ifelse(is.na(evl1$kwh_4),0,evl1$kwh_4) 
evl1$kwh_5 <- ifelse(is.na(evl1$kwh_5),0,evl1$kwh_5) 
evl1$kwh_6 <- ifelse(is.na(evl1$kwh_6),0,evl1$kwh_6) 
evl1$kwh_7 <- ifelse(is.na(evl1$kwh_7),0,evl1$kwh_7) 
evl1$kwh_8 <- ifelse(is.na(evl1$kwh_8),0,evl1$kwh_8) 
evl1$kwh_9 <- ifelse(is.na(evl1$kwh_9),0,evl1$kwh_9) 
evl1$kwh_10 <- ifelse(is.na(evl1$kwh_10),0,evl1$kwh_10) 
evl1$kwh_11 <- ifelse(is.na(evl1$kwh_11),0,evl1$kwh_11) 
evl1$kwh_12<- ifelse(is.na(evl1$kwh_12),0,evl1$kwh_12) 
evl1$total_kwh <-  ifelse(is.na(evl1$total_kwh),0,evl1$total_kwh) 


evl1$the_1 <- ifelse(is.na(evl1$the_1),0,evl1$the_1) 
evl1$the_2 <- ifelse(is.na(evl1$the_2),0,evl1$the_2) 
evl1$the_3 <- ifelse(is.na(evl1$the_3),0,evl1$the_3) 
evl1$the_4 <- ifelse(is.na(evl1$the_4),0,evl1$the_4) 
evl1$the_5 <- ifelse(is.na(evl1$the_5),0,evl1$the_5) 
evl1$the_6 <- ifelse(is.na(evl1$the_6),0,evl1$the_6) 
evl1$the_7 <- ifelse(is.na(evl1$the_7),0,evl1$the_7) 
evl1$the_8 <- ifelse(is.na(evl1$the_8),0,evl1$the_8) 
evl1$the_9 <- ifelse(is.na(evl1$the_9),0,evl1$the_9) 
evl1$the_10 <- ifelse(is.na(evl1$the_10),0,evl1$the_10) 
evl1$the_11 <- ifelse(is.na(evl1$the_11),0,evl1$the_11) 
evl1$the_12<- ifelse(is.na(evl1$the_12),0,evl1$the_12) 
evl1$total_the <-  ifelse(is.na(evl1$total_the),0,evl1$total_the) 

# get out some weird GEOID,and some community has long name
names(evl1)[names(evl1)=="census_blocks"]  <- "GEOID"
evl1 <- subset(evl1, !(is.na(GEOID)) & building_type !="")
evl1$com  <- ifelse(str_detect(evl1$com,","),"Ohare",evl1$com)



btypes<-unique(evl1$building_type)
btypes <- c("All Buildings", btypes) 

community <- unique(evl1$com)

# process data for task 3

evl3 <- subset(evl1, !is.na(total_units) & !is.na(total_population) & !is.na(house_size) )
#evls3  <- subset(evls3, !is.na(oc_unit_percent) & oc_unit_percent != 0 & !is.na(re_oc_percent) )
#evls3  <- subset(evls3, !is.na(re_oc_percent) )
evl3$GEOID  <- substr(evl3$GEOID,1,11) 
evl3$oc_unit_percent <- ifelse(is.na(evl3$oc_unit_percent),0,evl3$oc_unit_percent )
evl3$re_oc_percent <- ifelse(is.na(evl3$re_oc_percent),0,evl3$re_oc_percent ) 





# detect if it is mix buildings in same GEOID
type_building <-function(x){
  cur <-c()
  
  for(da in x){
    
    if(!(da %in% cur)){
      same <- TRUE
      cur <- c(cur,da)
      
    }
  }
  if(length(cur)>1){
   
    return("Mix building types")
  }
  else{
    return(cur[1])
  }
  
}



my_random1  <- function(x){
  index <- round(runif(1,min=0,max=length(x)),0)
  
  return(x[index[1]])
}
# multiple GEOID or not 

is_multiple <- function(x){
  if(length(x)>1){
    return("YES")
  }
  else {
    return("NO")
  }
}


# have all names of community of that track
my_random  <-function(x){

    uni <-c()
    wordy <- ""
    co<- 1
    if(length(x)==1){
      return(x[1])
    }
    else{
      
      for(i in x){
        if(!(i %in% uni)){
          uni <-c(uni,i)
          
        }
      }
      
      for(i in uni){
        if( co ==1){
          wordy <- i 
        }
        else{
       
          wordy <- paste(wordy,i,sep=",")
          
        }
        co <- co +1
        
      }
      
      return(wordy)
      
    
    }
}
  
  
  
  
  

# turn df with one row to list

by_list  <- function(x){
  num <-c()
  for(i in seq(1,ncol(x),by =1) ){
 
    num  <- c(num,x[1,i])
  }
  return(num)
}



ui  <- fluidPage(

  
  tags$head(
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');

        .t {
           text-align:center;             
        }
       
        .ac{
          display:flex; 
          flex-flow:row;
        }
        .b{
         /* border: 1px solid red; */
        }
        .at1{
           
           width:12%;
          /* border: 1px solid red;  */
        
         }
      .at2{
          margin-left:4px;
          margin-right:4px;
          width:100%;
        /*  border:1px solid red;  */
      }
      
      .at1 .a{
        margin-top:20px;
         height:min-content;
      }
      .w {
        height : 300px;
      }
      .u1{
        font-size:20px; 
        margin-left:40%;
      }
      #txt_tract31{
        font-size:18px;
      }
       #txt_tract32{
        font-size:18px;
      }
}
        
    "))
    
    
  ),

    
  
      tabsetPanel(type = "tabs",
                  tabPanel("Task1",
                           fluidRow(
                                   
                                    column(width=3,selectInput("property",label="Properties",c("Gas","Electricity","Building Age","Building Type","Building Height","Total Population"),selected="Electricity")),
                                    column(width=3,selectInput("month",label="Month",c("All","1","2","3","4","5","6","7","8","9","10","11","12"),selected="All")),
                                    column(width =3,selectInput("type","Building Type",btypes,selected="All Buildings")  ),
                                    column(width =2,style="margin-top:1%",bsButton("reset",label="View reset"))
                                    

                           ),
                           fluidRow( 
                             
                           column(width=12,mapviewOutput("my_map",height=400))
                           ),
                           fluidRow(
                             column(width=6,class="b",plotOutput("sum_kwh_plot",height=230)),
                             column(width=6,class="b",plotOutput("sum_the_plot",height=230))
                           ),
                           fluidRow(
                             column(width=12,tableOutput("table1"))
                           )
                           
                  )
                  ,tabPanel("Task2",
                            div(class="ac",
                                div(class="at1",
                                    column(width=12,selectInput("color1","Colors",c(1,2,3),selected =1 )),
                                    column(width=12,class="a",selectInput("property1",label="Properties",c("Gas","Electricity","Building Age","Building Type","Building Height","Total Population"),selected="Electricity")),
                                    column(width=12,class="a",selectInput("month1",label="Month",c("All","1","2","3","4","5","6","7","8","9","10","11","12"),selected="All")),
                                    column(width =12,class="a",selectInput("type1","Building Type",btypes,selected="All Buildings")  ),
                                    column(width =12,class="a",selectInput("com1","Community",community, selected ="Near West Side" )),
                                    column(width =12,bsButton("reset1",label="View reset"))      
                                    
                                    
                                    ),
                                div(class="at2",
                                   tabsetPanel(type ="tabs",
                                                tabPanel("Task2.1",
                                                         fluidRow(width=12,
                                                                  column(width=6,mapviewOutput("my_map21") ),
                                                                  column(width=6,mapviewOutput("my_map22"))
                                                         )
                                                ),
                                                tabPanel("Task2.2",
                                                         fluidRow(
                                                         
                                                           column(width =6, p(class ="u1","Screen1"),plotOutput("sum_kwh_plot21",height =210),plotOutput("sum_the_plot21",height=210) ),
                                                           column(width =6,p(class ="u1","Screen2"),plotOutput("sum_kwh_plot22",height =210),plotOutput("sum_the_plot22",height=210) )
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=12,p("Screen 1 Table "),tableOutput("table21"))
                                                           
                                                         ),
                                                         fluidRow(
                                                           column(width=12,p("Screen 2 Table "),tableOutput("table22"))
                                                         )
                                                )
                                                
                                    )
                                    
                                
                                  ),
                                div(class="at1",
                                    column(width=12,selectInput("color2","Colors",c(1,2,3),selected =1 )),
                                    column(width=12,class="a",selectInput("property2",label="Properties",c("Gas","Electricity","Building Age","Building Type","Building Height","Total Population"),selected="Electricity")),
                                    column(width=12,class="a",selectInput("month2",label="Month",c("All","1","2","3","4","5","6","7","8","9","10","11","12"),selected="All")),
                                    column(width =12,class="a",selectInput("type2","Building Type",btypes,selected="All Buildings")  ),
                                    column(width =12,class="a",selectInput("com2","Community",community, selected ="Loop" )),
                                    column(width =12,bsButton("reset2",label="View reset"))      
                                           
                                    ),
                                
                            )
                    
                  ),
                  tabPanel("Task 3", 
                        div(class = "ac",
                           div(class="at1",
                               column(width=12,selectInput("color31","Colors",c(1,2,3),selected =1 )),
                               column(width =12,class ="a",selectInput("property31",label="Properties",c("Gas","Electricity","Building Age","Building Type","Building Height","Total Population"),selected="Electricity")),
                               column(width=12,class="a",selectInput("month31",label="Month",c("All","1","2","3","4","5","6","7","8","9","10","11","12"),selected="All")),
                               column(width =12,class="a",selectInput("type31","Building Type",btypes,selected="All Buildings")  ),
                               column(width =12,bsButton("reset31",label="View reset")) ,
                               column(width =12,class ="a", selectInput("ten31","10% Track Property",c("Oldest Building","Newest Building","Tallest Building","Most Electricity","Most Gas","Most Population","Most Residental %","Most Renter %"),selected="Newest Building")) 
                               
                               ),
                           div(class="at2",
                               tabsetPanel(type ="tab",
                                           tabPanel("Task 3.1",
                                                  column(width=6,class="b",mapviewOutput("my_map31")), 
                                                  column(width=6,class="b",mapviewOutput("my_map32")), 
                                            ),
                                           tabPanel("Task 3.2",
                                                    fluidRow(
                                                      
                                                      column(width =6, p(class ="u1","Screen1"),plotOutput("sum_kwh_plot31",height =210),plotOutput("sum_the_plot31",height=210) ),
                                                      column(width =6,p(class ="u1","Screen2"),plotOutput("sum_kwh_plot32",height =210),plotOutput("sum_the_plot32",height=210) )
                                                    ),
                                                    
                                                    fluidRow(
                                                      column(width=12,p("Screen 1 Table "),tableOutput("table31"))
                                                      
                                                    ),
                                                    fluidRow(
                                                      column(width=12,p("Screen 2 Table "),tableOutput("table32"))
                                                    )   
                                            ),
                                           tabPanel("Task 3.3",
                                                
                                                    column(width=6,textOutput("txt_tract31"),mapviewOutput("my_tract31")),
                                                    column(width=6,textOutput("txt_tract32"),mapviewOutput("my_tract32"))            
                                                         
                                                    
                                           )
                                             
                                           
                                           
                               )
                           ),
                              
                           div(class="at1",
                               column(width=12,selectInput("color32","Colors",c(1,2,3),selected =1 )),
                               
                               column(width =12,class ="a",selectInput("property32",label="Properties",c("Gas","Electricity","Building Age","Building Type","Building Height","Total Population"),selected="Electricity")),
                               column(width=12,class="a",selectInput("month32",label="Month",c("All","1","2","3","4","5","6","7","8","9","10","11","12"),selected="All")),
                               column(width =12,class="a",selectInput("type32","Building Type",btypes,selected="All Buildings")  ),
                               column(width =12,bsButton("reset32",label="View reset")),
                               column(width =12,class ="a",selectInput("ten32", "10% Track Property",c("Oldest Building","Newest Building","Tallest Building","Most Electricity","Most Gas","Most Population","Most Residental %","Most Renter %"),selected="Oldest Building"))
                               
                            )
                               
                        )        
                  ), 
                  tabPanel("About",
                        div( style = "width:40%;border: 1px solid red;word-wrap :normal",
                        p(style="font-size:20px","Khang Mach"),
                        p("https://www.kaggle.com/chicago/chicago-energy-usage-2010"),
                        p("The data contains per census block of Chicago communities area.Each census block may have multiple lines where each line is for a different building type or building subtype."),
                        p("Can also have  electricity usage per month and the gas usage per month, the total population, average number of stories for the buildings and the average age of the buildings ")
                        )
                  )
                  
                  ,tabPanel("Taskn",
                           div(class= "ac",
                             div(class="a",p("aa"),textOutput("s")),
                             div(tabsetPanel(type = "tabs",tabPanel("t")))
                             
                           ) 
                            
                            
                  )
                  
           
                  
      )
      
)


#task 2 stimulation for 2 maps

task2_sti <- function(property,month,type,select_com,gr){

  if(type != "All Buildings"){
    evl21 <- subset(evl1,  building_type == type )
  }
 else {
   evl21<-evl1
 }
   View(evl21)

  

  # summarize info of a group that have same GEOID, also prefer to accumulate numeric columns
  if(property =="Electricity"){ # accumualte by kwh_1...kwh_12 or total_kwh
    if(month == "1"){
      #assign(coi,"kwh_1",envir =parent.frame())
      ho1 <- "kwh_1"   # remember what column select from 
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_1 = sum(kwh_1)) 
    }
    if(month == "2"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_2 = sum(kwh_2)) 
      ho1 <- "kwh_2"
       #assign(coi,"kwh_2",envir =parent.frame())
    }
    if(month == "3"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type),kwh_3 = sum(kwh_3)) 
      #assign(coi,"kwh_3",envir =parent.frame())
      ho1 <- "kwh_3"
    }
    if(month == "4"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_4 = sum(kwh_4)) 
      #assign(coi,"kwh_4",envir =parent.frame())
      ho1 <- "kwh_4"
    }
    if(month == "5"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_5 = sum(kwh_5)) 
      #assign(coi,"kwh_5",envir =parent.frame())
      ho1 <- "kwh_5"
    }
    if(month == "6"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type),kwh_6 = sum(kwh_6)) 
      #assign(coi,"kwh_6",envir =parent.frame())
      ho1 <- "kwh_6"
    }
    if(month == "7"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_7 = sum(kwh_7)) 
      #assign(coi,"kwh_7",envir =parent.frame())
      ho1 <- "kwh_7"
    }
    if(month == "8"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_8 = sum(kwh_8)) 
      #assign(coi,"kwh_8",envir =parent.frame())
      ho1 <- "kwh_8"
    }
    if(month == "9"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_9 = sum(kwh_9)) 
      #assign(coi,"kwh_9",envir =parent.frame())
      ho1 <- "kwh_9"
    }
    if(month == "10"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_10 = sum(kwh_10)) 
      #assign(coi,"kwh_10",envir =parent.frame())
      ho1 <- "kwh_10"
    }
    if(month == "11"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_11 = sum(kwh_11)) 
      #assign(coi,"kwh_11",envir =parent.frame())
      ho1  <- "kwh_11"
    }
    if(month == "12"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_12 = sum(kwh_12)) 
      #assign(coi,"kwh_12",envir =parent.frame())
      ho1 <- "kwh_12"
    }
    if( month == "All"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type),total_kwh= sum(total_kwh) )
      #assign(coi,"total_kwh",envir =parent.frame())
      ho1  <- "total_kwh"
    }
    
    
  }
  if(property == "Gas"){  #group GEOID and accumualte by the_1...the_12 or total_the
    
    if(month == "1"){
      evl21 <-  evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_1 = sum(the_1)) 
     # assign(coi,"the_1",envir =parent.frame())
      ho1  <-  "the_1"
    }
    if(month == "2"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_2 = sum(the_2)) 
      #assign(coi,"the_2",envir =parent.frame())
      ho1  <-  "the_2"
    }
    if(month == "3"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type),the_3 = sum(the_3)) 
      #assign(coi,"the_3",envir =parent.frame())
      ho1  <-  "the_3"
    }
    if(month == "4"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_4 = sum(the_4)) 
      #assign(coi,"the_4",envir =parent.frame())
      ho1  <-  "the_4"
    }
    if(month == "5"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_5 = sum(the_5)) 
     # assign(coi,"the_5",envir =parent.frame())
      ho1  <-  "the_5"
    }
    if(month == "6"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type),the_6 = sum(the_6)) 
      #assign(coi,"the_6",envir =parent.frame())
      ho1  <-  "the_6"
    }
    if(month == "7"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_7 = sum(the_7)) 
      #assign(coi,"the_7",envir =parent.frame())
      ho1  <-  "the_7"
    }
    if(month == "8"){
      evl21 <-  evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_8 = sum(the_8)) 
      #assign(coi,"the_8",envir =parent.frame())
      ho1  <-  "the_8"
    }
    if(month == "9"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_9 = sum(the_9)) 
     # assign(coi,"the_9",envir =parent.frame())
      ho1  <-  "the_9"
    }
    if(month == "10"){
      evl21 <-evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_10 = sum(the_10)) 
      #assign(coi,"the_10",envir =parent.frame())
      ho1  <-  "the_10"
    }
    if(month == "11"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_11 = sum(the_11)) 
      #assign(coi,"the_11",envir =parent.frame())
      ho1  <-  "the_11"
    }
    if(month == "12"){
      evl21 <- evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_12 = sum(the_12)) 
      #assign(coi,"the_12",envir =parent.frame())
      ho1  <-  "the_12"
    }
    if(month == "All"){ 
      evl21 <-  evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,total_the = sum(total_the)) 
      #assign(coi,"total_the",envir =parent.frame())
      ho1  <-  "total_the"
      
    }
    
    
  }
  if(property == "Building Age"){
    evl21 <-  evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,building_age = mean(building_age)) 
    #assign(coi,"building_age",envir =parent.frame())
    ho1 <- "building_age"
  }
  if(property == "Building Type"){
    evl21 <-  evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type) ) 
    #assign(coi,"building_type",envir =parent.frame())
    ho1  <- "building_type"
  }
  if(property == "Building Height"){
    evl21 <-  evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), stories = mean(stories)) 
    #assign(coi,"stories",envir =parent.frame())
    ho1  <-"stories"
  }
  
  if(property == "Total Population"){
    evl21$total_population  <- ifelse(is.na(evl21$total_population),0,evl21$total_population ) 
    evl21 <-  evl21 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), total_population = sum(total_population)) 
    #assign(coi,"total_population",envir =parent.frame())
    ho1  <- "total_population"
  }
  
 
  evl21 <-evl21 %>% filter(str_detect(community,select_com)==TRUE)  # detect unique GEOID that have that community name
  ay2 <- inner_join(aa,evl21,"GEOID")  %>% select(-c("value","variable","NAME")) #merge with shapefile
  #View(ay2)
  list(ay2,ho1)
  
  
  
  
  
  
  
  
}

#task3 stimulation for 2 map 
task3_sti  <- function(property,month,type,gr){
  
  if( type != "All Buildings"){
    evl31 <- subset(evl3,building_type ==type )
  }
  else{
    evl31<- evl3
  }
  ho1 <- "-"
  # here, group by tract GEOID, and again accumulate kwh,the as numeric for each  unique GEOID 
  if(property == "Electricity"){# accumulate for kwh_1..kwh_12 and total_kwh
    if(month =="1"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_1 = sum(kwh_1))
      ho1 <-"kwh_1"
    }
    else if(month =="2"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_2 = sum(kwh_2))
      ho1 <-"kwh_2"
    }
    else if(month =="3"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_3 = sum(kwh_3))
      ho1 <-"kwh_3"
    }
    else if(month =="4"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_4 = sum(kwh_4))
      ho1 <-"kwh_4"
    } 
    else if(month =="5"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_5 = sum(kwh_5))
      ho1 <-"kwh_5"
    }
    else if(month =="6"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_6 = sum(kwh_6))
      ho1 <-"kwh_6"
    }
    else if(month =="7"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_7 = sum(kwh_7))
      ho1 <-"kwh_7"
    }
    else if(month =="8"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_8 = sum(kwh_8))
      ho1 <-"kwh_8"
    }
    else if(month =="9"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_9 = sum(kwh_9))
      ho1 <-"kwh_9"
    }
    else if(month =="10"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_10 = sum(kwh_10))
      ho1 <-"kwh_10"
    }
    else if(month =="11"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_11 = sum(kwh_11))
      ho1 <-"kwh_11"
    }
    else if(month =="12"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), kwh_12 = sum(kwh_12))
      ho1 <-"kwh_12"
    }
    else if(month =="All"){
      evl31  <-evl31 %>% group_by(GEOID) %>%  summarise(Multiple = is_multiple(GEOID),building_type  =type_building(building_type), total_kwh = sum(total_kwh))
      ho1  <- "total_kwh"
    }
    
    
    
  }
  else if( property =="Gas"){ # accumulate for the1..the12 and total_the
    if(month =="1"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_1 = sum(the_1))
      ho1 <-"the_1"
    }
    else if(month =="2"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_2 = sum(the_2))
      ho1 <-"the_2"
    }
    else if(month =="3"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_3 = sum(the_3))
      ho1 <-"the_3"
    }
    else if(month =="4"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_4 = sum(the_4))
      ho1 <-"the_4"
    }
    else if(month =="5"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_5 = sum(the_5))
      ho1 <-"the_5"
    }
    else if(month =="6"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_6 = sum(the_6))
      ho1 <-"the_6"
    }
    else if(month =="7"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_7 = sum(the_7))
      ho1 <-"the_7"
    }
    else if(month =="8"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_8 = sum(the_8))
      ho1 <-"the_8"
    }
    else if(month =="9"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_9 = sum(the_9))
      ho1 <-"the_9"
    }
    else if(month =="10"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_10 = sum(the_10))
      ho1 <-"the_10"
    }
    else if(month =="11"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_11 = sum(the_11))
      ho1 <-"the_11"
    }
    else if(month =="12"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), the_12 = sum(the_12))
      ho1 <-"the_12"
    }
    else if(month =="All"){
      evl31  <-evl31 %>% group_by(GEOID)  %>% summarise(Multiple = is_multiple(GEOID), building_type  = type_building(building_type), total_the = sum(total_the))
      ho1 <-"total_the"
    }
  }
  else if(property == "Building Type"){
    evl31  <-evl31 %>% group_by(GEOID) %>%  summarise(Multiple = is_multiple(GEOID),building_type  =type_building(building_type))
    ho1  <- "building_type"
    
  }
  else if(property == "Building Height"){
    evl31  <-evl31 %>% group_by(GEOID) %>%  summarise(Multiple = is_multiple(GEOID),building_type  =type_building(building_type), stories = mean(stories))
    ho1  <- "stories"
  }
  else if(property == "Building Age"){
    evl31  <-evl31 %>% group_by(GEOID) %>%  summarise(Multiple = is_multiple(GEOID),building_type  =type_building(building_type), building_age = mean(building_age))
    ho1  <- "building_age"
  }
  else if(property == "Total Population"){
    evl31  <-evl31 %>% group_by(GEOID) %>%  summarise(Multiple = is_multiple(GEOID),building_type  =type_building(building_type), total_population = mean(total_population))
    ho1  <- "total_population"
  
  }
   
  evl311 <- inner_join(aa,evl31) # merge evl31 with tract lv shapefile
  View(evl311)
  list(evl311,ho1)  # return as a list of 2 variable, 
  
}


# find legend title for task3.3
find_title3 <-function(t){
  if(t == "Oldest Building"){
    return("Oldest average age")
  }
  else if(t== "Newest Building"){
    return("Newest average age")
  }
  else if(t == "Tallest Building" ){
    return("Most average tallest")
  }
  
  else if(t== "Most Electricity") {
    return("Most total kwh")
  }
  else if(t == "Most Gas"){
    return("Most total thermal")
  }
  else if( t== "Most Residental %"){
    return("Most average residental occupied %")
  }
  else if( t==  "Most Renter %"){
    return("Most average renter occupied %")
  }
  else if(t=="Most Population"){
    return("Most total population")
  }
  else{
    return(t)
  }
}

# tract 10% evaluation to take 10% of data based on input$ten
ten_tract  <-function(ten,type){ 
  
  if( type != "All Buildings"){
    evl32   <-subset(evl3,building_type ==type ) 
  }
  else{
    evl32  <- evl3
  }
  
  fl32 <-data.frame(x=c(""),y=c("")) 
  fl32 <- subset(fl32, x != "")
  
  if(ten == "Oldest Building"){ 
    names(fl32) <-c("GEOID","avg_age") 
    evl32  <- evl32 %>% group_by(GEOID) %>% summarise(avg_age = mean(building_age)) 
    len  <- as.integer(nrow(evl32) * 0.1)
    
    for(i in seq(1,len,by =1)){ #loop to take one out and find max of next update evl32
      nmax <-max(evl32$avg_age)
      ar3 <- evl32[evl32$avg_age == nmax, ]  # max  avg_age
      ar3$avg_age  <- round(ar3$avg_age,2)
      fl32 <- rbind(fl32,ar3)     # bind max row with fl32
      evl32  <-subset(evl32,evl32$avg_age != nmax)   # next update evl32
      
    }
    ho1  <- "avg_age"
  }
  else if(ten == "Newest Building"){
    names(fl32) <-c("GEOID","avg_age") 
    evl32  <- evl32 %>% group_by(GEOID) %>% summarise(avg_age = mean(building_age)) 
    len  <- as.integer(nrow(evl32) * 0.1)
    for(i in seq(1,len,by =1)){
      nmin <-min(evl32$avg_age)
      ar3 <- evl32[evl32$avg_age == nmin, ]      # min  avg_age
      ar3$avg_age  <- round(ar3$avg_age,2)
      fl32 <- rbind(fl32,ar3)
      evl32  <-subset(evl32,evl32$avg_age != nmin)
    }
    ho1  <- "avg_age"
  }
  
  else if(ten == "Tallest Building"){
    names(fl32) <-c("GEOID","avg_height") 
    evl32  <- evl32 %>% group_by(GEOID) %>% summarise(avg_height = mean(stories)) 
    len  <- as.integer(nrow(evl32) * 0.1)
    
    for(i in seq(1,len,by =1)){
      nmax <-max(evl32$avg_height)
      ar3 <- evl32[evl32$avg_height == nmax, ]  
      ar3$avg_height  <- round(ar3$avg_height,2)   #   max avg_height
      fl32 <- rbind(fl32,ar3)
      evl32  <-subset(evl32,evl32$avg_height != nmax)
    }
    ho1 <- "avg_height"
  }
  else if(ten  ==  "Most Electricity" ){
    names(fl32) <-c("GEOID","total_kwh") 
    evl32  <- evl32 %>% group_by(GEOID) %>% summarise(total_kwh  = sum(total_kwh)) 
    len  <- as.integer(nrow(evl32) * 0.1)
    
    for(i in seq(1,len,by =1)){
      nmax <-max(evl32$total_kwh)
      ar3 <- evl32[evl32$total_kwh == nmax, ]     # max total_kwh
      ar3$total_kwh  <- round(ar3$total_kwh,2)   
      fl32 <- rbind(fl32,ar3)
      evl32  <-subset(evl32,evl32$total_kwh != nmax)
    }
    ho1 <- "total_kwh"
    
  }
  else if( ten  == "Most Gas"){
    names(fl32) <-c("GEOID","total_the") 
    evl32  <- evl32 %>% group_by(GEOID) %>% summarise(total_the  = sum(total_the)) 
    len  <- as.integer(nrow(evl32) * 0.1)
    
    for(i in seq(1,len,by =1)){
      nmax <-max(evl32$total_the)
      ar3 <- evl32[evl32$total_the == nmax, ]      #max total_the
      ar3$total_the  <- round(ar3$total_the,2)     
      fl32 <- rbind(fl32,ar3)
      evl32  <-subset(evl32,evl32$total_the != nmax)
    }
    ho1 <- "total_the"
    
  }
  else if(ten == "Most Residental %"){
    names(fl32) <-c("GEOID","resident_percent") 
    evl32  <- evl32 %>% group_by(GEOID) %>% summarise(resident_percent  = mean(oc_unit_percent)) 
    len  <- as.integer(nrow(evl32) * 0.1)
    
    for(i in seq(1,len,by =1)){
      nmax <-max(evl32$resident_percent)
      ar3 <- evl32[evl32$resident_percent == nmax, ]  
      ar3$resident_percent  <- round(ar3$resident_percent,2)
      fl32 <- rbind(fl32,ar3)
      evl32  <-subset(evl32,evl32$resident_percent != nmax)
    }
    ho1 <- "resident_percent"
  }
  else if(ten == "Most Renter %"){
    names(fl32) <-c("GEOID","renter_percent") 
    evl32  <- evl32 %>% group_by(GEOID) %>% summarise(renter_percent  = mean(re_oc_percent)) 
    len  <- as.integer(nrow(evl32) * 0.1)
    
    for(i in seq(1,len,by =1)){
      nmax <-max(evl32$renter_percent)
      ar3 <- evl32[evl32$renter_percent == nmax, ]  
      ar3$renter_percent  <- round(ar3$renter_percent,2)
      fl32 <- rbind(fl32,ar3)
      evl32  <-subset(evl32,evl32$renter_percent != nmax)
    }
    ho1 <- "renter_percent"
    
  }   
  else if( ten ==  "Most Population"){
    names(fl32) <-c("GEOID","total_population")  
    
    evl32  <- evl32 %>% group_by(GEOID) %>% summarise(total_population = sum(total_population)) 
    len  <- as.integer(nrow(evl32) * 0.1)
    
    for(i in seq(1,len,by =1)){
      nmax <-max(evl32$total_population)
      ar3 <- evl32[evl32$total_population == nmax, ]  
      ar3$total_population  <- round(ar3$total_population,2)
      fl32 <- rbind(fl32,ar3)
      evl32  <-subset(evl32,evl32$total_population != nmax)
    }
    ho1 <- "total_population"
    
  }
  
  
  
  View(fl32)
  evl37  <- inner_join(aa,fl32,"GEOID")   #join with shapefile
  print("ss")
  list(evl37,ho1)
  
} 






# find good title legend for task 1-2
find_title <- function(x){ 
 
  id <- ""
  if(str_detect(x,"the_")){
    t <- substring(x,nchar(x)-1)
    a <-FALSE
    if(t == "11" | t== "12" | t=="13" |t=="10"){
      a<-TRUE 
    }
    if(a==FALSE){ 
      id <-paste("Thermal"," month ",substring(x,nchar(x)) ,sep ="") 
    }
    if(a==TRUE){
      id <-paste("Thermal"," month ",t,sep ="")  
    }
    
  }
  else if(str_detect(x,"kwh_")){
    t <- substring(x,nchar(x)-1)
    a <-FALSE
    if(t == "11" | t== "12" | t=="13" |t=="10"){
      a<-TRUE 
    }
    if(a==FALSE){
      id <- paste("Kwh"," month ",substring(x,nchar(x)),sep ="")  
    }
    else{
      id <- paste("Kwh"," month ",t,sep ="") 
    }
   
  }
  else if(x == "building_age"){
    id <- "Building age average"
  }
  else if(x== "building_type"){
    id <- "Building type"
  }
  else if(x== "stories"){
    id <- "Building height average"
  }
  else if(x== "total_kwh"){
    id <-  "Total kwh 12 months" 
    print("--total_kwh")
  }
  else if(x== "total_the"){
    id  <-  "Total thermal 12 months"
  }
  else if(x== "total_population"){
    id <-  "Total population"
  }
  
  return(id)
}







# color palete, those are I create, no use pallete

coy1 <- c("#34eb6e","#ebd334","#f0d573","#ed9261","#eb8934","#eb6834","#ae8aeb","#d034eb")
coy2   <- c( "#9ecae1","#afccfa","#6eabf5","#3182bd","#baaffa","#b85aae","#8877ed","#756bb1")
coy3  <- c("#5ab4ac","#15bfbf","#aff4fa","#c7afa5","#c99e89","#bf8265","#c2b85d","#ada76c","#a8874d")
cor1 <- c("#34eb6e","#ebd334","#eb8934","#32a6a8","#a83240","#d034eb")
cor2 <- c("#9ecae1","#3182bd","#baaffa","#756bb1","#b85aae","#afccfa")
cor3 <-c("#5ab4ac","#aff4fa","#ada76c","#bf8265","#c7afa5","#a8874d" ) 
find_coy <-function(x){  # use for 3 option color of numeric 
  if(x==1){
    return(coy1)
  }
  if(x==2){
    return(coy2)
  }
  if(x==3){
    return(coy3)
  }
} 
find_cor  <- function(x){  # use for 3 option color of building_type
  if(x==1){
    return(cor1)
  }
  if(x==2){
    return(cor2)
  }
  if(x==3){
    return(cor3)
  }
}
# #eb34d3

coy21 <- c("#34eb6e","#ebd334","#f0d573","#ed9261","#eb8934","#eb6834","#ae8aeb","#d034eb")
coy22   <- c("#9ecae1","#d61df2","#eb34d3","#c21588","#f21d92","#6816f5","#756bb1","#c21515")
coy23  <- c("#5ab4ac","#c2b85d","#c99e89","#ada76c","#a8874d")
find_coy2 <-function(x){  
  if(x==1){
    return(coy1)
  }
  if(x==2){
    return(coy22)
  }
  if(x==3){
    return(coy23)
  }
} 

#ho1 <- ""
hoi1 <- ""
hoi2  <-""
server <- function(input,output,session){
  #####TASK 3   - I put it first, so do code more efficient when scroll
  output$txt_tract31 <-renderText({ # give text for 10% tract, when group data by building_type
    ftitle <- find_title3(input$ten31)
    paste("10%","in","Chicago",input$type31,"that","have",ftitle)
    
  })
  
  output$txt_tract32 <-renderText({
    ftitle <- find_title3(input$ten32)
    paste("10%","in","Chicago",input$type32,"that","have",ftitle)
    
  })
  
  output$my_tract31  <- renderLeaflet({  # map1 of task   (10% tract)
    input$reset31
    ayo1 <- ten_tract(input$ten31,input$type31)
    print("q1")
    ftitle <- find_title3(input$ten31)
    View(ayo1[[1]])
    fcolor <-  find_coy2(input$color31)
    mapview(ayo1[[1]],zcol = ayo1[[2]] ,layer.name = ftitle ,col.regions = fcolor )@map
    
    
  })
  
  
  output$my_tract32  <- renderLeaflet({ # map2 of task 3.3
    input$reset32
    ayo2 <- ten_tract(input$ten32,input$type32)
    ftitle <- find_title3(input$ten32)
    ftitle <- paste(ftitle,".",sep ="") 
    fcolor <-  find_coy2(input$color32) 
    View(ayo2[[1]])
    mapview(ayo2[[1]],zcol = ayo2[[2]] ,layer.name = ftitle ,col.regions = fcolor )@map
    
  })
  
  
  
  
  output$my_map31  <- renderLeaflet({  #map1 of task3.1
    input$reset31
    ay31 <- task3_sti(input$property31,input$month31,input$type31,1)
    fcoy  <- find_coy2(input$color31) 
    fcor <- find_cor(input$color31)
    ftitle <- find_title(ay31[[2]])
    View(ay31[[1]])
    
    if(ay31[[2]] == "building_type"){
      mapview(ay31[[1]],zcol = ay31[[2]] ,col.regions =fcor , layer.name = ftitle )@map 
      
    }
    else{
      mapview(ay31[[1]],zcol = ay31[[2]] ,col.regions =fcoy, layer.name = ftitle )@map 
    }
  })
  
  output$my_map32  <- renderLeaflet({    #map2 of task3.1
    input$reset32
    ay32 <- task3_sti(input$property32,input$month32,input$type32,1)
    fcoy  <- find_coy2(input$color32) 
    fcor <- find_cor(input$color32)
    ftitle <- find_title(ay32[[2]])
    View(ay32[[1]])
    if(ay32[[2]] == "building_type"){
      mapview(ay32[[1]],zcol = ay32[[2]] ,col.regions =fcor , layer.name = paste(ftitle,".")  )@map 
      
    }
    else{
      mapview(ay32[[1]],zcol = ay32[[2]] ,col.regions =fcoy, layer.name = paste(ftitle,".") )@map 
    }
  })
  
  
  
  
  
 
  output$sum_the_plot31  <- renderPlot({ #plot sum gas  of screen1 task 3.2
    
      if(input$type31 !="All Buildings"){
        evl31 <- subset(evl3,building_type ==input$type31 )
      }
      else {
        evl31  <- evl3
      }
      evl31 <- evl31   %>% summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
      
      
      evl31<-by_list(evl31) 
      evl25 <- data.frame(matrix(unlist(evl31), nrow=length(evl31), byrow=TRUE))
    
      names(evl25)  <- c("total")
      
      sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
      sum_month$total <-evl25$total 
      
      ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Cubic meters",title = paste("Sum Gas 12 months of Chicago for ",input$type31))
    
    
  })
  
  
  
  output$sum_kwh_plot31  <- renderPlot({  #plot sum electricity  of screen1 task 3.2
    
    if(input$type31 !="All Buildings"){
      evl31 <- subset(evl3,building_type ==input$type31 )
    }
    else {
      evl31  <- evl3
    }
    evl31 <- evl31   %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
    
    
    evl31<-by_list(evl31) 
    evl25 <- data.frame(matrix(unlist(evl31), nrow=length(evl31), byrow=TRUE))
    
    names(evl25)  <- c("total")
    
    sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    sum_month$total <-evl25$total 
    
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Cubic meters",title = paste("Sum Electricity 12 months of Chicago for ",input$type31))
    
    
  })
  
  output$sum_the_plot32  <- renderPlot({    #plot sum gas  of screen2 task 3.2
    
    if(input$type32 !="All Buildings"){
      evl31 <- subset(evl3,building_type ==input$type32 )
    }
    else {
      evl31  <- evl3
    }
    evl31 <- evl31   %>% summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
    
    
    evl31<-by_list(evl31) 
    evl25 <- data.frame(matrix(unlist(evl31), nrow=length(evl31), byrow=TRUE))
    
    names(evl25)  <- c("total")
    
    sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    sum_month$total <-evl25$total 
    
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Cubic meters",title = paste("Sum Gas 12 months of Chicago for ",input$type32))
    
    
  })
  output$sum_kwh_plot32  <- renderPlot({ #plot sum electricity   of screen2 task 3.2
    
    if(input$type32 !="All Buildings"){
      evl31 <- subset(evl3,building_type ==input$type32 )
    }
    else {
      evl31  <- evl3
    }
    evl31 <- evl31   %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
    
    
    evl31<-by_list(evl31) 
    evl25 <- data.frame(matrix(unlist(evl31), nrow=length(evl31), byrow=TRUE))
    
    names(evl25)  <- c("total")
    
    sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    sum_month$total <-evl25$total 
    
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Cubic meters",title = paste("Sum Electricity 12 months of Chicago for ",input$type32))
    
    
  })
  
  
  
  output$table31 <- renderTable({  # table of screen1 task 3.2
    if(input$type31 !="All Buildings"){
      evl31 <- subset(evl3,building_type ==input$type31 )
    }
    else {
      evl31  <- evl3
    }
    
    evl32 <- evl31 %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
    evl33 <- evl31 %>% summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
    
    evl12<- by_list(evl32)
    evl13<- by_list(evl33)
    
    evl14<-c(evl12,evl13)
    evl14  <- as.integer(evl14) 
    evl15 <- data.frame(matrix(unlist(evl14),nrow =2,byrow=TRUE))
    colnames(evl15) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
    rownames(evl15) <- c("Kwh","Gas")  
    
    evl15 
    
    
  },include.rownames=TRUE)
  
  output$table32 <- renderTable({    # table of screen2 task 3.2
    if(input$type32 !="All Buildings"){
      evl31 <- subset(evl3,building_type ==input$type32 )
    }
    else {
      evl31  <- evl3
    }
    
    evl32 <- evl31 %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
    evl33 <- evl31 %>% summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
    
    evl12<- by_list(evl32)
    evl13<- by_list(evl33)
    
    evl14<-c(evl12,evl13)
    evl14  <- as.integer(evl14) 
    evl15 <- data.frame(matrix(unlist(evl14),nrow =2,byrow=TRUE))
    colnames(evl15) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
    rownames(evl15) <- c("Kwh","Gas")  
    
    evl15 
    
    
  },include.rownames=TRUE)
  
  
  
  ##########  TASK 1 
  
  set_type <- reactive({
    evl11 <- ""
    if(input$type != "All Buildings"){
      evl11  <-  subset(evl1,com == "Near West Side" & building_type == input$type)
    }else{
      evl11 <-  subset(evl1,com == "Near West Side") 
    }
     return(evl11)
  })
  
 
  

    
  
   #map 1 if else to group by GEOID and sum selected numeric column or identify building_type
  output$my_map <- renderLeaflet({
  
    evl11 <-set_type()
   
    
    if(input$property =="Electricity"){
      if(input$month == "1"){
        ho <- "kwh_1"
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_1 = sum(kwh_1)) 
      }
      if(input$month == "2"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_2 = sum(kwh_2)) 
        ho <- "kwh_2"
        }
      if(input$month == "3"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type),kwh_3 = sum(kwh_3)) 
        ho <- "kwh_3"
        }
      if(input$month == "4"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_4 = sum(kwh_4)) 
        ho <- "kwh_4"
        }
      if(input$month == "5"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_5 = sum(kwh_5)) 
        ho <- "kwh_5"
        }
      if(input$month == "6"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type),kwh_6 = sum(kwh_6)) 
        ho <- "kwh_6"
        }
      if(input$month == "7"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_7 = sum(kwh_7)) 
        ho <- "kwh_7"
        }
      if(input$month == "8"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_8 = sum(kwh_8)) 
        ho <- "kwh_8"
      }
      if(input$month == "9"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_9 = sum(kwh_9)) 
        ho <- "kwh_9"
      }
      if(input$month == "10"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_10 = sum(kwh_10)) 
        ho <- "kwh_10"
      }
      if(input$month == "11"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_11 = sum(kwh_11)) 
        ho <- "kwh_11"
        }
      if(input$month == "12"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,kwh_12 = sum(kwh_12)) 
        ho <- "kwh_12"
      }
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
      if(input$month == "2"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_2 = sum(the_2)) 
        ho  <- "the_2"
         }
      if(input$month == "3"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type),the_3 = sum(the_3)) 
        ho <- "the_3"
        }
      if(input$month == "4"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_4 = sum(the_4)) 
        ho <- "the_4"
        }
      if(input$month == "5"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_5 = sum(the_5)) 
        ho <- "the_5"
        }
      if(input$month == "6"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type),the_6 = sum(the_6)) 
        ho <- "the_6"
        }
      if(input$month == "7"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_7 = sum(the_7)) 
        ho <- "the_7"
        }
      if(input$month == "8"){
        evl11 <-  evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_8 = sum(the_8)) 
        ho <- "the_8"
        }
      if(input$month == "9"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_9 = sum(the_9)) 
        ho <- "the_9"
         }
      if(input$month == "10"){
        evl11 <-evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_10 = sum(the_10)) 
        ho <- "the_10"
        }
      if(input$month == "11"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_11 = sum(the_11)) 
        ho <- "the_11"
        }
      if(input$month == "12"){
        evl11 <- evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,the_12 = sum(the_12)) 
        ho <- "the_12"
         }
      if(input$month == "All"){ 
        evl11 <-  evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,total_the = sum(total_the)) 
        ho  <- "total_the"
      }
      
      
    }
    if(input$property == "Building Age"){
      evl11 <-  evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID),building_type = type_building(building_type) ,building_age = mean(building_age)) 
      ho <-"building_age"
    }
    if(input$property == "Building Type"){
      evl11 <-  evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), building_type = type_building(building_type) ) 
      ho <- "building_type"
    }
    if(input$property == "Building Height"){
      evl11 <-  evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), stories = mean(stories)) 
      ho  <- "stories"  
    }
    
    if(input$property == "Total Population"){
      evl11$total_population  <- ifelse(is.na(evl11$total_population),0,evl11$total_population ) 
      evl11 <-  evl11 %>%  group_by(GEOID) %>% summarise(community= my_random(com),Multiple = is_multiple(GEOID), total_population = sum(total_population)) 
      ho  <- "total_population"
    }
   #evl11 <-evl11 %>% subset(str_detect(community,"Near West Side"))
    #View(subset(evl11,str_detect(community,",")))
   ay1 <- inner_join(aa,evl11,"GEOID")  %>% select(-c("value","variable","NAME"))

   View(ay1)
   fcoy <-find_coy(1)
   fcor <- find_cor(1)
   ftitle <- find_title(ho)
   input$reset
  if(ho=="building_type"){ # ho assigned above served as what selected column 
    mapview(ay1,zcol=ho,col.regions =fcor,layer.name = ftitle  )@map
  }else {
   mapview(ay1,zcol=ho,col.regions =fcoy,layer.name = ftitle)@map
  }
    
    
   #a88e32
   #32a89a
   
    
  })
  
  output$sum_kwh_plot <- renderPlot({  #sum kwh plot 
    evl11 <-set_type()

    evl12 <- group_by(evl11) %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
 
    evl12<- by_list(evl12)
    evl13 <- data.frame(matrix(unlist(evl12), nrow=length(evl12), byrow=TRUE))
    names(evl13) <- c("total")
    
    sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    sum_month$total <-evl13$total 
  
    
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Kwh",title = paste("Sum Kwh 12 months for",input$type))  
    
  })
  
  
  output$sum_the_plot <- renderPlot({    #sum gas/thermal plot 
    


    evl11 <-set_type()
    
    #evl16  <- evl11 %>% group_by(GEOID) %>% summarise(com = my_random(com)) 
    #evl16 <- evl16 %>% subset(str_detect(com,"Near West Side"))
    #evl12 <- inner_join(evl11,evl16)
    #View(evl12)
    evl12 <- group_by(evl11) %>% summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
    
     
    
    evl12<- by_list(evl12)
    evl13 <- data.frame(matrix(unlist(evl12), nrow=length(evl12), byrow=TRUE))
    names(evl13) <- c("total")
    
    sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    sum_month$total <-evl13$total 
    
  
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Cubic meters",title = paste("Sum Gas 12 months for",input$type))
  })
  
  
  output$table1 <- renderTable({
    evl11  <- set_type()
    evl12 <- evl11 %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
    evl13 <- evl11 %>% summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
    
    evl12<- by_list(evl12)
    evl13<- by_list(evl13)
    evl14<-c(evl12,evl13)
    
    evl15 <- data.frame(matrix(unlist(evl14),nrow =2,byrow=TRUE))
    colnames(evl15) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
    rownames(evl15) <- c("Kwh","Gas")  
    print(evl15)
    
    print("s")
    evl15
    
  },include.rownames=TRUE)
  ############# TASK 2
  
  output$my_map21  <-renderLeaflet({ # map of screen1 
    ay3 <- task2_sti(input$property1,input$month1,input$type1,input$com1,1)
    ay21 <-ay3[[1]]
     
    View(ay21)
    fcoy1 <-find_coy2(input$color1)   # give color of numeric column 
    fcor1  <- find_cor(input$color1)   # give color of type/categeory column
    
    op1 <- ay3[[2]]
    ftitle1 <-  find_title(op1) # find right legend title
    
    input$reset1
    if(op1=="building_type"){
      mapview(ay3[[1]],zcol=op1,col.regions =fcor1,layer.name = ftitle1 )@map
    }else {
    
      mapview(ay3[[1]],zcol =op1,col.regions =fcoy1,layer.name = ftitle1 )@map
    }
    
  
    
    
  })
  
  
  
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
  
  
  
  
  output$sum_kwh_plot21  <- renderPlot({  # sum electricity  plot of screen1 
   
    if(input$type1 != "All Buildings"){
      evl21 <- subset(evl1, com == input$com1 & building_type == input$type1  )
    }
    else{
      evl21  <- subset(evl1, com == input$com1)
    }
 
  
    evl21  <- evl21 %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
  

     evl25<-by_list(evl21)
     evl25 <- data.frame(matrix(unlist(evl25), nrow=length(evl25), byrow=TRUE))
     
    names(evl25)  <- c("total")
   
    sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    sum_month$total <-evl25$total 
    
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Kwh",title = paste("Sum Kwh 12 months for ",input$type1,"in",input$com1))
    
    
    
    
    
    
  }) 
    
  output$sum_the_plot21 <- renderPlot({   # sum thermal  plot of screen1
    
    if(input$type1 != "All Buildings"){
      evl21 <- subset(evl1,com == input$com1 &  building_type == input$type1 )
    }
    else{
      evl21  <- subset(evl1,com == input$com1)
    }
    
    
    evl21  <- evl21   %>% summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
    
    
  

    evl25<-by_list(evl21)
    evl25 <- data.frame(matrix(unlist(evl25), nrow=length(evl25), byrow=TRUE))
    
    names(evl25)  <- c("total")
    #View(evl25)
    sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    sum_month$total <-evl25$total 
    
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Cubic meters",title = paste("Sum Gas 12 months for ",input$type1,"in",input$com1))
    
    
    
    
  })
  
  
  
  
  
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
    #View(evl271)
    
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Kwh",title = paste("Sum Kwh 12 months for ",input$type2,"in",input$com2))
    
    
    
    
    
    
  }) 
  
  
  
  output$sum_the_plot22 <- renderPlot({  # sum thermal  plot of screen2
    
    if(input$type2 != "All Buildings"){
      evl21 <- subset(evl1, com == input$com2 & building_type == input$type2 )
    }
    else{
      evl21  <- subset(evl1,com == input$com2)
    }
    
    
    evl21 <- evl21  %>%  summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
    
  

    evl25<-by_list(evl21)
    evl25 <- data.frame(matrix(unlist(evl25), nrow=length(evl25), byrow=TRUE))
    
    names(evl25)  <- c("total")
 
    sum_month <- data.frame(month = c(1,2,3,4,5,6,7,8,9,10,11,12))
    sum_month$total <-evl25$total 
    
    evl272 <-sum_month 
    #View(evl272)
    
    
    ggplot(sum_month,aes(x=month,y=total))+geom_line()+geom_point()+labs(x="Month",y= "Cubic meters",title = paste("Sum Gas 12 months for ",input$type2,"in",input$com2))
    
    
    
    
  })
  
  output$table21  <- renderTable({  # table  of screen1 
      
    if(input$type1 != "All Buildings"){
      evl21 <- subset(evl1, com == input$com1 & building_type == input$type1 )
    }
    else{
      evl21  <- subset(evl1,com == input$com1)
    }
    
    
    
    evl231  <- evl21  %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
    evl232  <- evl21  %>%  summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
    
    evl25 <- c(evl231,evl232)
     evl25  <- as.integer(evl25)
    
    evl15 <- data.frame(matrix(unlist(evl25),nrow =2,byrow=TRUE))
    colnames(evl15) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
    rownames(evl15) <- c("Kwh","Gas")  
    evl15
    
    
    
  },include.rownames=TRUE)
  
  
  output$table22 <- renderTable({  # table  of screen2
    
    if(input$type2 != "All Buildings"){
      evl21 <- subset(evl1, com == input$com2 & building_type == input$type2 )
    }
    else{
      evl21  <- subset(evl1,com == input$com2)
    }
    
    
    
    evl231  <- evl21  %>% summarise_at(vars("kwh_1","kwh_2","kwh_3","kwh_4","kwh_5","kwh_6","kwh_7","kwh_8","kwh_9","kwh_10","kwh_11","kwh_12"),sum) 
    evl232  <- evl21  %>%  summarise_at(vars("the_1","the_2","the_3","the_4","the_5","the_6","the_7","the_8","the_9","the_10","the_11","the_12"),sum) 
    
    evl25 <- c(evl231,evl232)
    evl25 <- as.integer(evl25)
    evl15 <- data.frame(matrix(unlist(evl25),nrow =2,byrow=TRUE))
    colnames(evl15) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
    rownames(evl15) <- c("Kwh","Gas")  
    evl15
    
    
  },include.rownames=TRUE)
  
  
  
  
}
shinyApp(ui,server)


