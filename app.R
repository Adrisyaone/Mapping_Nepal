#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggspatial)
library(tidyverse)
library(sf)




# load data set

ward<-sf::st_read("Dataset/hermes_NPL_wgs_4.shp")
municipality<-sf::st_read("Dataset/hermes_NPL_new_wgs(1)/hermes_NPL_new_wgs_3.shp")
municipality$MUNICIPALITY <- municipality$LOCAL
municipality$PROVINCE <- ifelse(municipality$PR_NAME=="Province No 1", "Koshi",
                            ifelse(municipality$PR_NAME=="Province No 2", "Madesh",
                                   ifelse(municipality$PR_NAME=="Bagmati Pradesh", "Bagmati",
                                          ifelse(municipality$PR_NAME=="Gandaki Pradesh", "Gandaki",
                                                 ifelse(municipality$PR_NAME=="Province No 5", "Lumbini",
                                                        ifelse(municipality$PR_NAME=="Karnali Pradesh", "Karnali",
                                                               ifelse(municipality$PR_NAME=="Sudurpashchim Pradesh", "Sudurpashchim",NA)))))))

district<-sf::st_read("Dataset/hermes_NPL_new_wgs(1)/hermes_NPL_new_wgs_2.shp")

province <-sf::st_read("Dataset/hermes_NPL_new_wgs(1)/hermes_NPL_new_wgs_1.shp")

province$PROVINCE <- ifelse(province$PR_NAME=="Province No 1", "Koshi",
                            ifelse(province$PR_NAME=="Province No 2", "Madesh",
                                   ifelse(province$PR_NAME=="Bagmati Pradesh", "Bagmati",
                                          ifelse(province$PR_NAME=="Gandaki Pradesh", "Gandaki",
                                                 ifelse(province$PR_NAME=="Province No 5", "Lumbini",
                                                        ifelse(province$PR_NAME=="Karnali Pradesh", "Karnali",
                                                               ifelse(province$PR_NAME=="Sudurpashchim Pradesh", "Sudurpashchim",NA)))))))

national<-sf::st_read("Dataset/hermes_NPL_new_wgs(1)/hermes_NPL_new_wgs_0.shp")
national$NATIONAL <- "Nepal"

# list of municipalities, district and province (cascading data)
dt <- municipality %>% 
  select(PROVINCE, DISTRICT, LOCAL, TYPE) %>% 
  mutate(geometry<-NULL)


# load ui.R, server.R and functions.R
source("ui.R")
source("server.R")
source("functions.R")



# Run the application 
shinyApp(ui = ui, server = server)


