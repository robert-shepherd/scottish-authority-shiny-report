#Script 1/5: Data manipulation

#The script reads in data, tidies the data and merges the data
#before outputting to a .csv file to be read in by the Shiny app

library(tidyverse)

#Change below to where you would like to read in the files from
setwd("C:/Users/rshepherd/Desktop/MSc/R Programming/Project")


#Reading in data
geogr_lookup <- read.csv("_geogr_lookup.csv",stringsAsFactors=FALSE)
crime <- read.csv("crime.csv",skip=7,stringsAsFactors=FALSE)
dwellings_ct_a_c <- read.csv("dwellings-council-tax-a-c.csv",skip=7,stringsAsFactors=FALSE)
dwellings_ct_d_e <- read.csv("dwellings-council-tax-d-e.csv",skip=7,stringsAsFactors=FALSE)
dwellings_ct_f_h <- read.csv("dwellings-council-tax-f-h.csv",skip=7,stringsAsFactors=FALSE)
hospital_admissions <- read.csv("hospital-admissions.csv",skip=9,stringsAsFactors=FALSE)
house_sales_prices <- read.csv("house-sales-prices.csv",skip=6,stringsAsFactors=FALSE)
job_seekers_allowance <- read.csv("job-seekers-allowance.csv",skip=8,stringsAsFactors=FALSE)
municipal_waste <- read.csv("municipal-waste.csv",skip=7,stringsAsFactors=FALSE)
municipal_waste_2 <- read.csv("municipal-waste-2.csv",skip=7,stringsAsFactors=FALSE)
proximity_to_derelict_site <- read.csv("proximity-to-derelict-site.csv",skip=7,stringsAsFactors=FALSE)

#Creating a function to clean data
clean_data <- function(x) {
  
  #Rename first field
  names(x)[1] <- "refArea"
  
  #Remove url in first field
  x <- x %>%
    mutate(refArea = str_replace(refArea,"http://statistics.gov.scot/id/statistical-geography/",""))
  
  return(x)
}

#Cleaning reference field
crime <- clean_data(crime)
dwellings_ct_a_c <- clean_data(dwellings_ct_a_c)
dwellings_ct_d_e <- clean_data(dwellings_ct_d_e)
dwellings_ct_f_h <- clean_data(dwellings_ct_f_h)
hospital_admissions <- clean_data(hospital_admissions)
house_sales_prices <- clean_data(house_sales_prices)
job_seekers_allowance <- clean_data(job_seekers_allowance)
municipal_waste <- clean_data(municipal_waste)
municipal_waste_2 <- clean_data(municipal_waste_2)
proximity_to_derelict_site <- clean_data(proximity_to_derelict_site)

#Tidying data
tidy_df <- function(x){
  df_name <- deparse(substitute(x))
  gathered_df <- x %>%
    gather_("year",df_name,colnames(x)[-1:-2])
  return(gathered_df)
}

crime <- tidy_df(crime)
dwellings_ct_a_c <- tidy_df(dwellings_ct_a_c)
dwellings_ct_d_e <- tidy_df(dwellings_ct_d_e)
dwellings_ct_f_h <- tidy_df(dwellings_ct_f_h)
hospital_admissions <- tidy_df(hospital_admissions)
house_sales_prices <- tidy_df(house_sales_prices)
job_seekers_allowance <- tidy_df(job_seekers_allowance)
municipal_waste <- tidy_df(municipal_waste)
municipal_waste_2 <- tidy_df(municipal_waste_2)
proximity_to_derelict_site <- tidy_df(proximity_to_derelict_site)

#Cleaning year field
clean_year <- function(x) {
  x$year <- str_extract(x$year,"\\d+")
  return(x)
}

crime <- clean_year(crime)
dwellings_ct_a_c <- clean_year(dwellings_ct_a_c)
dwellings_ct_d_e <- clean_year(dwellings_ct_d_e)
dwellings_ct_f_h <- clean_year(dwellings_ct_f_h)
hospital_admissions <- clean_year(hospital_admissions)
house_sales_prices <- clean_year(house_sales_prices)
job_seekers_allowance <- clean_year(job_seekers_allowance)
municipal_waste <- clean_year(municipal_waste)
municipal_waste_2 <- clean_year(municipal_waste_2)
proximity_to_derelict_site <- clean_year(proximity_to_derelict_site)

#Aggregating job_seekers_allowance

job_seekers_allowance <- job_seekers_allowance %>%
  group_by(refArea,Reference.Area,year) %>%
  summarise(job_seekers_allowance = mean(job_seekers_allowance, na.rm = TRUE)) %>%
  filter(!is.na(job_seekers_allowance))

#Merging data

#Unioning all ref areas
union_data <- union(crime[1:3], dwellings_ct_a_c[1:3], dwellings_ct_d_e[1:3]
                    ,dwellings_ct_f_h[1:3], hospital_admissions[1:3], house_sales_prices[1:3]
                    ,job_seekers_allowance[1:3], municipal_waste[1:3], municipal_waste_2[1:3]
                    ,proximity_to_derelict_site[1:3])

#Joining data
combined_dataset <- union_data %>%
  left_join(crime,by=c("refArea","Reference.Area","year")) %>%
  left_join(dwellings_ct_a_c,by=c("refArea","Reference.Area","year")) %>%
  left_join(dwellings_ct_d_e,by=c("refArea","Reference.Area","year")) %>%
  left_join(dwellings_ct_f_h,by=c("refArea","Reference.Area","year")) %>%
  left_join(hospital_admissions,by=c("refArea","Reference.Area","year")) %>%
  left_join(house_sales_prices,by=c("refArea","Reference.Area","year")) %>%
  left_join(job_seekers_allowance,by=c("refArea","Reference.Area","year")) %>%
  left_join(municipal_waste,by=c("refArea","Reference.Area","year")) %>%
  left_join(municipal_waste_2,by=c("refArea","Reference.Area","year")) %>%
  left_join(proximity_to_derelict_site,by=c("refArea","Reference.Area","year"))

#Renaming first 2 fields
names(combined_dataset)[1:2] <- c("ref_area_id", "ref_area_desc")

#Changing year to an integer
combined_dataset$year <- as.integer(combined_dataset$year)

#Creating a field to differentiate between the geographical units
combined_dataset <- combined_dataset %>%
  mutate(ref_area_type = ifelse(str_detect(ref_area_id,"^S1200"),"Local Authority"
                                ,ifelse(str_detect(ref_area_id,"^S0200"),"Intermediate Geography"
                                        ,"Data Zone")))

#Reshaping geogr_lookup file
datazone <- unique(geogr_lookup[c(1,3)])
interzone <- unique(geogr_lookup[c(2,3)])
council <- unique(geogr_lookup[c(3,3)])

field_names <- c("ref_area_id","local_authority_id")

names(datazone) <- field_names
names(interzone) <- field_names
names(council) <- field_names

geogr_lookup_reshaped <- rbind(datazone,interzone,council)

#Adding in local authority
combined_dataset <- left_join(combined_dataset
                              ,geogr_lookup_reshaped
                              ,by="ref_area_id")

#Creating a local authority lookup
local_authority_lookup <- combined_dataset %>%
  filter(ref_area_type == "Local Authority") %>%
  select(ref_area_id,ref_area_desc) %>%
  distinct

names(local_authority_lookup) <- c("local_authority_id","local_authority")

#Adding in missing authorities
local_authority_lookup <- rbind(local_authority_lookup
                                ,c("S12000015","Fife (Archived)")
                                ,c("S12000024","Perth and Kinross (Archived)"))

#Creating final dataset
scottish_neighbourhoods <- combined_dataset %>%
  left_join(local_authority_lookup,by="local_authority_id")

#Writing out csv
write.csv(scottish_neighbourhoods,"scottish_neighbourhoods.csv",row.names = FALSE)