########## Codes for SoCal Warehouse Project #########
sessionInfo()

#### Load required packages ####

install.packages("pacman")
require(pacman)
p_load(dplyr, tidyverse, tidyr, readxl, ggplot2, lmerTest)

#### 1. CLEANING: Process Costar Warehousing Data ####

# Load original warehouse dataset

costar_all <-list.files(path = "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\CoStar\\Raw",  # Identify all CSV files
                        pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv) %>%                              # Store all files in list
  bind_rows                                       # Combine data sets into one data set 
costar_all   

write.csv(costar_all, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\CoStar\\costar_all.csv")

# Subset by domain

costar_CA <- costar_all %>% 
  filter(Latitude >= 33.299117 & Latitude <= 34.942896 &
           Longitude >= -119.09145 & Longitude <= -114.74009)

write.csv(costar_CA, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\CoStar\\domain_subset.csv")

# Select variables of interest

costar_CA_subset <- costar_CA %>%
  select(Zip, RBA, Number.Of.Loading.Docks, Number.Of.Parking.Spaces, County.Name,
         Latitude, Longitude, Year.Built) %>%
  #  mutate_all(~ifelse(is.na(.), 0, .)) %>%
  mutate(Zip = substr(Zip,1,5),    # substring zipcode to only 5 digit format
         Zip = as.numeric(Zip),
         RBA = as.numeric(RBA),
         Number.Of.Loading.Docks = as.numeric(Number.Of.Loading.Docks),
         Number.Of.Parking.Spaces = as.numeric(Number.Of.Parking.Spaces),
         Year.Built = as.numeric(Year.Built)) 

sapply(costar_CA_subset, class) # Check if all variables are numeric, if not, mutate above


write.csv(costar_CA_subset, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_zipcode_1900s_2020.csv")


# Convert Year Column
# Change all years before 2000 to 2000, indicating that those warehouses were already built in 2000

costar_CA_subset$year <- ifelse(costar_CA_subset$Year.Built <= 2000, 2000, costar_CA_subset$Year.Built)

unique(costar_CA_subset$year)


#Costar_2000
costar_2000 <- costar_CA_subset %>%
  filter(year == 2000)
costar_2000_seq <- costar_2000[rep(seq_len(nrow(costar_2000)), each = 20), ]
costar_2000_seq$yearseq <- rep(seq(2000, 2019), length.out = nrow(costar_2000_seq))

costar_2000_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2000_seq.csv")

#Costar_2001
costar_2001 <- costar_CA_subset %>%
  filter(year == 2001)
costar_2001_seq <- costar_2001[rep(seq_len(nrow(costar_2001)), each = 19), ]
costar_2001_seq$yearseq <- rep(seq(2001, 2019), length.out = nrow(costar_2001_seq))

costar_2001_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2001_seq.csv")

#Costar_2002
costar_2002 <- costar_CA_subset %>%
  filter(year == 2002)
costar_2002_seq <- costar_2002[rep(seq_len(nrow(costar_2002)), each = 18), ]
costar_2002_seq$yearseq <- rep(seq(2002, 2019), length.out = nrow(costar_2002_seq))

costar_2002_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2002_seq.csv")

#Costar_2003
costar_2003 <- costar_CA_subset %>%
  filter(year == 2003)
costar_2003_seq <- costar_2003[rep(seq_len(nrow(costar_2003)), each = 17), ]
costar_2003_seq$yearseq <- rep(seq(2003, 2019), length.out = nrow(costar_2003_seq))

costar_2003_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2003_seq.csv")

#Costar_2004
costar_2004 <- costar_CA_subset %>%
  filter(year == 2004)
costar_2004_seq <- costar_2004[rep(seq_len(nrow(costar_2004)), each = 16), ]
costar_2004_seq$yearseq <- rep(seq(2004, 2019), length.out = nrow(costar_2004_seq))

costar_2004_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2004_seq.csv")

#Costar_2005
costar_2005 <- costar_CA_subset %>%
  filter(year == 2005)
costar_2005_seq <- costar_2005[rep(seq_len(nrow(costar_2005)), each = 15), ]
costar_2005_seq$yearseq <- rep(seq(2005, 2019), length.out = nrow(costar_2005_seq))

costar_2005_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2005_seq.csv")

#Costar_2006
costar_2006 <- costar_CA_subset %>%
  filter(year == 2006)
costar_2006_seq <- costar_2006[rep(seq_len(nrow(costar_2006)), each = 14), ]
costar_2006_seq$yearseq <- rep(seq(2006, 2019), length.out = nrow(costar_2006_seq))

costar_2006_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2006_seq.csv")

#Costar_2007
costar_2007 <- costar_CA_subset %>%
  filter(year == 2007)
costar_2007_seq <- costar_2007[rep(seq_len(nrow(costar_2007)), each = 13), ]
costar_2007_seq$yearseq <- rep(seq(2007, 2019), length.out = nrow(costar_2007_seq))

costar_2007_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2007_seq.csv")

#Costar_2008
costar_2008 <- costar_CA_subset %>%
  filter(year == 2008)
costar_2008_seq <- costar_2008[rep(seq_len(nrow(costar_2008)), each = 12), ]
costar_2008_seq$yearseq <- rep(seq(2008, 2019), length.out = nrow(costar_2008_seq))

costar_2008_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2008_seq.csv")

#Costar_2009
costar_2009 <- costar_CA_subset %>%
  filter(year == 2009)
costar_2009_seq <- costar_2009[rep(seq_len(nrow(costar_2009)), each = 11), ]
costar_2009_seq$yearseq <- rep(seq(2009, 2019), length.out = nrow(costar_2009_seq))

costar_2009_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2009_seq.csv")


#Costar_2010
costar_2010 <- costar_CA_subset %>%
  filter(year == 2010)
costar_2010_seq <- costar_2010[rep(seq_len(nrow(costar_2010)), each = 10), ]
costar_2010_seq$yearseq <- rep(seq(2010, 2019), length.out = nrow(costar_2010_seq))

costar_2010_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2010_seq.csv")

#Costar_2011
costar_2011 <- costar_CA_subset %>%
  filter(year == 2011)
costar_2011_seq <- costar_2011[rep(seq_len(nrow(costar_2011)), each = 9), ]
costar_2011_seq$yearseq <- rep(seq(2011, 2019), length.out = nrow(costar_2011_seq))

costar_2011_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2011_seq.csv")

#Costar_2012
costar_2012 <- costar_CA_subset %>%
  filter(year == 2012)
costar_2012_seq <- costar_2012[rep(seq_len(nrow(costar_2012)), each = 8), ]
costar_2012_seq$yearseq <- rep(seq(2012, 2019), length.out = nrow(costar_2012_seq))

costar_2012_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2012_seq.csv")

#Costar_2013
costar_2013 <- costar_CA_subset %>%
  filter(year == 2013)
costar_2013_seq <- costar_2013[rep(seq_len(nrow(costar_2013)), each = 7), ]
costar_2013_seq$yearseq <- rep(seq(2013, 2019), length.out = nrow(costar_2013_seq))

costar_2013_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2013_seq.csv")

#Costar_2014
costar_2014 <- costar_CA_subset %>%
  filter(year == 2014)
costar_2014_seq <- costar_2014[rep(seq_len(nrow(costar_2014)), each = 6), ]
costar_2014_seq$yearseq <- rep(seq(2014, 2019), length.out = nrow(costar_2014_seq))

costar_2014_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2014_seq.csv")

#Costar_2015
costar_2015 <- costar_CA_subset %>%
  filter(year == 2015)
costar_2015_seq <- costar_2015[rep(seq_len(nrow(costar_2015)), each = 5), ]
costar_2015_seq$yearseq <- rep(seq(2015, 2019), length.out = nrow(costar_2015_seq))

costar_2015_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2005_seq.csv")

#Costar_2016
costar_2016 <- costar_CA_subset %>%
  filter(year == 2016)
costar_2016_seq <- costar_2016[rep(seq_len(nrow(costar_2016)), each = 4), ]
costar_2016_seq$yearseq <- rep(seq(2016, 2019), length.out = nrow(costar_2016_seq))

costar_2016_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2016_seq.csv")

#Costar_2017
costar_2017 <- costar_CA_subset %>%
  filter(year == 2017)
costar_2017_seq <- costar_2017[rep(seq_len(nrow(costar_2017)), each = 3), ]
costar_2017_seq$yearseq <- rep(seq(2017, 2019), length.out = nrow(costar_2017_seq))

costar_2017_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2017_seq.csv")

#Costar_2018
costar_2018 <- costar_CA_subset %>%
  filter(year == 2018)
costar_2018_seq <- costar_2018[rep(seq_len(nrow(costar_2018)), each = 2), ]
costar_2018_seq$yearseq <- rep(seq(2018, 2019), length.out = nrow(costar_2018_seq))

costar_2018_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2018_seq.csv")

#Costar_2019
costar_2019 <- costar_CA_subset %>%
  filter(year == 2019)
costar_2019_seq <- costar_2019[rep(seq_len(nrow(costar_2019)), each = 1), ]
costar_2019_seq$yearseq <- rep(seq(2019, 2019), length.out = nrow(costar_2019_seq))

costar_2019_seq %>%  write.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2019_seq.csv")

# merge year seq data (USE FOR REGRESSION LATER)

costar_2000_2019_seq <- list.files(path = "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019",  # Identify all CSV files
                                   pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read.csv) %>%       # Store all files in list
  bind_rows                  # Combine data sets into one data set 
costar_2000_2019_seq = subset(costar_2000_2019_seq, select = -c(X) )


write.csv(costar_2000_2019_seq, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2000_2019_seq.csv")

# Obtain warehouses between 2000-2019 only, not including before 2000

costar_2000_2019_only <- costar_CA_subset %>% 
  filter(between(Year.Built,2000,2019)) %>%
  mutate(Zip = as.numeric(Zip),
         Zip = substr(Zip,1,5))

write.csv(costar_2000_2019_only,"C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_2000_2019_only.csv" )

# Warehouse built before 2000 only

costar_1900_1999_only <- costar_CA_subset %>% 
  filter(between(Year.Built,1900,1999)) %>%
  mutate(Zip = as.numeric(Zip),
         Zip = substr(Zip,1,5))

write.csv(costar_1900_1999_only,"C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_1900_1999_only.csv" )
#### 2. CLEANING: Process PM2.5 Data (Zipcode Level) ####

# Import raw data from 2000-2016

PM25_00_16 <- read_rds("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\PM25_Data\\zip_year_summary_contiguous_criteria_pollutants_0018.rds") %>%
  rename(Zip = "ZIP") %>%
  select(Zip, year, pm25)%>%     #select variables
  filter(Zip >= 90001 & Zip <= 93560) %>%    #Subset to study domain
  na.omit()

# Obtain raw data from 2017-2018

pm_2017 <- read_rds("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\PM25_Data\\2017_PM25_ZIPCODE.rds") %>%
  rename(Zip = "ZIPCODE",
         pm25 = "PM25") %>%
  mutate(year = 2017) %>%      #Add year column and indicate 2017 for all rows
  filter(Zip >= 90001 & Zip <= 93560) %>%    #Subset to study domain
  na.omit()


pm_2018 <- read_rds("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\PM25_Data\\2018_PM25_ZIPCODE.rds") %>%
  rename(Zip = "ZIPCODE",
         pm25 = "PM25") %>%
  mutate(year = 2018) %>%      #Add year column and indicate 2017 for all rows
  filter(Zip >= 90001 & Zip <= 93560) %>%    #Subset to study domain
  na.omit()

#Merge 2017 amd 2018 data to the large dataset
PM25_allyears <- rbind(PM25_00_16,pm_2017,pm_2018)


write.csv(PM25_allyears, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\PM25_Data\\PM25_CA_2000_2018.csv")
#### -------VALIDATION: Satellite/Ground Comparison for PM2.5/EC ####

## PM25

  satellite <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\PM25_Data\\CA_PM_1km_00_16.csv") %>%
    group_by(Lon, Lat, Year) %>%
    summarize(PM25_satellite = mean(PM25))
  
  
  EPA_ground <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\PM25_Data\\Calibrated_AQS_with_MAIAC_ID.csv") %>%
    mutate(Date = ymd(Date),
           Year = year(Date)) %>%
    filter(lon >= -119.38 & lon <= -114.60 &
             lat <= 35.15 & lat >= 32.70) %>%
    group_by(lon, lat, Year) %>%
    summarize(PM25_ground = mean(PM25)) %>%
    filter(Year <= 2018) %>%
    rename(Lon = "lon", Lat = "lat")
  
  
  
  lm_data <- merge(satellite,EPA_ground,  by = c("Lon","Lat","Year"))
  
  lm_test <- lm(PM25_ground ~ PM25_satellite, lm_data)
  
  #cor(lm_data$PM25_ground, lm_data$PM25_satellite)^2
  
  summary(lm_test)  # R square = 0.82, good performance


## EC

  satellite_EC <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\EC_Data\\EC_1km_2000_2019\\EC_1km_avg_by_year.csv") %>%
    rename(Lon = "lon", Lat = "lat", Year = "year")
  
  ground_EC <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\EC_Data\\Ground\\CSN_IMPROVE_EC_CA_2002_2020.csv") %>%
    select(Lon = Longitude, Lat = Latitude, Date.Local, EC_ground = Arithmetic.Mean) %>%
    filter(Lon >= -119.38 & Lon <= -114.60 &
             Lat <= 35.15 & Lat >= 32.70) %>%
    mutate(Date = ymd(Date.Local),
           Year = year(Date)) %>%
    group_by(Lon, Lat, Year) %>%
    summarize(EC_ground = mean(EC_ground))
  
  EC_coordinates = distinct(ground_EC, Lon, Lat)
  EC_coordinates$ID = c(1:7)
  
  satellite_coordinates = distinct(satellite_EC,Lat,Lon)
  satellite_coordinates$ID = paste0("satellite",c(1:nrow(satellite_coordinates)))
  
  EC_coordinates$satelliteID = NA
  for(i in 1:nrow(EC_coordinates))
  {
    lat_temp = EC_coordinates$Lat[i]
    lon_temp = EC_coordinates$Lon[i]
    
    dist = (lat_temp - satellite_coordinates$Lat)^2 + (lon_temp - satellite_coordinates$Lon)^2
    
    EC_coordinates$satelliteID[i] = satellite_coordinates$ID[which(dist == min(dist))]  
    
  }
  
  EC_coordinates = merge(EC_coordinates,satellite_coordinates, by.x = "satelliteID",by.y = "ID")
  EC_coordinates = EC_coordinates %>% subset(.,select = -c(satelliteID,ID))
  colnames(EC_coordinates) = c("Lon","Lat","Lat_satellite","Lon_satellite")
  
  
  lm_data_ec <- ground_EC %>%
    merge(EC_coordinates, by = c("Lon", "Lat")) %>%
    merge(satellite_EC, by.x = c("Lon_satellite", "Lat_satellite", "Year"),
          by.y = c("Lon","Lat","Year")) %>%
    rename(EC_satellite = EC)
  
  cor(lm_data_ec$EC_ground, lm_data_ec$EC_satellite)^2

#### 3. CLEANING: Process EC Data (Zipcode Level) ####

read_ec_files <- function(start_year, end_year, directory) {
  # Create an empty list to store the data frames
  ec_list <- list()
  
  # Loop through the years
  for (year in start_year:end_year) {
    # Read the CSV file for the current year
    file_path <- paste0(directory, "/annual_PM_components_by_zipcode_US_", year, ".csv")
    ec_data <- read.csv(file_path)
    
    # Add a 'year' column
    ec_data$year <- year
    
    # Store the data frame in the list
    ec_list[[year - start_year + 1]] <- ec_data
  }
  
  # Bind all data frames in the list into a single data frame
  ec_all <- do.call(rbind, ec_list)
  
  return(ec_all)
}

# Call the function to read all files
ec_allyears <- read_ec_files(2000, 2019, "C:/Users/byang77/Desktop/SoCal Warehouse/EC_Data/US_PM_comp_2000_2019_zipcode")

# Select variables and subset to study domain

ec_allyears_subset <- ec_allyears %>%
  select(ZIP, year, ec) %>%
  rename(Zip = "ZIP") %>%
  filter(Zip >= 90001 & Zip <= 93560) %>%
  na.omit()

write.csv(ec_allyears_subset,"C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\EC_Data\\EC_CA_2000_2020.csv")


#### 4. CLEANING: Process Demographic Data (Zipcode Level) ####

# Import data for CA demographic variables and select variables of interest

demographic <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Demographic\\demographic_CA_2000_2020.csv")


demographic_subset <- demographic %>%
  select(ZIP, year, population,pct_white, pct_poverty, 
         pct_renting, med_household_income, 
         pct_age_over_65, pct_edu_under_highschool)

write.csv(demographic_subset, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Demographic\\demographic_subset.csv")


#### 5. ANALYSIS: Costar Descriptive Analysis (Table 1, Table 2) ####

# Import all warehouse data 1900 - 2020

costar_1900_2022 <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_zipcode_1900s_2020.csv")

# Annual summary of warehouse capacity variables

# Before 2000

RBA_LD_PS_1900_1999 <- costar_1900_2022 %>%
  filter(Year.Built <= 1999) %>%
  summarize(total_warehouse = n(),
            RBA_med = median(RBA), RBA_IQR = IQR(RBA),
            LD_med = median(Number.Of.Loading.Docks, na.rm = T), LD_IQR = IQR(Number.Of.Loading.Docks, na.rm = T),
            PS_med = median(Number.Of.Parking.Spaces, na.rm = T), PS_IQR = IQR(Number.Of.Parking.Spaces, na.rm = T)) %>%
  mutate(Year.Built = c("<2000")) %>%
  relocate(Year.Built, .before = 1)

RBA_1900_1999 <- costar_1900_2022 %>%
  filter(Year.Built <= 1999) %>%
  summarize(RBA_increase = sum(RBA)) %>%
  mutate(Year.Built = c("<2000")) %>%
  relocate(Year.Built, .before = 1)

# 2000-2019 (Study Period)

RBA_LD_PS_annual <- costar_1900_2022 %>%
  filter(Year.Built <= 2019 & Year.Built >= 2000) %>%
  group_by(Year.Built) %>%
  summarize(total_warehouse = n(),
            RBA_med = median(RBA), RBA_IQR = IQR(RBA),
            LD_med = median(Number.Of.Loading.Docks, na.rm = T), LD_IQR = IQR(Number.Of.Loading.Docks, na.rm = T),
            PS_med = median(Number.Of.Parking.Spaces, na.rm = T), PS_IQR = IQR(Number.Of.Parking.Spaces, na.rm = T))

annual_RBA_2000_2019 <- costar_1900_2022 %>%
  filter(Year.Built <= 2019 & Year.Built >= 2000) %>%
  group_by(Year.Built) %>% 
  summarize(RBA_increase = sum(RBA))

# All years (1900-2019)

RBA_LD_PS_all <- costar_1900_2022 %>%
  filter(Year.Built <= 2019) %>%
  summarize(total_warehouse = n(),
            RBA_med = median(RBA), RBA_IQR = IQR(RBA),
            LD_med = median(Number.Of.Loading.Docks, na.rm = T), LD_IQR = IQR(Number.Of.Loading.Docks, na.rm = T),
            PS_med = median(Number.Of.Parking.Spaces, na.rm = T), PS_IQR = IQR(Number.Of.Parking.Spaces, na.rm = T)) %>%
  mutate(Year.Built = c("All")) %>%
  relocate(Year.Built, .before = 1)

RBA_all <- costar_1900_2022 %>%
  filter(Year.Built <= 2019) %>%
  summarize(RBA_increase = sum(RBA)) %>%
  mutate(Year.Built = c("All")) %>%
  relocate(Year.Built, .before = 1)

# Bind data for both period to show all data necessary for Table 1/Table 2

Costar_descriptive_all <- rbind(RBA_LD_PS_1900_1999, RBA_LD_PS_annual, RBA_LD_PS_all)

Costar_descriptive_RBA <- rbind(RBA_1900_1999, annual_RBA_2000_2019, RBA_all)
Costar_descriptive_all_1 <- merge(Costar_descriptive_all, Costar_descriptive_RBA, by.x = "Year.Built") 
Costar_descriptive_all_final <- Costar_descriptive_all_1 %>% 
  mutate(total_warehouse_pct = (total_warehouse / 10937)*100, 
         total_RBA_pct = (RBA_increase / 716720121)*100) %>%
  select(Year.Built, total_warehouse, total_warehouse_pct,RBA_increase,
         total_RBA_pct, RBA_med, RBA_IQR, LD_med, LD_IQR, PS_med, PS_IQR)

write.csv(Costar_descriptive_all_final, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_annual_descriptive.csv")

# Obtain LA, Orange, San Bernandino, and Riverside to highlight inland empire increase

costar_counties <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_2000_2019_only.csv") %>%
  rename(County = County.Name) %>%
  group_by(Year.Built, County) %>%
  summarize("Number_Warehouse_Built" = n(),
            "Sum_of_RBA" = sum(RBA),
            "Average_RBA" = mean(RBA))
write.csv(costar_counties, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_four_counties.csv")

# Graph warehouse by counties

figure_county <- costar_counties %>% 
  ggplot(aes(x = as.numeric(Year.Built), y = Number_Warehouse_Built, color = County)) + 
  geom_line(linetype = "solid", linewidth = 1 )  +
  geom_point(size = 1.4) +
  xlab("") +
  ylab("Number of Warehouses (n)") + 
  ggtitle(label = "Temporal Trends in Warehouse Characteristics by Counties") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 1), expand = c(0.02, 0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7, angle = 45,vjust = 1, hjust=1),
        legend.key.size = unit(0.5, "cm"), 
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) + 
  scale_color_manual(values = c("orange3","steelblue4", "springgreen4", "darkred", "slateblue4"))

figure_county

figure_county_2 <- costar_counties %>% 
  ggplot(aes(x = as.numeric(Year.Built), y = Sum_of_RBA, color = County)) + 
  geom_line(linetype = "solid", linewidth = 1 )  +
  geom_point(size = 1.4) +
  xlab("") +
  ylab("Sum of RBA (sq ft)") + 
  #  ggtitle(label = "Temporal Trends in Warehouse Characteristics by Counties") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 1), expand = c(0.02, 0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7, angle = 45,vjust = 1, hjust=1),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) + 
  scale_color_manual(values = c("orange3","steelblue4", "springgreen4", "darkred", "slateblue4"))

figure_county_2

figure_county_3 <- costar_counties %>% 
  ggplot(aes(x = as.numeric(Year.Built), y = Average_RBA, color = County)) + 
  geom_line(linetype = "solid", linewidth = 1 )  +
  geom_point(size = 1.4) +
  xlab("Year") +
  ylab("Average RBA (sq ft)") + 
  #  ggtitle(label = "Temporal Trends in Warehouse Characteristics by Counties") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 1), expand = c(0.02, 0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7, angle = 45,vjust = 1, hjust=1),
        legend.key.size = unit(0.5, "cm"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) + 
  scale_color_manual(values = c("orange3","steelblue4", "springgreen4", "darkred", "slateblue4"))

figure_county_3


require(cowplot)  

combined_counties <- plot_grid(figure_county, figure_county_2, figure_county_3, ncol = 1, align = 'v', 
                               rel_widths = c(1, 1, 1))
combined_counties

ggsave2("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Figures\\Temporal_trends_warehouse_counties.tiff", dpi = 300)  


#### 6. ANALYSIS: Graphing Costar Descriptive Data (Figure 2) ####

require(ggplot2)
# annual increase of warehouse number 

Costar_descriptive_all <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\CoStar\\costar_annual_descriptive.csv")
figure_2a <- Costar_descriptive_all %>% 
  filter(Year.Built <= 2019 & Year.Built >= 2000) %>%
  ggplot(aes(x = as.numeric(Year.Built), y = total_warehouse, group = 1)) + 
  stat_smooth(method="loess", linetype = "dashed", color = "orange", linewidth = 0.75,se=F) +
  geom_line(linetype = "solid", color = "steelblue",linewidth = 0.8 )  +
  geom_point(size = 1.4) +
  xlab("Year of Construction") +
  ylab("Number of Warehouses (n)") + 
  ggtitle(label = "Annual Increase in Warehouse Number") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1, 0)) +
  theme_bw() +
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 8)) 
figure_2a              

# annual increase of RBA

# Load original data and calculate annual increase of RBA

costar_1900_2022 <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\CoStar\\costar_zipcode_1900s_2020.csv") 

annual_RBA_2000_2019 <- costar_1900_2022 %>%
  filter(Year.Built <= 2019 & Year.Built >= 2000) %>%
  group_by(Year.Built) %>% 
  summarize(RBA_increase = sum(RBA))


# save the dataset to archive

write.csv(annual_RBA_2000_2019, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\annual_RBA_figure_2b.csv" )


# Generate figure

figure_2b <- annual_RBA_2000_2019 %>%
  ggplot(aes(x = as.numeric(Year.Built), y = RBA_increase, group =1)) +
  stat_smooth(method="loess", linetype = "dashed", color = "orange", linewidth = 0.75,se=F) +
  geom_line(color = "steelblue", linetype = "solid", linewidth = 0.8) +
  geom_point(size = 1.4, color = "black") +
  xlab("Year") +
  ylab("Rentable Building Area (10⁷ ft²)")+
  ggtitle(label = "Annual Increase in Rentable Building Area") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1, 0)) +
  scale_y_continuous(labels = comma_format(scale = 1e-7), breaks = seq(2.0e6, 2.1e7, by = 2e6)) +
  theme_bw()+
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 8))


figure_2b

# cumulative increase of warehouse number

# Load costar data from 1900-2022 and calculate cumulative number increase during 2000-2019

costar_1900_2022 <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\CoStar\\costar_zipcode_1900s_2020.csv") 

cumulative_number <- costar_1900_2022 %>%
  filter(Year.Built <= 2019)%>%
  group_by(Year.Built) %>%
  arrange(Year.Built) %>%
  summarize(total_number = n())%>%
  summarize(Year = Year.Built, cumu_number = cumsum(total_number))%>%
  filter(Year >= 2000)

# Save dataset in the archive

write.csv(cumulative_number, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\CoStar\\cumulative_RBA_figure_2c.csv" )

# Generate figures
require(scales)

figure_2c <- cumulative_number %>%
  ggplot(aes(x = as.numeric(Year), y = cumu_number, group =1)) +
  stat_smooth(method="lm", linetype = "dashed", color = "orange", linewidth = 0.75,se=F) +
  geom_line(color = "steelblue", linetype = "solid", linewidth = 0.8) +
  geom_point(size = 1.4, color = "black") +
  xlab("Year") +
  ylab("Number of Warehouse (n)")+
  ggtitle(label = "Cumulative Increase in Warehouse Number") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1, 0)) +
  scale_y_continuous(breaks = seq(9300, 11300, by = 500)) +
  theme_bw()+
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 8)) +      
  annotate("text",x=2003,y=11000,
           label=(paste0("slope==",coef(lm(cumulative_number$cumu_number~cumulative_number$Year))[2])),parse=TRUE,size =3)

figure_2c

# cumulative increase of RBA

# Load yearseq data which counts all previously built RBA for each year
costar_seq <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\CoStar\\Costar_Yearseq_2000_2019\\costar_2000_2019_seq.csv")

# create dataset showing annual cumulative data for RBA
cumulative_RBA <- costar_seq %>%
  group_by(yearseq) %>%
  summarize(total_RBA = sum(RBA)) %>%
  select(Year = "yearseq", total_RBA)

# save the dataset to archive

write.csv(cumulative_RBA, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\CoStar\\cumulative_RBA_figure_2d.csv" )

# Generate figure
require(scales)

figure_2d <- cumulative_RBA %>%
  ggplot(aes(x = as.numeric(Year), y = total_RBA, group =1)) +
  stat_smooth(method="lm", linetype = "dashed", color = "orange", linewidth = 0.75,se=F) +
  geom_line(color = "steelblue", linetype = "solid", linewidth = 0.8) +
  geom_point(size = 1.4, color = "black") +
  xlab("Year") +
  ylab("Cumulative Rentable Building Area (10⁹ ft²)")+
  ggtitle(label = "Cumulative Increase in Rentable Building Area") +
  scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1, 0)) +
  scale_y_continuous(labels = comma_format(scale = 1e-9), breaks = seq(1.9e9, 3.0e9, by = 1e8)) +
  theme_bw()+
  theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        axis.text = element_text(size = 8) ) +
  annotate("text",x=2003,y=2.75e9,
           label=(paste0("slope==",coef(lm(cumulative_RBA$total_RBA~cumulative_RBA$Year))[2])),parse=TRUE,size =3)

figure_2d

# Join all plots

install.packages("cowplot")
require(cowplot)  

combined_graph <- plot_grid(figure_2a, figure_2b,figure_2c, figure_2d)
combined_graph

#ggsave2("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Figures\\Combined_Figure_2.tiff", dpi = 300)  




#### 7. CLEANING: Select Control Region: Zipcodes from Domain with No warehouses ####

CA_demographic <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Demographic\\demographic_subset.csv") %>%
  select(-X)

# Import all zipcodes within domain and subset the CA dataset

Domain_zip <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\GIS\\zipcodes_within_domain.csv") %>%
  select(ZIP = "ZIP_cha")
Domain_demo <- merge(CA_demographic, Domain_zip, by.x = c("ZIP"))

# Import merged data for Costar and demographic

Costar_demo <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Merged_Data\\costar_demographic_merged.csv")


costar_zip_pop <- Costar_demo %>%
  select(ZIP = "Zip", year = "yearseq", pop_costar = "population") %>%
  group_by(ZIP,year) %>%
  summarize(ZIP = mean(ZIP), year = mean(year), pop_costar = mean(pop_costar))
domain_zip_pop <- Domain_demo %>%
  select(ZIP, year, pop_domain = "population") %>%
  filter(year <= 2019)

zip_pop_compare <- merge(domain_zip_pop, costar_zip_pop, by = c("ZIP", "year"), all.x = TRUE)

mean_pop <- zip_pop_compare %>%
  group_by(ZIP) %>%
  summarize(mean_domain = mean(pop_domain), mean_costar = mean(pop_costar))


# Filter out rows with NA values in the mean_costar column
mean_pop_filtered <- mean_pop[is.na(mean_pop$mean_costar), ]

# Calculate the median population of the zip codes in the filtered mean_pop dataset
median_population <- median(mean_pop_filtered$mean_costar)


# Define a threshold for population similarity
similarity_threshold <- 0.2  

# Calculate the population range based on the threshold around the median population
min_population <- median_population * (1 - similarity_threshold)
max_population <- median_population * (1 + similarity_threshold)

#Obtain zipcode in the study domain with no warehouses but have similar population than warehouse regions
similar_zipcodes <- mean_pop_filtered[mean_pop_filtered$mean_domain >= min_population & 
                                        mean_pop_filtered$mean_domain <= max_population, ]
# Create control dataset by merging the similar zipcode with full demographic data

Control_zip <- similar_zipcodes %>%
  select(ZIP)
Control_demo <- merge(CA_demographic, Control_zip, by.x = c("ZIP"))

# save to archive

#write.csv(Control_zip,"C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\zipcode_no_warehouse_control.csv")
#write.csv(Control_demo, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Demographic\\demographic_control.csv" )


#### 8. ANALYSIS: Comparative analysis for demographic variables (Table 3) ####

Control_demo <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Demographic\\demographic_control.csv")
Costar_demo <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Merged_Data\\costar_demographic_merged.csv")

# Use t-test to compare mean differences for each percentage variables

test_pct_white <- t.test(Control_demo$pct_white, Costar_demo$pct_white)
test_pct_white
sd(Control_demo$pct_white)
sd(Costar_demo$pct_white)

test_pct_poverty <- t.test(Control_demo$pct_poverty, Costar_demo$pct_poverty)
test_pct_poverty
sd(Control_demo$pct_poverty)
sd(Costar_demo$pct_poverty)

test_pct_rent <- t.test(Control_demo$pct_renting, Costar_demo$pct_renting)
test_pct_rent
sd(Control_demo$pct_renting)
sd(Costar_demo$pct_renting)

test_pct_edu <- t.test(Control_demo$pct_edu_under_highschool, Costar_demo$pct_edu_under_highschool)
test_pct_edu
sd(Control_demo$pct_edu_under_highschool)
sd(Costar_demo$pct_edu_under_highschool)

test_pct_old <- t.test(Control_demo$pct_age_over_65, Costar_demo$pct_age_over_65)
test_pct_old
sd(Control_demo$pct_age_over_65)
sd(Costar_demo$pct_age_over_65)

test_pct_income <- t.test(Control_demo$med_household_income, Costar_demo$med_household_income)
test_pct_income
sd(Control_demo$med_household_income)
sd(Costar_demo$med_household_income)

#### 9. ANALYSIS: Descriptive/Comparative analysis for PM2.5/EC (Table 4) ####

### PM2.5

  # Import PM2.5 Data for Entire CA
  
  CA_PM25 <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\PM25_Data\\PM25_CA_2000_2018.csv")
  
  # Import PM2.5 Data merged with Costar (Warehouse concentrated area)
  
  Costar_PM25 <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Merged_Data\\costar_demographic_PM_merged.csv")
  
  # Import Domain zipcodes and subset the entire CA data to domain region
  
  Control_zip <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\zipcode_no_warehouse_control.csv") %>%
    select(Zip = "ZIP")
  
  Control_PM25 <- merge(CA_PM25, Control_zip, by.x = c("Zip")) %>%
    select(-X)
  
  # Calculate annual average of PM2.5 for both warehouse concentrated area and entire domain
  
  # Warehouse concentrated area
  
  annual_PM25_ware <- Costar_PM25 %>%
    group_by(yearseq) %>%
    summarize(PM25_ware = mean(PM25)) %>%
    select(Year = "yearseq", PM25_ware)
  
  annual_PM25_ware_all <- Costar_PM25 %>%
    summarize(yearseq = paste0("All_years"),
              PM25_ware = mean(PM25))%>%
    select(Year = "yearseq", PM25_ware)
  
  annual_PM25_ware_final <- rbind(annual_PM25_ware, annual_PM25_ware_all)
  
  # Control Region
  
  annual_PM25_control <- Control_PM25 %>%
    group_by(year) %>%
    summarize(PM25_control = mean(pm25)) %>%
    select(Year = "year", PM25_control)
  
  annual_PM25_control_all <- Control_PM25 %>%
    summarize(Year = paste0("All_years"),
              PM25_control = mean(pm25))
  
  annual_PM25_control_final <- rbind(annual_PM25_control, annual_PM25_control_all)
  
  # Combine dataset for control region and warehouse concentrated area (Table 4)
  
  annual_PM25_desriptive <- merge(annual_PM25_ware_final, annual_PM25_control_final,
                                  by.x = c("Year"))


### Elemental Carbon

  # Import EC Data for Entire CA
  
  CA_EC <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\EC_Data\\EC_CA_2000_2020.csv")
  
  # Import EC Data merged with Costar (Warehouse concentrated area)
  
  Costar_EC <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Merged_Data\\costar_demographic_EC_merged.csv")
  Costar_EC <- na.omit(Costar_EC)
  
  # Import control zipcodes and subset the entire CA data to control region
  
  Control_zip <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\zipcode_no_warehouse_control.csv") %>%
    select(Zip = "ZIP")
  
  Control_EC <- merge(CA_EC, Control_zip, by.x = c("Zip")) %>%
    select(-X)
  
  # Calculate annual average of EC for both warehouse concentrated area and entire domain
  
  # Warehouse concentrated area
  
  annual_EC_ware <- Costar_EC %>%
    group_by(yearseq) %>%
    summarize(EC_ware = mean(EC)) %>%
    select(Year = "yearseq", EC_ware)
  
  annual_EC_ware_all <- Costar_EC %>%
    summarize(yearseq = paste0("All_years"),
              EC_ware = mean(EC))%>%
    select(Year = "yearseq", EC_ware)
  
  annual_EC_ware_final <- rbind(annual_EC_ware, annual_EC_ware_all)
  
  # Control Region
  
  annual_EC_control <- Control_EC %>%
    group_by(year) %>%
    summarize(EC_control = mean(ec)) %>%
    select(Year = "year", EC_control)
  
  annual_EC_control_all <- Control_EC %>%
    summarize(Year = paste0("All_years"),
              EC_control = mean(ec))
  
  annual_EC_control_final <- rbind(annual_EC_control, annual_EC_control_all)
  
  # Combine dataset for control region and warehouse concentrated area (Table 4)
  
  annual_EC_desriptive <- merge(annual_EC_ware_final, annual_EC_control_final,
                                by.x = c("Year"))
  
  # Combine final descriptive data for PM2.5 and EC to form complete Table 4
  
  PM25_EC_annual_descriptive <- merge(annual_EC_desriptive, annual_PM25_desriptive,
                                      by.x = c("Year"), all = TRUE)
  # Save dataset in archive
  
  write.csv(PM25_EC_annual_descriptive,"C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Figures\\Figure Data\\PM25_EC_annual_descriptive_Table_4.csv" )
  

### Compare mean differences between warehouse concentrated area and study domain

  # PM2.5
  
  t_test_PM25 <-  t.test(Control_PM25$pm25, Costar_PM25$PM25)
  t_test_PM25
  sd(Control_PM25$pm25)
  sd(Costar_PM25$PM25)
  
  # EC
  
  t_test_EC <-  t.test(Control_EC$ec, Costar_EC$EC)
  t_test_EC
  sd(Control_EC$ec)
  sd(Costar_EC$EC)

#### 10. ANALYSIS: Graphing PM2.5 and EC Descriptive Data (Figure 3) ####
  
  ### PM2.5
  
  require(ggplot2)
  
  # Import the final descriptive dataset generated from last step
  
  PM25_EC_annual_descriptive <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Figures\\Figure Data\\PM25_EC_annual_descriptive_Table_4.csv") %>%
    select(-X)
  
  Figure_3a <- PM25_EC_annual_descriptive %>%
    filter(Year <= 2018)%>%
    ggplot(aes(x = as.numeric(Year), y = PM25_ware), group = 1) +
    geom_smooth(aes(y = PM25_ware),method = "loess", linetype = "dashed", color = "steelblue", linewidth = 0.75, se=F) +
    geom_line(aes(y = PM25_ware), size = 0.8, color = "steelblue", linetype = "solid") +
    geom_point(aes(y = PM25_ware), size = 1.4) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1,0)) +
    scale_y_continuous(breaks = seq(9, 21, by = 2), expand = c(0,0)) +
    ylim(8.5, 21) +
    theme_bw() +
    xlab("Year") + ylab("PM2.5 Concentrations (μg/m³)") +
    ggtitle("Annual Average of PM2.5 Concentrations") +
    theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text = element_text(size = 8)) 
  
  Figure_3b <- PM25_EC_annual_descriptive %>%
    filter(Year <= 2018)%>%
    ggplot(aes(x = as.numeric(Year), y = PM25_control), group = 1) +
    geom_smooth(aes(y = PM25_control),method = "loess", linetype = "dashed", color = "orange", linewidth = 0.75, se=F) +
    geom_line(aes(y = PM25_control), size = 0.8, color = "orange", linetype = "solid") +
    geom_point(aes(y = PM25_control), size = 1.4) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1,0)) +
    scale_y_continuous(breaks = seq(9, 21, by = 2), expand = c(0,0)) +     
    ylim(8.5, 21) +
    theme_bw() +
    xlab("Year") + ylab("PM2.5 Concentrations (μg/m³)") +
    ggtitle("Annual Average of PM2.5 Concentrations") +
    theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text = element_text(size = 8)) 
  
  Figure_3c <- PM25_EC_annual_descriptive %>%
    ggplot(aes(x = as.numeric(Year))) +
    stat_smooth(aes(y = PM25_ware), color = "steelblue", method = "lm", linetype = "dashed", size = 0.75, se = FALSE) +
    stat_smooth(aes(y = PM25_control), color = "orange", method = "lm", linetype = "dashed", size = 0.75, se = FALSE) +
    geom_line(aes(y = PM25_ware), color = "steelblue", size = 0.8, linetype = "solid") +
    geom_point(aes(y = PM25_ware), size = 1.4) +
    geom_line(aes(y = PM25_control), color = "orange", size = 0.8, linetype = "solid") +
    geom_point(aes(y = PM25_control), size = 1.4) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1, 0)) +
    scale_y_continuous(breaks = seq(8.5, 21, by = 2), expand = c(0.1,0)) +
    theme_bw() +
    xlab("Year") + ylab("PM2.5 Concentrations (μg/m³)") +
    ggtitle("Annual Average of PM2.5 Concentrations") +
    theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), 
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text = element_text(size = 8))  
  
  ### Elemental Carbon
  
  Figure_3d <- PM25_EC_annual_descriptive %>%
    filter(Year <= 2018)%>%
    ggplot(aes(x = as.numeric(Year), y = EC_ware), group = 1) +
    geom_smooth(aes(y = EC_ware),method = "loess", linetype = "dashed", color = "steelblue", linewidth = 0.75, se=F) +
    geom_line(aes(y = EC_ware), size = 0.8, color = "steelblue", linetype = "solid") +
    geom_point(aes(y = EC_ware), size = 1.4) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1,0)) +
    scale_y_continuous(breaks = seq(0.6, 1.4, by = 0.1), expand = c(0,0))+
    ylim(0.6, 1.4) +
    theme_bw() +
    xlab("Year") + ylab("EC Concentrations (μg/m³)") +
    ggtitle("Annual Average of EC Concentrations") +
    theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text = element_text(size = 8)) 
  
  Figure_3e <- PM25_EC_annual_descriptive %>%
    filter(Year <= 2018)%>%
    ggplot(aes(x = as.numeric(Year), y = EC_control), group = 1) +
    geom_smooth(aes(y = EC_control),method = "loess", linetype = "dashed", color = "orange", linewidth = 0.75, se=F) +
    geom_line(aes(y = EC_control), size = 0.8, color = "orange", linetype = "solid") +
    geom_point(aes(y = EC_control), size = 1.4) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1,0)) +
    scale_y_continuous(breaks = seq(0.6, 1.4, by = 0.1), expand = c(0,0))+
    ylim(0.6, 1.4) +
    theme_bw() +
    xlab("Year") + ylab("EC Concentrations (μg/m³)") +
    ggtitle("Annual Average of EC Concentrations") +
    theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text = element_text(size = 8)) 
  
  Figure_3f <- PM25_EC_annual_descriptive %>%
    ggplot(aes(x = as.numeric(Year))) +
    geom_smooth(aes(y = EC_ware, color = "Warehouse"), method = "lm", linetype = "dashed", size = 0.75, se = FALSE) +
    geom_smooth(aes(y = EC_control, color = "Control"), method = "lm", linetype = "dashed", size = 0.75, se = FALSE) +
    geom_line(aes(y = EC_ware, color = "Warehouse"), size = 0.8, linetype = "solid") +
    geom_point(aes(y = EC_ware), size = 1.4) +
    geom_line(aes(y = EC_control, color = "Control"), size = 0.8, linetype = "solid") +
    geom_point(aes(y = EC_control), size = 1.4) +
    scale_x_continuous(breaks = seq(2000, 2020, by = 4), expand = c(0.1, 0)) +
    scale_color_manual(values = c("Warehouse" = "steelblue", "Control" = "orange")) +
    theme_bw() +
    xlab("Year") + ylab("EC Concentrations (μg/m³)") +
    ggtitle("Annual Average of EC Concentrations") +
    theme(plot.title = element_text(size = 9, hjust = 0.5, face = "bold"), 
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          axis.text = element_text(size = 8),
          legend.title = element_blank(),  
          legend.text = element_text(size = 6),
          legend.position = c(0.7, 0.8))  
  
  # COmbine graphs to generate Figure 3
  
  require(cowplot)  
  
  combined_graph <- plot_grid(Figure_3a, Figure_3b, Figure_3d,Figure_3e)
  combined_graph
  
  #We only need 3c and 3f but to adjust to the optimal size we pull in 2 additional figures
  combined_graph_2 <- plot_grid (Figure_3c, Figure_3f, Figure_3a, Figure_3b)
  combined_graph_2
  
  # ggsave2("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Figures\\Figure_3_pt1.tiff", dpi = 300)  
  # ggsave2("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Figures\\Figure_3_pt2.tiff", dpi = 300)  
  
  
#### 11. CLEANING: Merge Costar, PM2.5, EC, and Demographic on zipcode levels for Regression ####

# Import Costar,PM2.5, EC, and Demographic Datasets

costar <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Costar_Yearseq_2000_2019\\costar_2000_2019_seq.csv")
PM25 <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\PM25_Data\\PM25_CA_2000_2018.csv")
EC <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\EC_Data\\EC_CA_2000_2020.csv")
Demographic <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Demographic\\demographic_subset.csv")

# Calculate annual sum of warehouse variables by zipcode

costar_annual_zip <- costar %>%
  group_by(yearseq, Zip) %>%
  summarize(totalRBA = sum(RBA, na.rm = TRUE),
            totalLD = sum(Number.Of.Loading.Docks, na.rm = TRUE),
            totalPS = sum(Number.Of.Parking.Spaces, na.rm = TRUE),
            County = unique(County.Name)) %>%
  na.omit()

write.csv(costar_annual_zip, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_merged_2000_2019.csv")

# Merge warehouse to demographic data by zipcode

Costar_Demo <- merge(costar_annual_zip,Demographic, by.x=c("Zip", "yearseq"), by.y=c("ZIP","year"), all.x=T)
Costar_Demo <- na.omit(Costar_Demo)

write.csv(Costar_Demo, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_demographic_merged.csv")

#Merge EC with costar and demographic (REGRESSION READY)

Costar_Demo_EC <- EC %>%
  group_by(year, Zip) %>%
  summarize(EC = mean(ec, na.rm = TRUE)) %>%
  merge(Costar_Demo, by.x=c("Zip", "year"), by.y=c("Zip","yearseq")) %>%
  select(-X)

write.csv(Costar_Demo_EC, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_demographic_EC_merged.csv")

#Merge PM2.5 with costar and demographic (REGRESSION READY)

Costar_Demo_PM25 <- PM25 %>%
  group_by(year, Zip) %>%
  summarize(PM25 = mean(pm25, na.rm = TRUE)) %>%
  merge(Costar_Demo, by.x=c("Zip", "year"), by.y=c("Zip","yearseq")) %>%
  na.omit() %>%
  select(-X)

write.csv(Costar_Demo_PM25, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_demographic_PM_merged.csv")


#### 12. ANALYSIS: Linear Mixed Effect Models (Table 5) ####

### PM2.5 Models

# Load data
PM_model_data <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_demographic_PM_merged.csv")

#Calculate IQR for warehouses

IQR_RBA_PM <- IQR(PM_model_data$totalRBA)
IQR_LD_PM <- IQR(PM_model_data$totalLD)
IQR_PS_PM <- IQR(PM_model_data$totalPS)

#Change all demographic from proportions to percentages

PM_model_data$pct_racial_minor <- PM_model_data$pct_racial_minor*100
PM_model_data$pct_renting <- (PM_model_data$pct_renting*100)
PM_model_data$pct_edu_under_highschool <- (PM_model_data$pct_edu_under_highschool*100)
PM_model_data$pct_age_over_65 <- (PM_model_data$pct_age_over_65*100)


# Model 1 contains warehouse capacity only
lmer_PM_RBA_1 <- lmer(PM25 ~ totalRBA + (1|year), PM_model_data)
summary(lmer_PM_RBA_1) #***
fixef(lmer_PM_RBA_1)* IQR_RBA_PM  
confint(lmer_PM_RBA_1) *IQR_RBA_PM 

lmer_PM_LD_1 <- lmer(PM25 ~ totalLD + (1|year), PM_model_data)
summary(lmer_PM_LD_1) #***
fixef(lmer_PM_LD_1)* IQR_LD_PM  
confint(lmer_PM_LD_1) *IQR_LD_PM 

lmer_PM_PS_1 <- lmer(PM25 ~ totalPS + (1|year), PM_model_data)
summary(lmer_PM_PS_1) #***
fixef(lmer_PM_PS_1)* IQR_PS_PM  
confint(lmer_PM_PS_1) *IQR_PS_PM

# Model 2 adjusted for demographic covariates
lmer_PM_RBA_2 <- lmer(PM25 ~ totalRBA + (1|year)+ (1|County) + pct_racial_minor
                      + pct_renting + med_household_income
                      + pct_age_over_65 + pct_edu_under_highschool, PM_model_data)
summary(lmer_PM_RBA_2)  #***
fixef(lmer_PM_RBA_2)["totalRBA"]* IQR_RBA_PM 
confint(lmer_PM_RBA_2)* IQR_RBA_PM
confint(lmer_PM_RBA_2)

lmer_PM_LD_2 <- lmer(PM25 ~ totalLD + (1|year)+ (1|County) + pct_racial_minor
                     + pct_renting + med_household_income
                     + pct_age_over_65 + pct_edu_under_highschool, PM_model_data)
summary(lmer_PM_LD_2)     #***
fixef(lmer_PM_LD_2)["totalLD"]* IQR_LD_PM 
confint(lmer_PM_LD_2)* IQR_LD_PM
confint(lmer_PM_LD_2)

lmer_PM_PS_2 <- lmer(PM25 ~ totalPS + (1|year)+ (1|County) + pct_racial_minor
                     + pct_renting + med_household_income
                     + pct_age_over_65 + pct_edu_under_highschool, PM_model_data)
summary(lmer_PM_PS_2)     #***
fixef(lmer_PM_PS_2)["totalPS"]* IQR_PS_PM 
confint(lmer_PM_PS_2)* IQR_PS_PM
confint(lmer_PM_PS_2)

### EC Models

# Load data

EC_model_data <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_demographic_EC_merged.csv")

# Calculate IQR for warehouses

IQR_RBA_EC <- IQR(EC_model_data$totalRBA)
IQR_LD_EC <- IQR(EC_model_data$totalLD)
IQR_PS_EC <- IQR(EC_model_data$totalPS)

# Change all demographic from proportions to percentages

EC_model_data$pct_racial_minor <- EC_model_data$pct_racial_minor*100
EC_model_data$pct_renting <- (EC_model_data$pct_renting*100)
EC_model_data$pct_edu_under_highschool <- (EC_model_data$pct_edu_under_highschool*100)
EC_model_data$pct_age_over_65 <- (EC_model_data$pct_age_over_65*100)


# Model 1 contains warehouse capacity only

lmer_EC_RBA_1 <- lmer(EC ~ totalRBA + (1|year), EC_model_data)
summary(lmer_EC_RBA_1) #***
fixef(lmer_EC_RBA_1)* IQR_RBA_EC 
confint(lmer_EC_RBA_1) *IQR_RBA_EC 

lmer_EC_LD_1 <- lmer(EC ~ totalLD + (1|year), EC_model_data)
summary(lmer_EC_LD_1) #***
fixef(lmer_EC_LD_1)* IQR_LD_EC  
confint(lmer_EC_LD_1) *IQR_LD_EC 

lmer_EC_PS_1 <- lmer(EC ~ totalPS + (1|year), EC_model_data)
summary(lmer_EC_PS_1) #***
fixef(lmer_EC_PS_1)* IQR_PS_EC  
confint(lmer_EC_PS_1) *IQR_PS_EC

# Model 2 adjusted for demographic covariates

lmer_EC_RBA_2 <- lmer(EC ~ totalRBA + (1|year)+ (1|County) + pct_racial_minor
                      + pct_renting + med_household_income
                      + pct_age_over_65 + pct_edu_under_highschool, EC_model_data)
summary(lmer_EC_RBA_2)  #***
fixef(lmer_EC_RBA_2)["totalRBA"]* IQR_RBA_EC 
confint(lmer_EC_RBA_2)* IQR_RBA_EC
confint(lmer_EC_RBA_2)

lmer_EC_LD_2 <- lmer(EC ~ totalLD + (1|year) + (1|County) + pct_racial_minor
                     + pct_renting + med_household_income
                     + pct_age_over_65 + pct_edu_under_highschool, EC_model_data)
summary(lmer_EC_LD_2)     #***
fixef(lmer_EC_LD_2)["totalLD"]* IQR_LD_EC 
confint(lmer_EC_LD_2)* IQR_LD_EC
confint(lmer_EC_LD_2)

lmer_EC_PS_2 <- lmer(EC ~ totalPS + (1|year)+ (1|County) + pct_racial_minor
                     + pct_renting + med_household_income
                     + pct_age_over_65 + pct_edu_under_highschool, EC_model_data)
summary(lmer_EC_PS_2)     #***
fixef(lmer_EC_PS_2)["totalPS"]* IQR_PS_EC
confint(lmer_EC_PS_2)* IQR_PS_EC
confint(lmer_EC_PS_2)

#### 13. ANALYSIS: Associations in Five-Year Rolling Periods (Linear Regression) ####
rm(list = ls())

require(pacman)
p_load(dplyr, tidyverse)

# Load merged data for regression 

PM_data_all <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_demographic_PM_merged.csv") %>%
  select(-X)
EC_data_all <- read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\costar_demographic_EC_merged.csv") %>%
  select(-X)

# PM2.5

for (start_year in 2000:2014) { # Adjust the sequence to increment by 1 for rolling intervals
  end_year <- start_year + 4
  period_name <- paste("PM_", start_year, "_", end_year, sep = "")
  
  # Filter PM2.5 dataset for the current five-year rolling period
  assign(period_name, subset(PM_data_all, year >= start_year & year <= end_year))
}


# EC

for (start_year in 2000:2015) { # Adjust the sequence to increment by 1 for rolling intervals
  end_year <- start_year + 4
  period_name <- paste("EC_", start_year, "_", end_year, sep = "")
  
  # Filter EC dataset for the current five-year rolling period
  assign(period_name, subset(EC_data_all, year >= start_year & year <= end_year))
}



# Linear Regression for PM2.5

# Test for multicolinearity 

cor_matrix <- cor(data.frame(PM_data_all$totalRBA, PM_data_all$totalLD, PM_data_all$totalPS))
print(cor_matrix)   
# all variables are highly correlated with each other, therefore should run regression separately

# Perform linear regression for each dataset

# Create an empty data frame to store the results
results <- data.frame()

# Loop through each period
for (period_name in ls(pattern = "^PM_2")) {
  # Extract PM2.5 dataset for the current period
  PM_period <- get(period_name)
  
  # Extract time period without "PM_" prefix
  time_period <- gsub("^PM_", "", period_name)
  
  # Perform linear regression for rentable building area (RBA)
  lm_PM_RBA <- lm(PM25 ~ totalRBA + pct_racial_minor
                  + pct_renting + med_household_income
                  + pct_age_over_65 + pct_edu_under_highschool + year, data = PM_period)
  
  # Extract coefficients, standard errors, and p-values for RBA
  summary_RBA <- summary(lm_PM_RBA)$coefficients["totalRBA", ]
  beta_RBA <- summary_RBA["Estimate"]
  error_RBA <- summary_RBA["Std. Error"]
  p_value_RBA <- summary_RBA["Pr(>|t|)"]
  
  # Create a data frame for RBA results
  df_RBA <- data.frame(
    Variable = "RBA",
    Time_Period = time_period,
    Beta = beta_RBA,
    Error = error_RBA,
    P_Value = p_value_RBA,
    Pollution = "PM2.5"
  )
  
  # Perform linear regression for number of loading docks (LD)
  lm_PM_LD <- lm(PM25 ~ totalLD + pct_racial_minor
                 + pct_renting + med_household_income
                 + pct_age_over_65 + pct_edu_under_highschool + year, data = PM_period)
  
  # Extract coefficients, standard errors, and p-values for LD
  summary_LD <- summary(lm_PM_LD)$coefficients["totalLD", ]
  beta_LD <- summary_LD["Estimate"]
  error_LD <- summary_LD["Std. Error"]
  p_value_LD <- summary_LD["Pr(>|t|)"]
  
  # Create a data frame for LD results
  df_LD <- data.frame(
    Variable = "LD",
    Time_Period = time_period,
    Beta = beta_LD,
    Error = error_LD,
    P_Value = p_value_LD,
    Pollution = "PM2.5"
  )
  
  # Perform linear regression for number of parking spaces (PS)
  lm_PM_PS <- lm(PM25 ~ totalPS + pct_racial_minor
                 + pct_renting + med_household_income
                 + pct_age_over_65 + pct_edu_under_highschool + year, data = PM_period)
  
  # Extract coefficients, standard errors, and p-values for PS
  summary_PS <- summary(lm_PM_PS)$coefficients["totalPS", ]
  beta_PS <- summary_PS["Estimate"]
  error_PS <- summary_PS["Std. Error"]
  p_value_PS <- summary_PS["Pr(>|t|)"]
  
  # Create a data frame for PS results
  df_PS <- data.frame(
    Variable = "PS",
    Time_Period = time_period,
    Beta = beta_PS,
    Error = error_PS,
    P_Value = p_value_PS,
    Pollution = "PM2.5"
  )
  
  # Bind the results for RBA, LD, and PS
  results <- rbind(results, df_RBA, df_LD, df_PS)
}

# View the results
print(results)


# Linear Regression for EC
# Create an empty data frame to store the results for EC
results_EC <- data.frame()

# Loop through each period
for (period_name in ls(pattern = "^EC_2")) {
  # Extract EC dataset for the current period
  EC_period <- get(period_name)
  
  # Extract time period without "EC_" prefix
  time_period <- gsub("^EC_", "", period_name)
  
  # Perform linear regression for rentable building area (RBA)
  lm_EC_RBA <- lm(EC ~ totalRBA + pct_racial_minor
                  + pct_renting + med_household_income
                  + pct_age_over_65 + pct_edu_under_highschool + year, data = EC_period)
  
  # Extract coefficients, standard errors, and p-values for RBA
  summary_EC_RBA <- summary(lm_EC_RBA)$coefficients["totalRBA", ]
  beta_EC_RBA <- summary_EC_RBA["Estimate"]
  error_EC_RBA <- summary_EC_RBA["Std. Error"]
  p_value_EC_RBA <- summary_EC_RBA["Pr(>|t|)"]
  
  # Create a data frame for RBA results for EC
  df_EC_RBA <- data.frame(
    Variable = "RBA",
    Time_Period = time_period,
    Beta = beta_EC_RBA,
    Error = error_EC_RBA,
    P_Value = p_value_EC_RBA,
    Pollution = "EC"
  )
  
  # Perform linear regression for number of loading docks (LD)
  lm_EC_LD <- lm(EC ~ totalLD + pct_racial_minor
                 + pct_renting + med_household_income
                 + pct_age_over_65 + pct_edu_under_highschool + year, data = EC_period)
  
  # Extract coefficients, standard errors, and p-values for LD
  summary_EC_LD <- summary(lm_EC_LD)$coefficients["totalLD", ]
  beta_EC_LD <- summary_EC_LD["Estimate"]
  error_EC_LD <- summary_EC_LD["Std. Error"]
  p_value_EC_LD <- summary_EC_LD["Pr(>|t|)"]
  
  # Create a data frame for LD results for EC
  df_EC_LD <- data.frame(
    Variable = "LD",
    Time_Period = time_period,
    Beta = beta_EC_LD,
    Error = error_EC_LD,
    P_Value = p_value_EC_LD,
    Pollution = "EC"
  )
  
  # Perform linear regression for number of parking spaces (PS)
  lm_EC_PS <- lm(EC ~ totalPS + pct_racial_minor
                 + pct_renting + med_household_income
                 + pct_age_over_65 + pct_edu_under_highschool + year, data = EC_period)
  
  # Extract coefficients, standard errors, and p-values for PS
  summary_EC_PS <- summary(lm_EC_PS)$coefficients["totalPS", ]
  beta_EC_PS <- summary_EC_PS["Estimate"]
  error_EC_PS <- summary_EC_PS["Std. Error"]
  p_value_EC_PS <- summary_EC_PS["Pr(>|t|)"]
  
  # Create a data frame for PS results for EC
  df_EC_PS <- data.frame(
    Variable = "PS",
    Time_Period = time_period,
    Beta = beta_EC_PS,
    Error = error_EC_PS,
    P_Value = p_value_EC_PS,
    Pollution = "EC"
  )
  
  # Bind the results for RBA, LD, and PS for EC
  results_EC <- rbind(results_EC, df_EC_RBA, df_EC_LD, df_EC_PS)
}

# View the results for EC
print(results_EC)

# Combine results for PM2.5 and EC and clean the data frame

results_combined <- rbind(results, results_EC)

results_cleaned <- results_combined %>%
  rename(warehouse_var = Variable,
         time_period = Time_Period,
         estimate = Beta,
         std_error = Error,
         p_value = P_Value,
         pollutant = Pollution)

write.csv(results_cleaned, "C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Regression_results_five_year_rolling_period.csv", row.names = F)


### Graphing estimate trends

require(dplyr)
require(lubridate)
require(ggplot2)
require(tidyr)
require(gridExtra)
require(scales)
require(cowplot)

dat = read.csv("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Regression_results_five_year_rolling_period.csv")
dat$warehouse_var = factor(dat$warehouse_var,levels = c("RBA","LD","PS"),
                           labels = c("RBA", "Loading Docks","Parking Spaces"))
dat$CI_upper = dat$estimate + 1.96 * dat$std_error
dat$CI_lower = dat$estimate - 1.96 * dat$std_error
pd = position_dodge(0.5)

plot1 = dat %>% ggplot(., aes(x = time_period, y = estimate,  color = pollutant)) + 
  geom_smooth() +
  geom_point(position = pd,size = 1.5) +
  geom_errorbar(aes(x = time_period, ymax = CI_upper,ymin = CI_lower),position = pd,linewidth = 1.0) + 
  facet_wrap(pollutant~warehouse_var, scales = "free_y")+
  xlab("Time Periods") + ylab(paste0("Beta Coefficients (95% CI)"))+ 
#  scale_y_continuous(labels = label_number(accuracy = 0.005)) +
  theme_bw() + 
  theme(strip.placement = "outside",strip.background.x=element_rect(color = NA,  fill=NA),strip.background.y=element_rect(color = NA,  fill=NA)) +
  theme(axis.title.x = element_text(size = 10, face = "bold"))  +
  theme(axis.title.y = element_text(size = 11, face = "bold"))  +
  theme(axis.text.x = element_text(size = 6,angle = 45,vjust = 1, hjust=1)) + 
  theme(axis.text.y = element_text(size = 9)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_color_manual(values = c("orange","steelblue"))
plot1

ggsave2("C:\\Users\\byang77\\Desktop\\SoCal Warehouse\\Revision_data\\Supplement_1.tiff", dpi = 300)
