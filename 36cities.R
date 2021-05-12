# Quant IV
# Uni: pr2576
# Downloading census data covariates for 36 cities

# install and load packages
  install.packages("tidyverse")
  install.packages("tidycensus")
  library(tidycensus)
  library(tibble)
  library(dplyr)
  library(tidyverse)

# load starting database
  setwd("C:/Users/Pranav Ramkumar/Desktop/Quant Series/Q4/Data collection/36 cities")
  census = read.csv("census_start.csv")
  census$State.FIPS.Code <- formatC(census$State.FIPS.Code, width = 2, format = "d", flag = "0")
  census$Place.FIPS.Code <- formatC(census$Place.FIPS.Code, width = 5, format = "d", flag = "0")
  census$geoid <- paste(census$State.FIPS.Code,census$Place.FIPS.Code,sep="")

# browse variables
  vars <- load_variables(year=2015, dataset = "acs1", cache=TRUE)
  
# The following are the census fields we need
  # S0101_C01_001 - Total!!Estimate!!Total populationA
  # S0101_C02_001 - Male!!Estimate!!Total population
  # Male/Total - pct_male
  # S0101_C03_001 - Female!!Estimate!!Total population
  # S0101_C01_032 - Total!!Estimate!!SUMMARY INDICATORS!!Median age (years)
  
  # B01001_001 - TotalPopulationB 
  # B01001_018-025 + B01001_042-049 - Population Age 60 and over
  # Age60andover/Total - pct60andover
  # B01001_020-025 + B01001_044-049 - Population Age 65 and over
  # Age65andover/Total - pct65andover
  # B01001_023-025 + B01001_047-049 - Population Age 75 and over
  # Age75andover/Total - pct75andover

  # B02001_001 - Estimate!!TotalPopulationC
  # B02001_002 - Estimate!!Total!!White alone
  # White/Total - pct_white
  # B02001_003 - Estimate!!Total!!Black or African American alone
  # Black/Total - pct_black
  # B02001_004 - Estimate!!Total!!American Indian and Alaska Native alone
  # B02001_005 - Estimate!!Total!!Asian alone	
  # B02001_006 - Estimate!!Total!!Native Hawaiian and Other Pacific Islander alone
  # B02001_007 - Estimate!!Total!!Some other race alone

# Add all the empty columns into the census table
  census$TotalpopA2010 <- NA
  census$Malepop2010 <- NA
  census$pctmale2010 <- NA
  census$Femalepop2010 <- NA
  census$Medianage2010 <- NA
  census$TotalpopB2010 <- NA
  census$Age60andover2010 <- NA
  census$pct60andover2010 <- NA
  census$Age65andover2010 <- NA
  census$pct65andover2010 <- NA
  census$Age75andover2010 <- NA
  census$pct75andover2010 <- NA
  census$TotalpopC2010 <- NA
  census$Whitepop2010 <- NA
  census$pct_white2010 <- NA
  census$Blackpop2010 <- NA
  census$pct_black2010 <- NA
  census$AmericanIndian_AlaskaNativepop2010 <- NA
  census$Asianpop2010 <- NA
  census$NativeHawaiian_PacificIslanderpop2010 <- NA
  census$Otherpop2010 <- NA
  
  census$TotalpopA2011 <- NA
  census$Malepop2011 <- NA
  census$pctmale2011 <- NA
  census$Femalepop2011 <- NA
  census$Medianage2011 <- NA
  census$TotalpopB2011 <- NA
  census$Age60andover2011 <- NA
  census$pct60andover2011 <- NA
  census$Age65andover2011 <- NA
  census$pct65andover2011 <- NA
  census$Age75andover2011 <- NA
  census$pct75andover2011 <- NA
  census$TotalpopC2011 <- NA
  census$Whitepop2011 <- NA
  census$pct_white2011 <- NA
  census$Blackpop2011 <- NA
  census$pct_black2011 <- NA
  census$AmericanIndian_AlaskaNativepop2011 <- NA
  census$Asianpop2011 <- NA
  census$NativeHawaiian_PacificIslanderpop2011 <- NA
  census$Otherpop2011 <- NA
  
  census$TotalpopA2012 <- NA
  census$Malepop2012 <- NA
  census$pctmale2012 <- NA
  census$Femalepop2012 <- NA
  census$Medianage2012 <- NA
  census$TotalpopB2012 <- NA
  census$Age60andover2012 <- NA
  census$pct60andover2012 <- NA
  census$Age65andover2012 <- NA
  census$pct65andover2012 <- NA
  census$Age75andover2012 <- NA
  census$pct75andover2012 <- NA
  census$TotalpopC2012 <- NA
  census$Whitepop2012 <- NA
  census$pct_white2012 <- NA
  census$Blackpop2012 <- NA
  census$pct_black2012 <- NA
  census$AmericanIndian_AlaskaNativepop2012 <- NA
  census$Asianpop2012 <- NA
  census$NativeHawaiian_PacificIslanderpop2012 <- NA
  census$Otherpop2012 <- NA
  
  census$TotalpopA2013 <- NA
  census$Malepop2013 <- NA
  census$pctmale2013 <- NA
  census$Femalepop2013 <- NA
  census$Medianage2013 <- NA
  census$TotalpopB2013 <- NA
  census$Age60andover2013 <- NA
  census$pct60andover2013 <- NA
  census$Age65andover2013 <- NA
  census$pct65andover2013 <- NA
  census$Age75andover2013 <- NA
  census$pct75andover2013 <- NA
  census$TotalpopC2013 <- NA
  census$Whitepop2013 <- NA
  census$pct_white2013 <- NA
  census$Blackpop2013 <- NA
  census$pct_black2013 <- NA
  census$AmericanIndian_AlaskaNativepop2013 <- NA
  census$Asianpop2013 <- NA
  census$NativeHawaiian_PacificIslanderpop2013 <- NA
  census$Otherpop2013 <- NA
  
  census$TotalpopA2014 <- NA
  census$Malepop2014 <- NA
  census$pctmale2014 <- NA
  census$Femalepop2014 <- NA
  census$Medianage2014 <- NA
  census$TotalpopB2014 <- NA
  census$Age60andover2014 <- NA
  census$pct60andover2014 <- NA
  census$Age65andover2014 <- NA
  census$pct65andover2014 <- NA
  census$Age75andover2014 <- NA
  census$pct75andover2014 <- NA
  census$TotalpopC2014 <- NA
  census$Whitepop2014 <- NA
  census$pct_white2014 <- NA
  census$Blackpop2014 <- NA
  census$pct_black2014 <- NA
  census$AmericanIndian_AlaskaNativepop2014 <- NA
  census$Asianpop2014 <- NA
  census$NativeHawaiian_PacificIslanderpop2014 <- NA
  census$Otherpop2014 <- NA
  
  census$TotalpopA2015 <- NA
  census$Malepop2015 <- NA
  census$pctmale2015 <- NA
  census$Femalepop2015 <- NA
  census$Medianage2015 <- NA
  census$TotalpopB2015 <- NA
  census$Age60andover2015 <- NA
  census$pct60andover2015 <- NA
  census$Age65andover2015 <- NA
  census$pct65andover2015 <- NA
  census$Age75andover2015 <- NA
  census$pct75andover2015 <- NA
  census$TotalpopC2015 <- NA
  census$Whitepop2015 <- NA
  census$pct_white2015 <- NA
  census$Blackpop2015 <- NA
  census$pct_black2015 <- NA
  census$AmericanIndian_AlaskaNativepop2015 <- NA
  census$Asianpop2015 <- NA
  census$NativeHawaiian_PacificIslanderpop2015 <- NA
  census$Otherpop2015 <- NA
  
# Get the data for all 36 cities
for (i in 1:36) {
  # 2010
  my.data <-read.csv(census[i,"S0101_url_2010"], header = T)
  census[i,"TotalpopA2010"]<-my.data$S0101_C01_001E
  census[i,"Malepop2010"]<-my.data$S0101_C02_001E
  census[i,"pctmale2010"]<-census[i,"Malepop2010"]/census[i,"TotalpopA2010"]
  census[i,"Femalepop2010"]<-my.data$S0101_C03_001E
  census[i,"Medianage2010"]<-my.data$S0101_C01_032E
  census[i,"TotalpopA2010"]<-my.data$S0101_C01_001E
  
  my.data <-read.csv(census[i,"B01001_url_2010"], header = T)
  census[i,"TotalpopB2010"]<-my.data$B01001_001E
  census[i,"Age60andover2010"] <- (my.data$B01001_018E+my.data$B01001_019E+my.data$B01001_020E+
                                   my.data$B01001_021E+my.data$B01001_022E+my.data$B01001_023E+
                                   my.data$B01001_024E+my.data$B01001_025E+
                                   my.data$B01001_042E+my.data$B01001_043E+my.data$B01001_044E+
                                   my.data$B01001_045E+my.data$B01001_046E+my.data$B01001_047E+
                                   my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct60andover2010"] <-census[i,"Age60andover2010"]/census[i,"TotalpopB2010"]
  census[i,"Age65andover2010"] <-(my.data$B01001_020E+my.data$B01001_021E+my.data$B01001_022E+
                                  my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                  my.data$B01001_044E+my.data$B01001_045E+my.data$B01001_046E+
                                  my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct65andover2010"] <-census[i,"Age65andover2010"]/census[i,"TotalpopB2010"]
  census[i,"Age75andover2010"] <-(my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                  my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct75andover2010"] <-census[i,"Age75andover2010"]/census[i,"TotalpopB2010"]
  
  my.data <-read.csv(census[i,"B02001_url_2010"], header = T)
  census[i,"TotalpopC2010"]<-my.data$B02001_001E
  census[i,"Whitepop2010"]<-my.data$B02001_002E
  census[i,"pct_white2010"]<-census[i,"Whitepop2010"]/census[i,"TotalpopC2010"]
  census[i,"Blackpop2010"]<-my.data$B02001_003E
  census[i,"pct_black2010"]<-census[i,"Blackpop2010"]/census[i,"TotalpopC2010"]
  census[i,"AmericanIndian_AlaskaNativepop2010"]<-my.data$B02001_004E
  census[i,"Asianpop2010"]<-my.data$B02001_005E
  census[i,"NativeHawaiian_PacificIslanderpop2010"]<-my.data$B02001_006E
  census[i,"Otherpop2010"]<-my.data$B02001_007E
  
  # 2011
  my.data <-read.csv(census[i,"S0101_url_2011"], header = T)
  census[i,"TotalpopA2011"]<-my.data$S0101_C01_001E
  census[i,"Malepop2011"]<-my.data$S0101_C02_001E
  census[i,"pctmale2011"]<-census[i,"Malepop2011"]/census[i,"TotalpopA2011"]
  census[i,"Femalepop2011"]<-my.data$S0101_C03_001E
  census[i,"Medianage2011"]<-my.data$S0101_C01_032E
  
  my.data <-read.csv(census[i,"B01001_url_2011"], header = T)
  census[i,"TotalpopB2011"]<-my.data$B01001_001E
  census[i,"Age60andover2011"] <- (my.data$B01001_018E+my.data$B01001_019E+my.data$B01001_020E+
                                     my.data$B01001_021E+my.data$B01001_022E+my.data$B01001_023E+
                                     my.data$B01001_024E+my.data$B01001_025E+
                                     my.data$B01001_042E+my.data$B01001_043E+my.data$B01001_044E+
                                     my.data$B01001_045E+my.data$B01001_046E+my.data$B01001_047E+
                                     my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct60andover2011"] <-census[i,"Age60andover2011"]/census[i,"TotalpopB2011"]
  census[i,"Age65andover2011"] <-(my.data$B01001_020E+my.data$B01001_021E+my.data$B01001_022E+
                                    my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_044E+my.data$B01001_045E+my.data$B01001_046E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct65andover2011"] <-census[i,"Age65andover2011"]/census[i,"TotalpopB2011"]
  census[i,"Age75andover2011"] <-(my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct75andover2011"] <-census[i,"Age75andover2011"]/census[i,"TotalpopB2011"]
  
  my.data <-read.csv(census[i,"B02001_url_2011"], header = T)
  census[i,"TotalpopC2011"]<-my.data$B02001_001E
  census[i,"Whitepop2011"]<-my.data$B02001_002E
  census[i,"pct_white2011"]<-census[i,"Whitepop2011"]/census[i,"TotalpopC2011"]
  census[i,"Blackpop2011"]<-my.data$B02001_003E
  census[i,"pct_black2011"]<-census[i,"Blackpop2011"]/census[i,"TotalpopC2011"]
  census[i,"AmericanIndian_AlaskaNativepop2011"]<-my.data$B02001_004E
  census[i,"Asianpop2011"]<-my.data$B02001_005E
  census[i,"NativeHawaiian_PacificIslanderpop2011"]<-my.data$B02001_006E
  census[i,"Otherpop2011"]<-my.data$B02001_007E
  
  # 2012
  my.data <-read.csv(census[i,"S0101_url_2012"], header = T)
  census[i,"TotalpopA2012"]<-my.data$S0101_C01_001E
  census[i,"Malepop2012"]<-my.data$S0101_C02_001E
  census[i,"pctmale2012"]<-census[i,"Malepop2012"]/census[i,"TotalpopA2012"]
  census[i,"Femalepop2012"]<-my.data$S0101_C03_001E
  census[i,"Medianage2012"]<-my.data$S0101_C01_032E
  
  my.data <-read.csv(census[i,"B01001_url_2012"], header = T)
  census[i,"TotalpopB2012"]<-my.data$B01001_001E
  census[i,"Age60andover2012"] <- (my.data$B01001_018E+my.data$B01001_019E+my.data$B01001_020E+
                                     my.data$B01001_021E+my.data$B01001_022E+my.data$B01001_023E+
                                     my.data$B01001_024E+my.data$B01001_025E+
                                     my.data$B01001_042E+my.data$B01001_043E+my.data$B01001_044E+
                                     my.data$B01001_045E+my.data$B01001_046E+my.data$B01001_047E+
                                     my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct60andover2012"] <-census[i,"Age60andover2012"]/census[i,"TotalpopB2012"]
  census[i,"Age65andover2012"] <-(my.data$B01001_020E+my.data$B01001_021E+my.data$B01001_022E+
                                    my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_044E+my.data$B01001_045E+my.data$B01001_046E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct65andover2012"] <-census[i,"Age65andover2012"]/census[i,"TotalpopB2012"]
  census[i,"Age75andover2012"] <-(my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct75andover2012"] <-census[i,"Age75andover2012"]/census[i,"TotalpopB2012"]
  
  my.data <-read.csv(census[i,"B02001_url_2012"], header = T)
  census[i,"TotalpopC2012"]<-my.data$B02001_001E
  census[i,"Whitepop2012"]<-my.data$B02001_002E
  census[i,"pct_white2012"]<-census[i,"Whitepop2012"]/census[i,"TotalpopC2012"]
  census[i,"Blackpop2012"]<-my.data$B02001_003E
  census[i,"pct_black2012"]<-census[i,"Blackpop2012"]/census[i,"TotalpopC2012"]
  census[i,"AmericanIndian_AlaskaNativepop2012"]<-my.data$B02001_004E
  census[i,"Asianpop2012"]<-my.data$B02001_005E
  census[i,"NativeHawaiian_PacificIslanderpop2012"]<-my.data$B02001_006E
  census[i,"Otherpop2012"]<-my.data$B02001_007E
  
  # 2013
  my.data <-read.csv(census[i,"S0101_url_2013"], header = T)
  census[i,"TotalpopA2013"]<-my.data$S0101_C01_001E
  census[i,"Malepop2013"]<-my.data$S0101_C02_001E
  census[i,"pctmale2013"]<-census[i,"Malepop2013"]/census[i,"TotalpopA2013"]
  census[i,"Femalepop2013"]<-my.data$S0101_C03_001E
  census[i,"Medianage2013"]<-my.data$S0101_C01_032E
  
  my.data <-read.csv(census[i,"B01001_url_2013"], header = T)
  census[i,"TotalpopB2013"]<-my.data$B01001_001E
  census[i,"Age60andover2013"] <- (my.data$B01001_018E+my.data$B01001_019E+my.data$B01001_020E+
                                     my.data$B01001_021E+my.data$B01001_022E+my.data$B01001_023E+
                                     my.data$B01001_024E+my.data$B01001_025E+
                                     my.data$B01001_042E+my.data$B01001_043E+my.data$B01001_044E+
                                     my.data$B01001_045E+my.data$B01001_046E+my.data$B01001_047E+
                                     my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct60andover2013"] <-census[i,"Age60andover2013"]/census[i,"TotalpopB2013"]
  census[i,"Age65andover2013"] <-(my.data$B01001_020E+my.data$B01001_021E+my.data$B01001_022E+
                                    my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_044E+my.data$B01001_045E+my.data$B01001_046E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct65andover2013"] <-census[i,"Age65andover2013"]/census[i,"TotalpopB2013"]
  census[i,"Age75andover2013"] <-(my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct75andover2013"] <-census[i,"Age75andover2013"]/census[i,"TotalpopB2013"]
  
  my.data <-read.csv(census[i,"B02001_url_2013"], header = T)
  census[i,"TotalpopC2013"]<-my.data$B02001_001E
  census[i,"Whitepop2013"]<-my.data$B02001_002E
  census[i,"pct_white2013"]<-census[i,"Whitepop2013"]/census[i,"TotalpopC2013"]
  census[i,"Blackpop2013"]<-my.data$B02001_003E
  census[i,"pct_black2013"]<-census[i,"Blackpop2013"]/census[i,"TotalpopC2013"]
  census[i,"AmericanIndian_AlaskaNativepop2013"]<-my.data$B02001_004E
  census[i,"Asianpop2013"]<-my.data$B02001_005E
  census[i,"NativeHawaiian_PacificIslanderpop2013"]<-my.data$B02001_006E
  census[i,"Otherpop2013"]<-my.data$B02001_007E
  
  # 2014
  my.data <-read.csv(census[i,"S0101_url_2014"], header = T)
  census[i,"TotalpopA2014"]<-my.data$S0101_C01_001E
  census[i,"Malepop2014"]<-my.data$S0101_C02_001E
  census[i,"pctmale2014"]<-census[i,"Malepop2014"]/census[i,"TotalpopA2014"]
  census[i,"Femalepop2014"]<-my.data$S0101_C03_001E
  census[i,"Medianage2014"]<-my.data$S0101_C01_032E
  
  my.data <-read.csv(census[i,"B01001_url_2014"], header = T)
  census[i,"TotalpopB2014"]<-my.data$B01001_001E
  census[i,"Age60andover2014"] <- (my.data$B01001_018E+my.data$B01001_019E+my.data$B01001_020E+
                                     my.data$B01001_021E+my.data$B01001_022E+my.data$B01001_023E+
                                     my.data$B01001_024E+my.data$B01001_025E+
                                     my.data$B01001_042E+my.data$B01001_043E+my.data$B01001_044E+
                                     my.data$B01001_045E+my.data$B01001_046E+my.data$B01001_047E+
                                     my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct60andover2014"] <-census[i,"Age60andover2014"]/census[i,"TotalpopB2014"]
  census[i,"Age65andover2014"] <-(my.data$B01001_020E+my.data$B01001_021E+my.data$B01001_022E+
                                    my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_044E+my.data$B01001_045E+my.data$B01001_046E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct65andover2014"] <-census[i,"Age65andover2014"]/census[i,"TotalpopB2014"]
  census[i,"Age75andover2014"] <-(my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct75andover2014"] <-census[i,"Age75andover2014"]/census[i,"TotalpopB2014"]
  
  my.data <-read.csv(census[i,"B02001_url_2014"], header = T)
  census[i,"TotalpopC2014"]<-my.data$B02001_001E
  census[i,"Whitepop2014"]<-my.data$B02001_002E
  census[i,"pct_white2014"]<-census[i,"Whitepop2014"]/census[i,"TotalpopC2014"]
  census[i,"Blackpop2014"]<-my.data$B02001_003E
  census[i,"pct_black2014"]<-census[i,"Blackpop2014"]/census[i,"TotalpopC2014"]
  census[i,"AmericanIndian_AlaskaNativepop2014"]<-my.data$B02001_004E
  census[i,"Asianpop2014"]<-my.data$B02001_005E
  census[i,"NativeHawaiian_PacificIslanderpop2014"]<-my.data$B02001_006E
  census[i,"Otherpop2014"]<-my.data$B02001_007E
  
  # 2015
  my.data <-read.csv(census[i,"S0101_url_2015"], header = T)
  census[i,"TotalpopA2015"]<-my.data$S0101_C01_001E
  census[i,"Malepop2015"]<-my.data$S0101_C02_001E
  census[i,"pctmale2015"]<-census[i,"Malepop2015"]/census[i,"TotalpopA2015"]
  census[i,"Femalepop2015"]<-my.data$S0101_C03_001E
  census[i,"Medianage2015"]<-my.data$S0101_C01_032E
  
  my.data <-read.csv(census[i,"B01001_url_2015"], header = T)
  census[i,"TotalpopB2015"]<-my.data$B01001_001E
  census[i,"Age60andover2015"] <- (my.data$B01001_018E+my.data$B01001_019E+my.data$B01001_020E+
                                     my.data$B01001_021E+my.data$B01001_022E+my.data$B01001_023E+
                                     my.data$B01001_024E+my.data$B01001_025E+
                                     my.data$B01001_042E+my.data$B01001_043E+my.data$B01001_044E+
                                     my.data$B01001_045E+my.data$B01001_046E+my.data$B01001_047E+
                                     my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct60andover2015"] <-census[i,"Age60andover2015"]/census[i,"TotalpopB2015"]
  census[i,"Age65andover2015"] <-(my.data$B01001_020E+my.data$B01001_021E+my.data$B01001_022E+
                                    my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_044E+my.data$B01001_045E+my.data$B01001_046E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct65andover2015"] <-census[i,"Age65andover2015"]/census[i,"TotalpopB2015"]
  census[i,"Age75andover2015"] <-(my.data$B01001_023E+my.data$B01001_024E+my.data$B01001_025E+
                                    my.data$B01001_047E+my.data$B01001_048E+my.data$B01001_049E)
  census[i,"pct75andover2015"] <-census[i,"Age75andover2015"]/census[i,"TotalpopB2015"]
  
  my.data <-read.csv(census[i,"B02001_url_2015"], header = T)
  census[i,"TotalpopC2015"]<-my.data$B02001_001E
  census[i,"Whitepop2015"]<-my.data$B02001_002E
  census[i,"pct_white2015"]<-census[i,"Whitepop2015"]/census[i,"TotalpopC2015"]
  census[i,"Blackpop2015"]<-my.data$B02001_003E
  census[i,"pct_black2015"]<-census[i,"Blackpop2015"]/census[i,"TotalpopC2015"]
  census[i,"AmericanIndian_AlaskaNativepop2015"]<-my.data$B02001_004E
  census[i,"Asianpop2015"]<-my.data$B02001_005E
  census[i,"NativeHawaiian_PacificIslanderpop2015"]<-my.data$B02001_006E
  census[i,"Otherpop2015"]<-my.data$B02001_007E
}  

# Export  
  write.csv(census,"census_finish.csv", row.names = FALSE)