library(tidyverse)
library(rprojroot)
library(qs)

# Call NDIS data folders
dataFolders <- list.dirs(path = "/Users/sebastianroth/Desktop/Data/ndis", full.names = TRUE, recursive = FALSE)

# Grab files for: Active providers' data
path <- dataFolders[1]

dataFiles <- list.files(path, pattern = "*.csv", recursive = TRUE)

# Function for reading multiple CSVs at once 
read_csv_files <- function(x){
  df <- tryCatch({read.csv(paste(path, x, sep = "/"), na.strings = c(""))}, error = function(e){})
  return(df)
}

# Create data list 
dataList <- lapply(dataFiles, read_csv_files)

# Create dataset
data <- dataList %>%
  bind_rows() %>%
  mutate(ReportingDate = ifelse(RprtDt=="30-Sep-19", "2019-09-30",
                                ifelse(RprtDt=="30-Sep-22", "2022-09-30",
                                       ifelse(RprtDt=="30JUN2020", "2020-06-30",
                                              ifelse(RprtDt=="30Jun2021", "2021-06-30",
                                                     ifelse(RprtDt=="30JUN2022", "2022-06-30",
                                                            ifelse(RprtDt=="30SEP2020", "2020-09-30",
                                                                   ifelse(RprtDt=="30Sep2021", "2021-09-30",
                                                                          ifelse(RprtDt=="31-Dec-19", "2019-12-31",
                                                                                 ifelse(RprtDt=="31-Dec-21", "2021-12-31",
                                                                                        ifelse(RprtDt=="31-Mar-22", "2022-03-31",
                                                                                               ifelse(RprtDt=="31DEC2020", "2020-12-31",
                                                                                                      ifelse(RprtDt=="31DEC2022", "2022-12-31",
                                                                                                             ifelse(RprtDt=="31MAR2020", "2020-03-31",
                                                                                                                    ifelse(RprtDt=="31MAR2021", "2021-03-31",
                                                                                                                           ifelse(RprtDt=="31MAR2023", "2023-03-31", NA))))))))))))))),
         State = ifelse(is.na(statecd), StateCd, statecd),
         ServiceDistrict = SrvcDstrctNm,
         DisabilityGroup = DsbltyGrpNm,
         AgeBand = AgeBnd,
         SupportClass = SuppClass,
         ActiveIn = ActvIn,
         ProviderCount = PrvdrCnt) %>%
  select(ReportingDate,
         State,
         ServiceDistrict,
         DisabilityGroup,
         AgeBand,
         SupportClass,
         ActiveIn,
         ProviderCount) %>%
  distinct()

root <- rprojroot::is_rstudio_project

dataDir <- root$find_file("data")

# Dataset creation
qsave(data, paste(dataDir, "active-providers-dataset.qs", sep = "/"), preset = "fast")
