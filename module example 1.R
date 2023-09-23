# a light example of reading data 

# Method 1 ----------------------------------------------------------------

library(tidyr)
library(dplyr)


u5mr = "C:/Users/lyhel/Dropbox/UN IGME data/2020 Round Estimation/Code/output/GR20200214_all/Results.csv"
imr  = "C:/Users/lyhel/Dropbox/UN IGME Data/2020 Round Estimation/Code/output/IMR20200219_all/Results.csv"
nmr  = "C:/Users/lyhel/Dropbox/UN IGME data/2020 Round Estimation/Code/output/NMR_forDeathCalculation/Results_NMR_2020-08-20.csv"

# read data 
df_u5  <- read.csv(u5mr, stringsAsFactors = FALSE)
dim(df_u5)
df_u5[1,]
df_imr <- read.csv(imr , stringsAsFactors = FALSE)
df_nmr <- read.csv(nmr , stringsAsFactors = FALSE)

year.end <- "X2018.5"
year.start <- "X1931.5"

df_u5_2 <- df_u5 %>% select(c("Country.Name","ISO.Code","Indicator","Quantile",
                              paste0("X",as.numeric(gsub("X","",year.start)):as.numeric(gsub("X","",year.end))))) %>% 
  gather(year, val, paste0("X",as.numeric(gsub("X","",year.start)):as.numeric(gsub("X","",year.end)))) %>% 
  mutate(Year=as.numeric(gsub("X","",year)))
df_u5_2$Shortind  <- "U5MR"
df_u5_2$Indicator <- "Under-five Mortality Rate"

df_imr_2 <- df_imr %>% select(c("Country.Name","ISO.Code","Indicator","Quantile",
                                paste0("X",as.numeric(gsub("X","",year.start)):as.numeric(gsub("X","",year.end))))) %>% 
  gather(year, val, paste0("X",as.numeric(gsub("X","",year.start)):as.numeric(gsub("X","",year.end)))) %>% 
  mutate(Year=as.numeric(gsub("X","",year)))
df_imr_2$Shortind  <- "IMR"
df_imr_2$Indicator <- "Infant Mortality Rate"

df_nmr_2 <- df_nmr %>% select(c("Country.Name","ISO.Code","Indicator","Quantile",
                                paste0("X",as.numeric(gsub("X","",year.start)):as.numeric(gsub("X","",year.end))))) %>% 
  gather(year, val, paste0("X",as.numeric(gsub("X","",year.start)):as.numeric(gsub("X","",year.end)))) %>% 
  mutate(Year=as.numeric(gsub("X","",year)))
df_nmr_2$Shortind  <- "NMR"
df_nmr_2$Indicator <- "Neonatal Mortality Rate"

# combine the three dataset
data_all <- rbind(df_u5_2, df_imr_2, df_nmr_2)
table(data_all$Shortind, data_all$Year)


# Method 2 ---------------------------------------------------------------
library(tidyr)
library(dplyr)


dir_u5mr = "C:/Users/lyhel/Dropbox/UN IGME data/2020 Round Estimation/Code/output/GR20200214_all/Results.csv"
dir_imr  = "C:/Users/lyhel/Dropbox/UN IGME Data/2020 Round Estimation/Code/output/IMR20200219_all/Results.csv"
dir_nmr  = "C:/Users/lyhel/Dropbox/UN IGME data/2020 Round Estimation/Code/output/NMR_forDeathCalculation/Results_NMR_2020-08-20.csv"

read.data <- function(filename, year.start = "X1931.5", year.end = "X2018.5", ind) {
  dta <- read.csv(filename, stringsAsFactors = F)
  dta <- dta[, c("Country.Name","ISO.Code","Indicator","Quantile",paste0("X",as.numeric(gsub("X","",year.start)):as.numeric(gsub("X","",year.end))))]
  dta1 <- gather(dta, year, val, year.start:year.end) %>%
    mutate(Year = as.numeric(gsub("X","",year)))
  dta1$Shortind <- ind
  dta1$Indicator <- dplyr::recode(ind, 
                                  "U5MR" = "Under-five Mortality Rate",
                                  "IMR"  = "Infant Mortality Rate",
                                  "NMR"  = "Neonatal Mortality Rate")
  return(dta1)
}
df_u5_2  <- read.data(filename = dir_u5mr, ind = "U5MR")
df_imr_2 <- read.data(filename = dir_imr, ind = "IMR")
df_nmr_2 <- read.data(filename = dir_nmr, ind = "NMR")

# combine the three dataset
data_all <- rbind(df_u5_2, df_imr_2, df_nmr_2)
table(data_all$Shortind, data_all$Year)



# Method 3 ----------------------------------------------------------------
library("CME.assistant")
file_dirs <- list(
  dir_u5mr = "C:/Users/lyhel/Dropbox/UN IGME data/2020 Round Estimation/Code/output/GR20200214_all/Results.csv",
  dir_imr  = "C:/Users/lyhel/Dropbox/UN IGME Data/2020 Round Estimation/Code/output/IMR20200219_all/Results.csv",
  dir_nmr  = "C:/Users/lyhel/Dropbox/UN IGME data/2020 Round Estimation/Code/output/NMR_forDeathCalculation/Results_NMR_2020-08-20.csv"
)
data_all <- rbindlist(lapply(file_dirs, read.results.csv))
data_all[, table(Shortind, Year)]
