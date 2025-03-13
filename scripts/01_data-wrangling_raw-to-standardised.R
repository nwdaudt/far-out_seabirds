## 
## Raw data to standardised format
##
## -------------------------------------------------------------------------- ##
## -------------------------------------------------------------------------- ##

## Libraries ####
library(readr)
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(stringr)
library(janitor)
library(sf)

## Read data raw ####

df_FarOut <-
  dir(path = "./data-raw/", pattern = "*.csv", ignore.case = TRUE, full.names = TRUE) %>% 
  purrr::map_df(~readr::read_csv(., col_types = readr::cols(.default = "c"))) %>%
  janitor::clean_names()

df_FarOut <- dplyr::rename(df_FarOut, hour = time)

### As per **15/01/2021**, the .csv output template from CyberTracker changed;
### thus, I needed to wrangle different "datasets" to standardise columns, etc.

## Although the template changed in 15-Jan-2021, I had a (manual) checklist of 
## cells to correct util the end of the Summer 2021 trip (23-Jan-2021); 
## so, I've done the following steps:
##
## 1 - I filtered up to Jan-2021 trip ('1st dataset') and verified the info (see below); 
## 2 - I did the wrangling on the '2nd dataset' (trips after Jan-2021)
## *****************************************************************************

### 1st data set ####

### ****************************************************************************
### 1st data set, until 23/01/2021 *********************************************
### ****************************************************************************

df1_FarOut <- 
  df_FarOut %>% 
  dplyr::select(date, hour, lat, lon, 
                swell, bf, home_screen, 
                seabirds, albatross, mollymawk, shearwater, petrel, 
                storm_diving_petrel, prion, gull, tern, 
                australasian_gannet, skua, penguin, other_seabird, 
                count, seabird_note, note)

## Set up right column classes
# Date and time
df1_FarOut$date_time <- lubridate::dmy_hms(paste(df1_FarOut$date, df1_FarOut$hour))
df1_FarOut$date <- lubridate::dmy(df1_FarOut$date)
df1_FarOut$hour <- lubridate::hms(df1_FarOut$hour)

##
## Filter dates BEFORE 23/01/2021 **********************************************
## This date represents the end of the trip, although my notes regarding data set
## adjustments were until the 15/01/2021, as mentioned above
df1_FarOut <- dplyr::filter(df1_FarOut, date <= "2021-01-23") 

# Factor
factor_cols <- c("swell", "bf", "home_screen", "seabirds", "albatross", 
                 "mollymawk", "shearwater", "petrel", "storm_diving_petrel", 
                 "prion", "gull", "tern", "australasian_gannet", "skua", 
                 "penguin", "other_seabird")
df1_FarOut[factor_cols] <- lapply(df1_FarOut[factor_cols], as.factor)

# Numeric
numeric_cols <- c("lat", "lon", "count")
df1_FarOut[numeric_cols] <- lapply(df1_FarOut[numeric_cols], as.numeric)

rm("factor_cols", "numeric_cols")

## Fill conditions (swell & bf) for the whole 'df'
df1_FarOut <- 
  df1_FarOut %>% 
  tidyr::fill(swell, .direction = "updown") %>% 
  tidyr::fill(bf, .direction = "updown")

## Filter just *seabird* information and Conditions, and drop unused levels
df1_FarOut <- 
  df1_FarOut %>% 
  dplyr::filter(home_screen == "Conditions" | 
                  home_screen == "Note" | 
                  home_screen == "Seabird START" | 
                  home_screen == "Seabird END" | 
                  home_screen == "Seabird count") %>% 
  droplevels(.)

## Create an ID number for each seabird count ('id'), 
## which is between (including) every 'Seabird START' and 'Seabird END' from
## 'home_screen' variable
df1_FarOut <- 
  df1_FarOut %>% 
  dplyr::mutate(id = ifelse(home_screen == "Seabird START", seq(1:n()), NA)) %>% 
  tidyr::fill(id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Note" | home_screen == "Conditions", 
                            NA, id)) %>% 
  dplyr::relocate(id, .before = home_screen)

## The raw data set contains some errors. Next, I fix them --

## Double-check notes to fix tabel at once
notes_check <-
  df1_FarOut %>% 
  dplyr::filter(home_screen == "Note") %>%
  dplyr::select(date_time, note, seabird_note)

## Delete wrong data inputs (e.g. double 'Seabird START/END', no 'Seabird START/END'...);
## input (add) new rows (e.g. 'Seabird START/END'); and modify some cells.

## Delete
df1_FarOut <- 
  df1_FarOut %>% 
  dplyr::filter(!c(id == 74 & date == "2019-11-15" & hour == "13H 27M 29S" & home_screen == "Seabird count"),
                !c(id == 108 & date == "2019-11-16" & hour == "8H 8M 30S" & home_screen == "Seabird END"),
                !c(date == "2019-11-16" & hour == "8H 9M 2S" & home_screen == "Note"),
                !c(id == 117 & date == "2019-11-16" & hour == "9H 25M 56S" & home_screen == "Seabird END"),
                !c(id == 140 & date == "2019-11-16" & hour == "11H 27M 48S" & home_screen == "Seabird END"),
                !c(id == 155 & date == "2019-11-16" & hour == "12H 43M 20S" & home_screen == "Seabird START"),
                !c(id == 155 & date == "2019-11-16" & hour == "12H 45M 24S" & home_screen == "Seabird count"),
                !c(id == 155 & date == "2019-11-16" & hour == "12H 46M 21S" & home_screen == "Seabird END"),
                !c(date == "2019-11-16" & hour == "12H 47M 28S" & home_screen == "Note"),
                !c(id == 353 & date == "2020-01-27" & hour == "10H 8M 27S" & home_screen == "Seabird START"),
                !c(id == 577 & date == "2020-01-28" & hour == "16H 28M 3S" & home_screen == "Seabird START"),
                !c(id == 823 & date == "2020-02-02" & hour == "8H 44M 37S" & home_screen == "Seabird START"),
                !c(date == "2020-02-02" & hour == "8H 45M 24S" & home_screen == "Note"),
                !c(id == 1773 & date == "2021-01-22" & hour == "9H 1M 23S" & home_screen == "Seabird START"),
                !c(id == 1889 & date == "2021-01-23" & hour == "13H 22M 6S" & home_screen == "Seabird START"))

## Modify
# Transform to character temporarily
df1_FarOut$home_screen <- as.character(df1_FarOut$home_screen)
df1_FarOut$hour <- as.character(df1_FarOut$hour)

# *Forgotten seabird count number
df1_FarOut$count[df1_FarOut$date == "2021-01-11" & df1_FarOut$id == "1297" & df1_FarOut$home_screen == "Seabird count"] <- 1

# *Forgotten 'Seabird END', that was logged a couple of minutes after; 
# push it to the same info as last seabird count.
df1_FarOut$hour[df1_FarOut$date == "2021-01-15" & df1_FarOut$id == "1625" & df1_FarOut$home_screen == "Seabird END"] <- "19H 45M 2S"
df1_FarOut$lat[df1_FarOut$date == "2021-01-15" & df1_FarOut$id == "1625" & df1_FarOut$home_screen == "Seabird END"] <- -34.43981
df1_FarOut$lon[df1_FarOut$date == "2021-01-15" & df1_FarOut$id == "1625" & df1_FarOut$home_screen == "Seabird END"] <- 173.4811

# As changed an 'hour' cell above, need to correct the 'date_time' column accordingly
df1_FarOut$date_time[df1_FarOut$id == "1625" & df1_FarOut$home_screen == "Seabird END"] <-
  lubridate::ymd_hms("2021-01-15 19H 45M 2S")

# Back to factor / period
df1_FarOut$home_screen <- as.factor(df1_FarOut$home_screen)
df1_FarOut$hour <- lubridate::hms(df1_FarOut$hour)

## Input
df_input <- data.frame(
  date = lubridate::ymd(c("2019-11-16", "2020-02-03", "2020-02-03", 
                          "2021-01-12", 
                          "2021-01-15", "2021-01-15", 
                          "2021-01-15", "2021-01-16", 
                          "2021-01-16", "2021-01-22")),
  hour = lubridate::hms(c("9H 8M 56S", "8H 49M 53S", "9H 36M 13S", 
                          "10H 21M 10S", 
                          "12H 41M 49S", "13H 17M 13S", 
                          "17H 32M 15S", "10H 05M 33S", 
                          "10H 58M 16S", "12H 53M 30S")),
  date_time = lubridate::ymd_hms(c("2019-11-16 9H 8M 56S", "2020-02-03 8H 49M 53S", "2020-02-03 9H 36M 13S",
                                   "2021-01-12 10H 21M 10S", 
                                   "2021-01-15 12H 41M 49S", "2021-01-15 13H 17M 13S",
                                   "2021-01-15 17H 32M 15S", "2021-01-16 10H 05M 33S", 
                                   "2021-01-16 10H 58M 16S", "2021-01-22 12H 53M 30S")),
  lat = as.numeric(c(-34.10391, -35.07099, -35.03732, 
                     -34.42122, 
                     -34.24153, -34.26678, 
                     -34.43571, -34.25881,
                     -34.31432, -34.90925)),
  lon = as.numeric(c(174.1118, 175.1295, 175.1657, 
                     173.3327, 
                     173.3656, 173.35281, 
                     173.2771, 174.10574,
                     174.1179, 175.1355)),
  home_screen = as.factor(c("Seabird START", "Seabird END", "Seabird END",
                            "Seabird END", 
                            "Seabird END", "Seabird START", 
                            "Seabird END", "Seabird START", 
                            "Seabird END", "Seabird START"))
)

df1_FarOut <- 
  dplyr::bind_rows(df_input, df1_FarOut) %>% 
  dplyr::arrange(date, hour)

rm("df_input", "notes_check")

## Need to delete and create the 'id' column again, with data set fixed
df1_FarOut <- 
  df1_FarOut %>% 
  dplyr::select(- id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Seabird START", seq(1:n()), NA)) %>% 
  tidyr::fill(id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Note" | home_screen == "Conditions", 
                            NA, id)) %>% 
  dplyr::relocate(id, .before = home_screen)

## Create an object to check duration of each count; then merge it with the main data set

id_count_duration <- 
  df1_FarOut %>% 
  dplyr::select(id, home_screen, date_time) %>% 
  dplyr::filter(home_screen == "Seabird START" | home_screen == "Seabird END") %>% 
  tidyr::pivot_wider(names_from = home_screen, values_from = date_time) %>% 
  dplyr::rename(seabird_start = "Seabird START", 
                seabird_end = "Seabird END") %>% 
  dplyr::mutate(id_count_duration = seabird_end - seabird_start) %>% # unit == 'min'
  dplyr::select(id, id_count_duration)

df1_FarOut <- dplyr::left_join(df1_FarOut, id_count_duration, by = "id")

rm("id_count_duration")

### 2nd data set ####

### ****************************************************************************
### 2nd data set, after 12/01/2021 *********************************************
### ****************************************************************************

df2_FarOut <- 
  df_FarOut %>% 
  dplyr::select(date, hour, lat, lon, 
                swell, bf, home_screen, 
                seabirds, seabird_group, albatross, mollymawk, shearwater, petrel, 
                storm_diving_petrel, prion, gull, tern, 
                australasian_gannet, skua, penguin, other_seabird, 
                count, sb_count, seabird_note, note)

## The 'hour' column from the last leg of voyage 9 (20/11/2023 to 23/11/2023) -------------#
# and voyage 10 (04/05/2024 to 06/05/2024) has a different format. 
# So we need to deal with it right now to not have problems later on

# Subset and deal with 'hour' format
df2_v9_2ndleg_v10 <- 
  df2_FarOut[5539:6295, ] %>% 
  dplyr::mutate(hour = 
                  stringr::str_sub(
                    as.character(lubridate::parse_date_time(hour, '%I:%M:%S %p'))
                    , start = 9), 
                .after = date)

# Subset main dataset and `rbind` back the corrected-format `df2_v9_2ndleg_v10`
df2_FarOut <- 
  rbind(df2_FarOut[1:5538,],
        df2_v9_2ndleg_v10)

rm("df2_v9_2ndleg_v10")
## ---------------------------------------------------------------------------------------#

## Set up right column classes
# Date and time
df2_FarOut$date_time <- lubridate::dmy_hms(paste(df2_FarOut$date, df2_FarOut$hour))
df2_FarOut$date <- lubridate::dmy(df2_FarOut$date)
df2_FarOut$hour <- lubridate::hms(df2_FarOut$hour)

##
## Filter dates AFTER 12/01/2021 ***********************************************
##
df2_FarOut <- dplyr::filter(df2_FarOut, date > "2021-01-12")

# Factor
factor_cols <- c("swell", "bf", "home_screen", "seabirds", "seabird_group", 
                 "albatross", "mollymawk", "shearwater", "petrel", 
                 "storm_diving_petrel", "prion", "gull", "tern", 
                 "australasian_gannet", "skua", "penguin", "other_seabird")
df2_FarOut[factor_cols] <- lapply(df2_FarOut[factor_cols], as.factor)

# Numeric
numeric_cols <- c("lat", "lon", "count", "sb_count")
df2_FarOut[numeric_cols] <- lapply(df2_FarOut[numeric_cols], as.numeric)

rm("factor_cols", "numeric_cols")

## Fill conditions (swell & bf) for the whole 'df'
df2_FarOut <- 
  df2_FarOut %>% 
  tidyr::fill(swell, .direction = "updown") %>% 
  tidyr::fill(bf, .direction = "updown")

## Filter just *seabird* information and Conditions, and drop unused levels
df2_FarOut <- 
  df2_FarOut %>% 
  dplyr::filter(home_screen == "Conditions" | 
                  home_screen == "Note" | 
                  home_screen == "Seabird START" | 
                  home_screen == "Seabird END" | 
                  home_screen == "Seabird count") %>% 
  droplevels(.)

## Create an ID number for each seabird count ('id'), 
## which is between (including) every 'Seabird START' and 'Seabird END' from
## 'home_screen' variable
df2_FarOut <- 
  df2_FarOut %>% 
  dplyr::arrange(date_time) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Seabird START", seq(1:n()), NA)) %>% 
  tidyr::fill(id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Note" | home_screen == "Conditions", 
                            NA, id)) %>%
  dplyr::relocate(id, .before = home_screen)

id_count_duration <- 
  df2_FarOut %>% 
  dplyr::select(id, home_screen, date_time) %>% 
  dplyr::filter(home_screen == "Seabird START" | home_screen == "Seabird END")

## Check if there are "id"s with more/less than 2 occurrences
# (they all should have two -- a START and an END)
tmp <- plyr::count(id_count_duration$id)
tmp <- dplyr::filter(tmp, ! freq == 2)

## Check notes to double-check info to correct at once
notes_check2 <-
  df2_FarOut %>% 
  dplyr::filter(home_screen == "Note") %>%
  dplyr::select(date_time, note, seabird_note)

## Delete duplicated/wrong "Seabird START/END"
df2_FarOut <- 
  df2_FarOut %>% 
  dplyr::filter(# duplicated START, delete one
                !c(date == "2021-01-22" & hour == "9H 1M 23S"),
                # cancel START (see notes)
                !c(date == "2021-01-23" & hour == "13H 22M 6S"),
                # triple START, so delete the first two
                !c(date == "2021-07-14" & hour == "10H 18M 6S"),
                !c(date == "2021-07-14" & hour == "10H 18M 32S"),
                # cancel this next count, no info on possible END time
                !c(date == "2021-07-15" & hour == "12H 41M 12S"),
                !c(date == "2021-07-15" & hour == "12H 41M 57S"),
                !c(date == "2021-07-15" & hour == "12H 45M 0S"),
                # duplicated END, delete second
                !c(date == "2022-01-20" & hour == "8H 54M 5S"),
                # duplicated START, delete first
                !c(date == "2022-01-23" & hour == "15H 52M 56S"),
                # duplicated START, delete first
                !c(date == "2022-11-14" & hour == "16H 15M 11S"),
                # cancel this next count, no info on possible END time
                !c(date == "2023-11-23" & hour == "15H 58M 11S"),
                !c(date == "2023-11-23" & hour == "16H 0M 40S"))

## Modify 
# (Transform to character temporarily)
df2_FarOut$home_screen <- as.character(df2_FarOut$home_screen)
df2_FarOut$hour <- as.character(df2_FarOut$hour)

# Modify a 'Seabird END' that should be 'START'
df2_FarOut$home_screen[df2_FarOut$date == "2023-01-22" & df2_FarOut$hour == "10H 8M 8S"] <- "Seabird START"

# Modify 'Seabird START' that should be 'END'
df2_FarOut$home_screen[df2_FarOut$date == "2023-11-23" & df2_FarOut$hour == "10H 39M 24S"] <- "Seabird END"
df2_FarOut$home_screen[df2_FarOut$date == "2023-11-23" & df2_FarOut$hour == "14H 23M 36S"] <- "Seabird END"

# *Forgotten 'Seabird END', that was logged a couple of minutes after; 
# push it to the same info as last seabird count (according to note at 19:50:43).
df2_FarOut$hour[df2_FarOut$date == "2021-01-15" & df2_FarOut$hour == "19H 49M 22S" & df2_FarOut$home_screen == "Seabird END"] <- "19H 45M 2S"
df2_FarOut$lat[df2_FarOut$date == "2021-01-15" & df2_FarOut$hour == "19H 45M 2S" & df2_FarOut$home_screen == "Seabird END"] <- -34.43981
df2_FarOut$lon[df2_FarOut$date == "2021-01-15" & df2_FarOut$hour == "19H 45M 2S" & df2_FarOut$home_screen == "Seabird END"] <- 173.4811

# As changed an 'hour' cell above, need to correct the 'date_time' column accordingly
df2_FarOut$date_time[df2_FarOut$date == "2021-01-15" & df2_FarOut$hour == "19H 45M 2S"] <-
  lubridate::ymd_hms("2021-01-15 19H 45M 2S")

# (Back to factor / period)
df2_FarOut$home_screen <- as.factor(df2_FarOut$home_screen)
df2_FarOut$hour <- lubridate::hms(df2_FarOut$hour)
    ## A few cells fail to parse in `lubridate::hms(df2_FarOut$hour)`, 
    ## but info is saved under the 'date_time' column

# *Forgotten seabird count number (according to note at 17:47:44)
df2_FarOut$sb_count[df2_FarOut$date == "2021-07-15" & df2_FarOut$hour == "17H 46M 46S" & 
                      df2_FarOut$home_screen == "Seabird count"] <- 1

## Input missing "Seabird START/END"
df_input <- data.frame(
  date = lubridate::ymd(c("2021-01-15", "2021-01-15", 
                          "2021-01-15", "2021-01-16",
                          "2021-01-16", "2021-01-22",
                          "2022-11-13", "2022-11-14",
                          "2022-11-14", "2022-11-15",
                          "2023-01-22", "2023-11-15",
                          "2023-11-16", "2023-11-21",
                          "2024-05-04", "2024-05-04", "2024-05-04",
                          "2024-05-05", "2024-05-05")),
  hour = lubridate::hms(c("12H 41M 49S", "13H 17M 13S", 
                          "17H 32M 15S", "10H 05M 33S",
                          "10H 58M 16S", "12H 53M 30S",
                          "13H 45M 45S", "11H 12M 50S",
                          "13H 10M 25S", "7H 37M 56S",
                          "11H 40M 24S", "8H 36M 4S",
                          "7H 34M 56S", "13H 29M 47S",
                          "8H 18M 25S", "13H 14M 26S", "15H 6M 11S",
                          "14H 21M 16S", "14H 41M 31S")),
  date_time = lubridate::ymd_hms(c("2021-01-15 12H 41M 49S", "2021-01-15 13H 17M 13S", 
                                   "2021-01-15 17H 32M 15S", "2021-01-16 10H 05M 33S",
                                   "2021-01-16 10H 58M 16S", "2021-01-22 12H 53M 30S",
                                   "2022-11-13 13H 45M 45S", "2022-11-14 11H 12M 50S",
                                   "2022-11-14 13H 10M 25S", "2022-11-15 7H 37M 56S",
                                   "2023-01-22 11H 40M 24S", "2023-11-15 8H 36M 4S",
                                   "2023-11-16 7H 34M 56S", "2023-11-21 13H 29M 47S",
                                   "2024-05-04 8H 18M 25S", "2024-05-04 13H 14M 26S", "2024-05-04 15H 6M 11S",
                                   "2024-05-05 14H 21M 16S", "2024-05-05 14H 41M 31S")),
  lat = as.numeric(c(-34.24153, -34.26678, 
                     -34.43571, -34.25881,
                     -34.31432, -34.90925,
                     -34.42759, -34.46566,
                     -34.32663, -34.34787,
                     -34.40585, -34.63979,
                     -34.54437, -34.42587,
                     -34.63715, -34.45727, -34.48590,
                     -34.40364, -34.43157)),
  lon = as.numeric(c(173.3656, 173.35281, 
                     173.2771, 174.10574,
                     174.1179, 175.1355,
                     173.6422, 173.4513,
                     173.5155, 173.1439,
                     173.2921, 173.5695,
                     173.4244, 173.6320,
                     173.5712, 173.5765, 173.5315,
                     173.3684, 173.3343)),
  home_screen = as.factor(c("Seabird END", "Seabird START", 
                            "Seabird END", "Seabird START",
                            "Seabird END", "Seabird START",
                            "Seabird END", "Seabird START",
                            "Seabird END", "Seabird END",
                            "Seabird START", "Seabird START",
                            "Seabird START", "Seabird START",
                            "Seabird START", "Seabird END", "Seabird END",
                            "Seabird END", "Seabird END")),
  note = c("forgot to end seabirds", "forgot to start seabird", 
           "forgot to press seabird END", "effort ON - forgot to start seabirds",
           "dolphin sighting - forgot to press seabird END", "forgot to start seabirds, 10mins before the end",
           "nwd - input END by hand", "nwd - input START by hand",
           "nwd - input END by hand", "nwd - input END by hand",
           "nwd - input START by hand", "nwd - input START by hand",
           "nwd - input START by hand", "nwd - input START by hand",
           "nwd - input START by hand", "nwd - input END by hand", "nwd - input END by hand",
           "nwd - input END by hand", "nwd - input END by hand")
)

df2_FarOut <- 
  dplyr::bind_rows(df_input, df2_FarOut) %>% 
  dplyr::arrange(date, hour)

rm("df_input", "tmp", "notes_check2")

## Run column 'id' and 'id_count_duration' again
## Need to delete and create the 'id' column again, with data set fixed
df2_FarOut <- 
  df2_FarOut %>% 
  dplyr::select(- id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Seabird START", seq(1:n()), NA)) %>% 
  tidyr::fill(id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Note" | home_screen == "Conditions", 
                            NA, id)) %>% 
  dplyr::relocate(id, .before = home_screen)

## Check if there are "id"s with more/less than 2 occurrences -- AGAIN
# (they all should have two -- a START and an END)
# 
# id_count_duration <-
#   df2_FarOut %>%
#   dplyr::select(id, home_screen, date_time) %>%
#   dplyr::filter(home_screen == "Seabird START" | home_screen == "Seabird END")
# 
# tmp <- plyr::count(id_count_duration$id)
# tmp <- dplyr::filter(tmp, ! freq == 2)       ## ------- OK !!
# 
# rm("id_count_duration", "tmp")

## Create an object to check duration of each count; then merge it with the main data set
id_count_duration <- 
  df2_FarOut %>% 
  dplyr::select(id, home_screen, date_time) %>% 
  dplyr::arrange(date_time) %>% 
  dplyr::filter(home_screen == "Seabird START" | home_screen == "Seabird END") %>% 
  tidyr::pivot_wider(names_from = home_screen, values_from = date_time) %>% 
  dplyr::rename(seabird_start = "Seabird START", 
                seabird_end = "Seabird END") %>% 
  dplyr::mutate(id_count_duration = (seabird_end - seabird_start)) %>% 
  dplyr::select(id, id_count_duration)

# Not sure why but here R is getting the difftime from about as 'secs', 
# so force it to be 'mins'
units(id_count_duration$id_count_duration) <- "mins"

df2_FarOut <- dplyr::left_join(df2_FarOut, id_count_duration, by = "id")

rm("id_count_duration")

### Merge data sets ('df1' and 'df2') & coalesce columns ####

### Filter dates earlier than 2021-01-15 from 'df1', then merge both 'dfs' *****
df_FarOut_tidy <- 
  df1_FarOut %>% 
  dplyr::filter(date < "2021-01-15") %>%
  dplyr::full_join(., df2_FarOut)

### 'Coalesce' columns that represent the same data but under different column names

# Seabird groups ('_gr')
vars_to_coalesce_gr <- c("seabirds", "seabird_group")

df_FarOut_tidy$seabird_gr <- 
  apply(df_FarOut_tidy[colnames(df_FarOut_tidy) %in% c(vars_to_coalesce_gr)], 1, 
        function(x) as.factor(paste(x[!is.na(x)], collapse = ", ")))

# Seabird species ('_sp')
vars_to_coalesce_sp <- c("albatross", "mollymawk", "shearwater", "petrel", "penguin",
                         "storm_diving_petrel", "prion", "gull", "tern", "skua",
                         "australasian_gannet", "other_seabird")

df_FarOut_tidy$seabird_sp <- 
  apply(df_FarOut_tidy[colnames(df_FarOut_tidy) %in% c(vars_to_coalesce_sp)], 1, 
        function(x) as.factor(paste(x[!is.na(x)], collapse = ", ")))

# Seabird count ('_ct')
vars_to_coalesce_ct <- c("count", "sb_count")

df_FarOut_tidy$seabird_ct <- 
  apply(df_FarOut_tidy[colnames(df_FarOut_tidy) %in% c(vars_to_coalesce_ct)], 1, 
        function(x) as.numeric(paste(x[!is.na(x)], collapse = ", ")))

## Due to the different templates, I needed to do this trick 
# (only for the Australasian Gannet)
df_FarOut_tidy <- 
  df_FarOut_tidy %>%
  dplyr::mutate(seabird_ct = ifelse(seabird_gr == "Australasian gannet" & is.na(seabird_ct), 
                                    yes = as.numeric(as.character(seabird_sp)),
                                    no = seabird_ct))

# >> Warning message: ! NAs introduced by coercion 
# >> There are other species with NA in 'seabird_ct' (see next code section)

df_FarOut_tidy$seabird_sp[df_FarOut_tidy$seabird_gr == "Australasian gannet"] <- 
  as.factor("Australasian gannet")

### Then clean columns
df_FarOut_tidy <- 
  df_FarOut_tidy[, !(colnames(df_FarOut_tidy) %in% 
                       c(vars_to_coalesce_gr, 
                         vars_to_coalesce_sp, 
                         vars_to_coalesce_ct))]

rm("vars_to_coalesce_gr", "vars_to_coalesce_sp", "vars_to_coalesce_ct",
   "df1_FarOut", "df2_FarOut")

### Remove unused rows, check 'seabird_ct' and 'id' ####

## Keep only seabird-related data
df_FarOut_tidy <- 
  df_FarOut_tidy %>% 
  dplyr::filter(! home_screen == "Conditions",
                ! home_screen == "Note") %>%
  droplevels(.)

## I am assuming if people forgot to type the number of individuals, it was "1"
df_FarOut_tidy$seabird_ct[is.na(df_FarOut_tidy$seabird_ct)] <- 1

## Check it -- OK!
# tmp <-
#   df_FarOut_tidy %>%
#   dplyr::filter(home_screen == "Seabird count") %>%
#   dplyr::filter(is.na(seabird_ct))

## "id": delete and do it again (yes, I know...)
df_FarOut_tidy <-
  df_FarOut_tidy %>% 
  dplyr::select(- id) %>% 
  dplyr::mutate(id = ifelse(home_screen == "Seabird START", seq(1:n()), NA)) %>% 
  tidyr::fill(id) %>%
  dplyr::relocate(id, .before = home_screen)

## Check if there are "id"s with more/less than 2 occurrences
# (they all should have two -- a START and an END) # --------- OK!
# tmp <-
#   dplyr::filter(df_FarOut_tidy, home_screen == "Seabird START" | home_screen == "Seabird END")
# tmp <- plyr::count(tmp$id)
# tmp <- dplyr::filter(tmp, ! freq == 2)
# rm("tmp")

### Find the surveyed distance between START/END points for each 'id' ####
id_start <- 
  df_FarOut_tidy %>% 
  dplyr::select(id, home_screen, lat, lon) %>% 
  dplyr::filter(home_screen == "Seabird START") %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

id_end <- 
  df_FarOut_tidy %>% 
  dplyr::select(id, home_screen, lat, lon) %>% 
  dplyr::filter(home_screen == "Seabird END") %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

id_count_dist <- 
  as.data.frame(id_start) %>%
  dplyr::mutate(id_dist = as.numeric(
    sf::st_distance(id_start, id_end, by_element = TRUE))) %>% 
  dplyr::mutate(id_dist_km = id_dist / 1000) %>% ## units = km
  dplyr::select(id, id_dist_km)

df_FarOut_tidy <- 
  dplyr::left_join(df_FarOut_tidy, id_count_dist, by = "id") 

rm("id_start", "id_end", "id_count_dist")

### Re-order columns, create columns, and fix a few rows for the last time ####

## Re-order cols
df_FarOut_tidy <- 
  df_FarOut_tidy %>%
  dplyr::select(
    id, date_time, lat, lon, home_screen, 
    seabird_gr, seabird_sp, seabird_ct, 
    id_count_duration, id_dist_km, 
    swell, bf, date, hour, note, seabird_note
  )

## Create 'voyage' col

# Voyage 1: 2019-11-05 -- 2019-11-17
# Voyage 2: 2020-01-27 -- 2020-02-05
# Voyage 3: 2021-01-10 -- 2021-01-23
# Voyage 4: 2021-07-14 -- 2021-07-15
# Voyage 5: 2022-01-18 -- 2022-01-26
# Voyage 6: 2022-11-12 -- 2022-11-20
# Voyage 7: 2023-01-20 -- 2023-01-24
# Voyage 8: 2023-05-25 -- 2023-05-26
# Voyage 9: 2023-11-14 -- 2023-11-25
# Voyage 10: 2024-05-04 -- 2024-05-06

df_FarOut_tidy <- 
  df_FarOut_tidy %>%
  dplyr::mutate(voyage = dplyr::case_when(
    date <= "2019-11-17" ~ "01voyage",
    date >= "2020-01-27" & date <= "2020-02-05" ~ "02voyage",
    date >= "2021-01-10" & date <= "2021-01-23" ~ "03voyage",
    date >= "2021-07-14" & date <= "2021-07-15" ~ "04voyage", 
    date >= "2022-01-18" & date <= "2022-01-26" ~ "05voyage", 
    date >= "2022-11-12" & date <= "2022-11-20" ~ "06voyage", 
    date >= "2023-01-20" & date <= "2023-01-24" ~ "07voyage",
    date >= "2023-05-25" & date <= "2023-05-26" ~ "08voyage", 
    date >= "2023-11-14" & date <= "2023-11-25" ~ "09voyage",
    date >= "2024-05-04" & date <= "2024-05-06" ~ "10voyage"), 
    .before = home_screen)

## Create 'season' col
# Dec--Feb (summer); Mar--May (autumn); Jun--Aug (winter); Sep--Nov (spring)

df_FarOut_tidy <- 
  df_FarOut_tidy %>%
  dplyr::mutate(season = dplyr::case_when(
    voyage == "01voyage" ~ "spring",
    voyage == "02voyage" ~ "summer",
    voyage == "03voyage" ~ "summer",
    voyage == "04voyage" ~ "winter",
    voyage == "05voyage" ~ "summer",
    voyage == "06voyage" ~ "spring",
    voyage == "07voyage" ~ "summer",
    voyage == "08voyage" ~ "autumn",
    voyage == "09voyage" ~ "spring",
    voyage == "10voyage" ~ "autumn"), 
    .before = home_screen)

## A few more rows need to be deleted, according to notes (see 'tmp' below)
tmp <- 
  df_FarOut_tidy %>% 
  dplyr::filter(!is.na(note) | !is.na(seabird_note)) %>% 
  dplyr::select(id, date, hour, note, seabird_note)

# Delete rows
df_FarOut_tidy <- 
  df_FarOut_tidy %>%
  dplyr::filter(!c(id == 983 & hour == "8H 35M 57S"), # "delete this one"
                !c(id == 1153 & hour == "9H 38M 27S"), # "delete previous white faced sighting it was actually a white bellied"
                !c(id == 1963 & hour == "16H 13M 2S"), # "possible grey telnet, off effort"
                !c(id == 1963 & hour == "17H 13M 50S"), # "off effort"
                !c(id == 2054 & hour == "10H 38M 23S")) # "Delete this entry"

# Fix a seabird id in the species column (as.character -> fix -> as.factor)
df_FarOut_tidy$seabird_sp <- as.character(df_FarOut_tidy$seabird_sp)

df_FarOut_tidy$seabird_sp[df_FarOut_tidy$id == 136 & 
                            df_FarOut_tidy$hour == "19H 47M 20S"] <- "Campbell albatross"

df_FarOut_tidy$seabird_sp[df_FarOut_tidy$id == 2492 & 
                            df_FarOut_tidy$hour == "9H 40M 15S"] <- "White-capped albatross"

df_FarOut_tidy$seabird_sp <- as.factor(df_FarOut_tidy$seabird_sp)

## I've noticed a few 'home_screen == Seabird count' observations (= rows) have 
## empty info on 'seabird_gr' and 'seabird_sp' and no observations, so need to delete them

# Overwrite 'tmp' to find those obs:

# levels(df_FarOut_tidy$seabird_gr) # Check lvls
# levels(df_FarOut_tidy$seabird_sp) # Check lvls

tmp <- 
  df_FarOut_tidy %>% 
  dplyr::filter(home_screen == "Seabird count") %>% 
  dplyr::filter(seabird_gr == "" | seabird_sp == "")

df_FarOut_tidy <- 
  dplyr::anti_join(df_FarOut_tidy, tmp) %>% 
  droplevels(.)

rm("tmp")

## While running the code above, I've realised a few inconsistencies on how 
## we named 'unknown' taxa, so let's standardise them

# levels(df_FarOut_tidy$seabird_sp) # Check lvls

df_FarOut_tidy <- 
  df_FarOut_tidy %>% 
  dplyr::mutate(seabird_sp = as.factor(
                  dplyr::case_match(seabird_sp,
                                    "Unid. wandering albatross" ~ "Wandering albatross",
                                    "Unidentified mollymawk" ~ "Mollymawk sp",
                                    "Unidentified petrel" ~ "Petrel sp",
                                    .default = seabird_sp)))

## ...and that we have "Others ..." without any notes, too... (see 'tmp' below)
## seabird_sp == "Other" is a "Mollymawk sp" -- will change below.
## seabird_gr == "Other" with seabird_sp == "Other seabird" without any notes, don't give us any clue -- so, remove them.

tmp <- df_FarOut_tidy %>% dplyr::filter(seabird_sp == "Other" | seabird_sp == "Other seabird")

df_FarOut_tidy$seabird_sp[df_FarOut_tidy$id == 136 & 
                            df_FarOut_tidy$hour == "19H 46M 37S"] <- "Mollymawk sp"

tmp <- tmp[2:nrow(tmp), ] # remove the record corrected in the above line

df_FarOut_tidy <- 
  dplyr::anti_join(df_FarOut_tidy, tmp) %>% 
  droplevels(.)

rm("tmp")

## Also, let's call mollymawks as albatrosses in 'seabird_gr'

df_FarOut_tidy <- 
  df_FarOut_tidy %>% 
  dplyr::mutate(seabird_gr = ifelse(seabird_gr == "Mollymawk",
                                    yes = "Albatross", no = as.character(seabird_gr)))

## ... and change (fix!) "Grey petrel" to "Grey-faced petrel (Oi)" -- the former was wrongly ID'ed in the field and checked later
df_FarOut_tidy <- 
  df_FarOut_tidy %>% 
  dplyr::mutate(seabird_sp = ifelse(seabird_sp == "Grey petrel",
                                    yes = "Grey-faced petrel (Oi)", no = as.character(seabird_sp)))

### Lastly, let's create another 'group' column according to their feeding guilds/foraging behaviour
# Large procellariids (albatrosses, giant petrels);
# Medium procellariids (petrels, shearwaters);
# Small procellariids (storm petrels, diving-petrels, prions);
# Sulids (gannet);
# Larids (skuas, gulls);

df_FarOut_tidy <- 
  df_FarOut_tidy %>% 
  dplyr::mutate(seabird_gr_foraging = dplyr::case_when(
    seabird_sp == "Buller's shearwater" ~ "Medium procellariids",
    seabird_sp == "Fluttering/Hutton's shearwater" ~ "Medium procellariids",
    seabird_sp == "Flesh-footed shearwater" ~ "Medium procellariids",
    seabird_sp == "Sooty shearwater" ~ "Medium procellariids",
    seabird_sp == "Grey-faced petrel (Oi)" ~ "Medium procellariids",
    seabird_sp == "White-chinned petrel" ~ "Medium procellariids",
    seabird_sp == "Cook/Pycroft petrel" ~ "Medium procellariids",
    seabird_sp == "Northern giant petrel" ~ "Large procellariids",
    seabird_sp == "Black petrel" ~ "Medium procellariids",
    seabird_sp == "Wandering albatross" ~ "Large procellariids",
    seabird_sp == "Wilson's storm petrel" ~ "Small procellariids",
    seabird_sp == "Black-winged petrel" ~ "Medium procellariids",
    seabird_sp == "Australasian gannet" ~ "Sulids",
    seabird_sp == "Campbell albatross" ~ "Large procellariids",
    seabird_sp == "Little shearwater" ~ "Medium procellariids",
    seabird_sp == "White-faced storm petrel" ~ "Small procellariids",
    seabird_sp == "Black-bellied storm petrel" ~ "Small procellariids",
    seabird_sp == "Northern royal albatross" ~ "Large procellariids",
    seabird_sp == "White-bellied storm petrel" ~ "Small procellariids",
    seabird_sp == "Red-billed gull" ~ "Larids",
    seabird_sp == "Black-billed gull" ~ "Larids",
    seabird_sp == "Diving petrel" ~ "Small procellariids",
    seabird_sp == "NZ storm petrel" ~ "Small procellariids",
    seabird_sp == "Arctic skua" ~ "Larids",
    seabird_sp == "White-capped albatross" ~ "Large procellariids",
    seabird_sp == "Cape pigeon" ~ "Medium procellariids",
    seabird_sp == "Black-browed albatross" ~ "Large procellariids",
    seabird_sp == "Fairy prion" ~ "Small procellariids",
    seabird_sp == "Southern royal albatross" ~ "Large procellariids",
    seabird_sp == "Buller's albatross" ~ "Large procellariids",
    seabird_sp == "White-necked petrel" ~ "Medium procellariids",
    seabird_sp == "Brown skua" ~ "Larids",
    .default = NA
  ), .after = seabird_gr)

### And rename two species names
df_FarOut_tidy <- 
  df_FarOut_tidy %>% 
  dplyr::mutate(seabird_sp = ifelse(seabird_sp == "Grey-faced petrel (Oi)",
                                    yes = "Grey-faced petrel", no = as.character(seabird_sp))) %>% 
  dplyr::mutate(seabird_sp = ifelse(seabird_sp == "Fluttering/Hutton's shearwater",
                                    yes = "Fluttering shearwater", no = as.character(seabird_sp)))

### Add species mass to calculate biomass ####

# unique(df_FarOut_tidy$seabird_sp)

## Species mass according to
# H.A. Robertson & B.D. Heather (2015) The hand guide to the birds of New Zealand. Penguin Random House New Zealand

df_FarOut_tidy <- 
  df_FarOut_tidy %>% 
  dplyr::mutate(species_mass_kg = dplyr::case_when(
    seabird_sp == "Buller's shearwater" ~ 0.425,
    seabird_sp == "Fluttering shearwater" ~ 0.325, # Average between species
    seabird_sp == "Flesh-footed shearwater" ~ 0.6,
    seabird_sp == "Sooty shearwater" ~ 0.8,
    seabird_sp == "Grey-faced petrel" ~ 0.55,
    seabird_sp == "White-chinned petrel" ~ 1.25,
    seabird_sp == "Cook/Pycroft petrel" ~ 0.175, # Average between species
    seabird_sp == "Northern giant petrel" ~ 4.5,
    seabird_sp == "Black petrel" ~ 0.7,
    seabird_sp == "Wandering albatross" ~ 6.5,
    seabird_sp == "Wilson's storm petrel" ~ 0.035,
    seabird_sp == "Black-winged petrel" ~ 0.175,
    seabird_sp == "Australasian gannet" ~ 2.3,
    seabird_sp == "Campbell albatross" ~ 3,
    seabird_sp == "Little shearwater" ~ 0.24,
    seabird_sp == "White-faced storm petrel" ~ 0.045,
    seabird_sp == "Black-bellied storm petrel" ~ 0.055,
    seabird_sp == "Northern royal albatross" ~ 9,
    seabird_sp == "White-bellied storm petrel" ~ 0.05,
    seabird_sp == "Red-billed gull" ~ 0.28, # Average between sexes
    seabird_sp == "Black-billed gull" ~ 0.275, # Average between sexes
    seabird_sp == "Diving petrel" ~ 0.13,
    seabird_sp == "NZ storm petrel" ~ 0.033,
    seabird_sp == "Arctic skua" ~ 0.4,
    seabird_sp == "White-capped albatross" ~ 4,
    seabird_sp == "Cape pigeon" ~ 0.45,
    seabird_sp == "Black-browed albatross" ~ 3.5,
    seabird_sp == "Fairy prion" ~ 0.125,
    seabird_sp == "Southern royal albatross" ~ 9,
    seabird_sp == "Buller's albatross" ~ 3,
    seabird_sp == "White-necked petrel" ~ 0.45,
    seabird_sp == "Brown skua" ~ 1.812, # Average between sexes
    .default = NA
  ), .after = seabird_ct)

### Save it: Long format ####

readr::write_csv(df_FarOut_tidy, 
                 "./data-processed/raw-tidy/seabird-raw-tidy-long.csv")

### Save it: Wide format (groups & species) ####

## To pivot the data into wide format, 
## each row must represent a seabird count ("id_df"), and
## each column added in the next step will be a seabird taxon 
## (a 'group' ["gr_df"] or a 'species' ["sp_df"]).

## For this, first we need to get basic info for each seabird count: 
id_df <- 
  df_FarOut_tidy %>% 
  dplyr::select(id, date, season, voyage,
                id_count_duration, id_dist_km, swell, bf) %>%
  dplyr::group_by(id) %>%
  # I've chosen to get 'max' 'bf' instead of 'swell', as it affects more the visibility 
  dplyr::slice(which.max(bf))

## Then, we need to get average values of lat/lon/hour (as a single point in space and time):
id_df <- 
  df_FarOut_tidy %>% 
  dplyr::select(id, home_screen, lat, lon, hour) %>%
  dplyr::filter(! home_screen == "Seabird count") %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(lat = mean(lat),
                   lon = mean(lon),
                   hour = 
                     lubridate::seconds_to_period(mean(
                       lubridate::period_to_seconds(hour)))) %>%
  ## And then we can join with the previous 'id_df' 
  ## (note that I've piped this next bit of code to avoid creating an unnecessary object)
  dplyr::left_join(id_df, ., by = "id") %>%
  dplyr::relocate(c(hour, lat, lon), .before = "season")

# Round id duration/distance (don't need as much 'precision' using many decimals)
id_df <- 
  id_df %>%
  dplyr::mutate(id_count_duration = round(as.numeric(id_count_duration), digits = 2),
                id_dist_km = round(id_dist_km, digits = 2))

## Group-level --------------------------------------------------------------- #
gr_df <- 
  df_FarOut_tidy %>%
  dplyr::filter(home_screen == "Seabird count") %>% 
  dplyr::select(id, seabird_gr_foraging, seabird_ct) %>%
  dplyr::group_by(id, seabird_gr_foraging) %>%
  dplyr::summarise(seabird_ct = sum(seabird_ct)) %>%
  dplyr::ungroup(.) %>% 
  tidyr::pivot_wider(id_cols = id,
                     names_from = seabird_gr_foraging,
                     values_from = seabird_ct,
                     values_fill = 0)

df_FarOut_wide_gr <- 
  dplyr::left_join(id_df, gr_df, by = "id") %>%
  replace(is.na(.), 0) %>% 
  janitor::clean_names()

## Save it
readr::write_csv(df_FarOut_wide_gr, 
                 "./data-processed/raw-tidy/seabird-raw-tidy-wide-groups.csv")

## Species-level ------------------------------------------------------------- #
sp_df <- 
  df_FarOut_tidy %>%
  dplyr::filter(home_screen == "Seabird count") %>% 
  dplyr::select(id, seabird_sp, seabird_ct) %>%
  dplyr::group_by(id, seabird_sp) %>%
  dplyr::summarise(seabird_ct = sum(seabird_ct)) %>%
  tidyr::pivot_wider(id_cols = id,
                     names_from = seabird_sp,
                     values_from = seabird_ct,
                     values_fill = 0)

df_FarOut_wide_sp <- 
  dplyr::left_join(id_df, sp_df, by = "id") %>%
  replace(is.na(.), 0) %>% 
  janitor::clean_names()

## Save it
readr::write_csv(df_FarOut_wide_sp, 
                 "./data-processed/raw-tidy/seabird-raw-tidy-wide-species.csv")

## Clean environment
rm("id_df", "gr_df", "sp_df")

