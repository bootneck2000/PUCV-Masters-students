library(tidyverse)
library(lubridate)


dir = "C:/Users/field_admin/Desktop/Camera test"
setwd(dir)


d = readr::read_csv("AX_2573_20251110.csv") %>%
  dplyr::mutate(datetime = date + millis/1000) %>%
  d
  
d$datetime = format(d$datetime, "%Y-%m-%d %H:%M:%OS3")
