# script for GitHub Action
# download and process income indicators (37100194, 37100195)

packages <- c("dplyr", "readr", "tidyr")
install.packages(setdiff(packages, rownames(installed.packages()))) 

library(dplyr)
library(readr)
library(tidyr)
source("R/download_data.R")

income_cs <- download_data(
  "37100194", c("sex", "trade", "mode", "ind"),
  file_name="income_cs")
income_long <- download_data(
  "37100195", c("sex", "trade", "mode", "ind"),
  file_name="income_long")
