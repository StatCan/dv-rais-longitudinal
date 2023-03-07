# script for GitHub Action
# download and process data for all tables

packages <- c("dplyr", "readr", "tidyr")
install.packages(setdiff(packages, rownames(installed.packages()))) 

library(dplyr)
library(readr)
library(tidyr)
source("R/download_data.R")

pathway <- download_data(
  "37100193", c("sex", "trade", "ind"),
  file_name="pathway")

income_cs <- download_data(
  "37100194", c("sex", "trade", "mode", "ind"),
  file_name="income_cs")
income_long <- download_data(
  "37100195", c("sex", "trade", "mode", "ind"),
  file_name="income_long")

mobility_measures <- download_data(
  "37100205", c("trade", "mode", "years", "type", "ind"),
  file_name="mobility_measures")
mobility_matrix <- download_data(
  "37100204", c("trad", "mode", "years", "type", "to"),
  file_name="mobility_matrix")