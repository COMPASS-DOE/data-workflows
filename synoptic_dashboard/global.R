## This script holds functions and constants

library(googledrive)
library(janitor)
library(lubridate)
library(plotly)
library(purrr)
library(shiny)
library(shinydashboard)
library(tidyverse)

source("aquatroll_functions.R")
source("sapflow_functions.R")
source("teros_functions.R")

# First, set the GDrive folder to find files
directory <- "https://drive.google.com/drive/folders/1-1nAeF2hTlCNvg_TNbJC0t6QBanLuk6g"

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = "*@pnnl.gov",
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "../synoptic_dashboard/.secrets"
)

# drive_auth(path = ".secrets/client_secret.json")
gdrive_files <- drive_ls(directory)

# create the start-time based on two weeks ago (from today)
two_weeks_ago <- with_tz(Sys.time(), tzone = "EST") - days(14)


