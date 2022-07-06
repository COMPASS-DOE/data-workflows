## This script holds functions and constants

library(googledrive)
library(janitor)
library(purrr)
library(shiny)
library(tidyverse)
library(shiny)
library(shinydashboard)

source("aquatroll_functions.R")

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
