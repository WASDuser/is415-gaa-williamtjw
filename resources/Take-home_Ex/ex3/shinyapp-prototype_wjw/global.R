library(shiny)
library(shinythemes)
library(DT) # For interactive table previews (if needed)
library(ggplot2) # For plotting
library(dplyr) # For dataset manipulation
library(sfdep)
library(spdep)
pacman::p_load(sfdep, spdep, tmap, sf, ClustGeo, ggpubr, cluster, factoextra, NbClust, heatmaply, corrplot, psych, tidyverse, GGally)

# Load datasets
adm2_sf <- read_rds("data/rds/adm2_sf.rds")
crime_district <- read_rds("data/rds/crime_district.rds")
pop_data <- read_rds("data/rds/pop_data.rds")
WEST_MSIA <- read_rds('data/rds/WEST_MSIA.rds')
EAST_MSIA <- read_rds('data/rds/EAST_MSIA.rds')
crime_boundary <- read_rds('data/rds/crime_boundary.rds')