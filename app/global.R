# Cleaning ----
detach_all <- function() {
  basic.pkg <- c("package:stats", "package:graphics", "package:grDevices", 
                 "package:utils", "package:datasets", "package:methods", "package:base")
  
  pkg.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1 ,TRUE, FALSE)]
  
  pkg.list <- setdiff(pkg.list, basic.pkg)
  
  lapply(pkg.list, detach, character.only = TRUE)
}
detach_all()
rm(list = ls())

temp = memuse::Sys.meminfo(compact.free=FALSE)
if(.Platform$OS.type == "windows") {
  memory.limit(size = temp$totalram@size*1000/2)
} else if(.Platform$OS.type == "unix") {
  ulimit::memory_limit(temp$totalram@size*1000/2)
}

#Show Inf in DT datatable
# options(htmlwidgets.TOJSON_ARGS = list(na = 'string'))

# Loading ----
library(dplyr)
library(knitr)
library(plotly)
library(testthat)
library(kableExtra)

library(shiny)
library(shinydashboard)
library(tippy)
library(shinycssloaders)
library(shinytest)
library(htmltools)

library(networkD3)
library(VineCopula)
library(xtable)

library(DT)
library(randomcoloR)
library(tidyverse)
library(markdown)
library(GGally)
library(memuse)
library(ulimit)

# path_root = "//media/juro/DATA/Work/NBS/PortfolioOptimization/app/"
path_root = ""
# path_root = "C:/Users/jhledik/Desktop/PortfolioOptimization/"

source(paste0(path_root,"utils/theme.R"))
source(paste0(path_root,"utils/logo.R"))
source(paste0(path_root,"utils/utils.R"))

source(paste0(path_root, "code/R/return_simulation.R"))
source(paste0(path_root, "code/R/portfolio_optimization.R"))
