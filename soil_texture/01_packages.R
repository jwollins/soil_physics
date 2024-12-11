## HEADER ####
## Who: J Collins
## what: 01 Packages
## Last edited: 2024-04-09

## 01.1 PACKAGES ####

suppressPackageStartupMessages({
  library(dplyr) # for "glimpse" and data manipulation
  library(ggplot2) # general plotting
  library(scales) # for "comma"
  library(magrittr)
  library(ggpubr) 
  library(sp)
  library(gstat)
  library(rgdal)
  library(raster)
  library(gridExtra)
  library(png)
  library(grid)
  library(jpeg)
  library(cowplot)
  library(ggsci)
  library(sjPlot)
  library(sjmisc)
  library(sjlabelled)
  library(viridis)
  library(broom)
  library(sf) # load shp files
  library(reshape2) # cast spatial objects to df
  library(raster)
  library(rasterVis)
  library(automap) # auto variograms
  library(ggspatial) # annotate north arrow
  library(readxl)
  library(soiltexture)
})