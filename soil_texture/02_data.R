## HEADER ####
## Who: J Collins
## what: 02 Data
## Last edited: 2024-04-09

## 01 LOAD DATA ####

### boundary and plot ####
bound.plots <- st_read("data/shp_files/boundry_and_plots/bound.and.full.plots.shp")

# add field boundary to treatments
bound.plots$Treatment[is.na(bound.plots$Treatment)] <- "Field"

# check spdf layers
plot(bound.plots, max.plot = 15)



### plots #### 

plots <- st_read("data/shp_files/full_plots/all.plots.shp")

# check spdf layers
plot(plots, max.plot = 25)

crs(plots)

### points ####

rp <- st_read("data/shp_files/sampling_points/250rp.wgs84.shp")

plot(rp, max.plot = 15)



### Image ####

# read photo in as a raster
image <- raster("data/shp_files/images/WGS84_High_res_field_image.tif")

# check resoloution
res(image)
#aggregate to reduce raster file resoloution
image <- aggregate(image, fact = 20)
# check reduced res
res(image)

#convert the raster to points for plotting
image <- rasterToPoints(image)

#Make the points a dataframe for ggplot
df.image <- data.frame(image)

#Make appropriate column headings
colnames(df.image) <- c("Longitude", "Latitude", 'Values')



## buffered plots ####

buff.plot <- st_read("data/shp_files/buffered_plots/all.plots.buffered.shp")

plot(buff.plot, max.plot = 15)


## load spatial grid ####

f_grid <- read.csv("data/grid_data/wgs_84_smaller_grid_points_clipped.csv")
coordinates(f_grid) <- ~ x + y 





## Soil texture ####

# shp data

soil_texture <- st_read("data/shp_files/soil_data.shp")

plot(soil_texture)


# CSV data

csvdat <- read.csv(file = "data/soil_data/soil_texture_data.csv", 
                   header = TRUE)

csvdat[is.na(csvdat)] <- 0

coordinates(csvdat) <- ~ Longitude + Latitude # step 3 above












