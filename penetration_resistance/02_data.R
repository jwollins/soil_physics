### ERP penetrologger data
### Deployment: June 2024
### J Collins 
### 2024-06-22

### 2.1 PACKAGES ####

setwd(dir = "~/Documents/ERP/scripts/erpsoiltools/")

source(file = "penetration_res/01_packages.R")


## 02 - Data processing ####

setwd(dir = "~/jcollins@earthroverprogram.org - Google Drive/Shared drives/DATA/FIELD_DEPLOYMENTS/2024-10-Joesfield/soil_data/")



### 2.2 LOAD DATA ####

# Get the file list
filelist <- list.files(pattern = ".*.txt", path = "penetrologger_data/", full.names = TRUE)

# Create an empty list to store the data frames from each file
file_data <- list()

# Loop through each file in the file list
for (file in filelist) {
  # Read the file using read_delim
  data <- read.delim2(
    file = file,
    header = TRUE, 
    sep = "\t", 
    dec = ",",
    col.names = c("name", "coordinates", 1:82),
    skip = 13)
  
  # Store the data in the list
  file_data[[file]] <- data
}

# Now file_data contains the data frames from each file, where the names of the list elements are the file names


### 2.4 SAVE INFO ####

# Create a directory to store the processed files if it doesn't exist
output_dir <- "penetrologger_data/processed/long_format_data/"
dir.create(output_dir, showWarnings = FALSE)

# Loop through the list of dataframes
for (i in seq_along(file_data)) {
  # Save as CSV in specified directory
  csv_file <- paste0(output_dir, basename(gsub('.txt', '', filelist[[i]])), "_processed_data", ".csv")  # Name the CSV file with path
  write.csv(file_data[[i]], 
            file = csv_file, row.names = FALSE)  # Save as CSV
  
  # Print message
  cat("Saved", csv_file, "\n")
}







### 2.3 LOAD INFO ####


# Create an empty list to store the data frames from each file
file_info <- list()

# Loop through each file in the file list
for (file in filelist) {
  # Read the file using read_delim
  info <- read.delim2(
    file = file,
    header = TRUE, 
    sep = ":", 
    dec = ".",
    col.names = c("name", "data"),
    skip = 2, 
    nrows = 9, strip.white = T)
  
  # Store the data in the list
  file_info[[file]] <- as.data.frame(info)
}

# Now file_data contains the data frames from each file, where the names of the list elements are the file names


#dir.create(path = "2024_05_31/info/")
# Create a directory to store the processed files if it doesn't exist
output_dir <- "penetrologger_data/info/"
dir.create(output_dir, showWarnings = FALSE)




### 2.4 SAVE INFO ####

# Loop through the list of dataframes
for (i in seq_along(file_info)) {
  # Save as CSV in specified directory
  csv_file <- paste0(output_dir, basename(gsub('.txt', '', filelist[[i]])), "_processed_info", ".csv")  # Name the CSV file with path
  write.csv(file_info[[i]], 
            file = csv_file, row.names = FALSE)  # Save as CSV
  
  # Print message
  cat("Saved", csv_file, "\n")
}



## add the treatments to the info

for (i in seq_along(file_info)) {
  ifelse(file_info[[i]]$data[1] == 24060703 | file_info[[i]]$data[1] == 24060704, 
         yes = file_info[[i]] <- rbind(file_info[[i]], list("Treatment", "Conventional")), 
         no = file_info[[i]] <- rbind(file_info[[i]], list("Treatment", "Conservation")))
}



### 2.5 DATA PROCESSING ####

# Create a directory to store the processed files if it doesn't exist
output_dir <- "penetrologger_data/processed/"
dir.create(output_dir, showWarnings = FALSE)


rbind(file_info[[1]], list("Treatment", "Conservation"))




### 2.51 Coordinates ####

coord_dat <- data.frame()

# Loop through each data frame in data_frames
for (i in seq_along(file_data)) {
  
  
  # project name
  file_data[[i]]$project <- file_info[[i]][1,2]
  
  file_data[[i]] <- file_data[[i]] %>% relocate(project, .after = coordinates)
  
  # username
  file_data[[i]]$username <- file_info[[i]][2,2]
  
  file_data[[i]] <- file_data[[i]] %>% relocate(username, .after = project)
  
  # date
  file_data[[i]]$date <- file_info[[i]][3,2]
  
  file_data[[i]] <- file_data[[i]] %>% relocate(date, .after = username)
  
  # pens per plot
  file_data[[i]]$pens_per_plot <- file_info[[i]][4,2]
  
  file_data[[i]] <- file_data[[i]] %>% relocate(pens_per_plot, .after = date)
  
  # nr_of_pens_done
  file_data[[i]]$nr_of_pens_done <- file_info[[i]][5,2]
  
  file_data[[i]] <- file_data[[i]] %>% relocate(nr_of_pens_done, .after = pens_per_plot)
  
  # cone type
  file_data[[i]]$cone_type <- file_info[[i]][6,2]
  
  file_data[[i]] <- file_data[[i]] %>% relocate(cone_type, .after = nr_of_pens_done)
  
  # pen speed
  file_data[[i]]$pen_speed <- file_info[[i]][7,2]
  
  file_data[[i]] <- file_data[[i]] %>% relocate(pen_speed, .after = cone_type)
  
  # depth unit
  file_data[[i]]$depth_unit <- file_info[[i]][8,2]
  
  file_data[[i]] <- file_data[[i]] %>% relocate(depth_unit, .after = pen_speed)
  
  # pressure unit
  file_data[[i]]$pressure_unit <- file_info[[i]][9,2]
  
  file_data[[i]] <- file_data[[i]] %>% relocate(pressure_unit, .after = depth_unit)
  
  
 #  # bind the name and coordinate rows from the df list
  coord_dat <-  rbind(coord_dat, file_data[[i]][,c(1:11)])
  
}




# Function to convert DMS to decimal degrees
dms_to_decimal <- function(d, m, direction) {
  decimal <- as.numeric(d) + as.numeric(m) / 60
  if (direction %in% c("S", "W")) {
    decimal <- -decimal
  }
  return(decimal)
}

# Extract and convert coordinates
coord_dat <- coord_dat %>%
  mutate(
    lat_deg = as.numeric(str_extract(coordinates, "(?<=N|S)\\d+")),
    lat_min = as.numeric(str_extract(coordinates, "(?<=N\\d{2} )\\d+\\.\\d+")),
    lon_deg = as.numeric(str_extract(coordinates, "(?<=W|E)\\d+")),
    lon_min = as.numeric(str_extract(coordinates, "(?<=W\\d{3} )\\d+\\.\\d+")),
    lat_dir = str_extract(coordinates, "^[NS]"),
    lon_dir = str_extract(coordinates, "[WE]$"),
    latitude = mapply(dms_to_decimal, lat_deg, lat_min, lat_dir),
    longitude = mapply(dms_to_decimal, lon_deg, lon_min, lon_dir)
  )



# Extract plot number and create new column
coord_dat <- coord_dat %>%
  mutate(plot_number = as.numeric(str_extract(name, "(?<=PLOT-)\\d+\\.\\d+")))

# change crs 
coord_dat$longitude = coord_dat$longitude*(-1)


# add treatment

# coord_dat$treatment <- ifelse(test = coord_dat$project == 24060703 | 24060703, 
#                               yes = "Conservation", 
#                               no = "Conventional")

# Save as CSV in specified directory
write.csv(x =  coord_dat, 
          file = "penetrologger_data/info/24_10_info.csv", 
          row.names = FALSE)











# for (i in seq_along(file_info)) {
#   # bind the name and coordinate rows from the df list
#   coord_dat$project <-  file_info[[i]][,2]
# }







### DATA ####



# Create a directory to store the processed files if it doesn't exist
output_dir <- "penetrologger_data/processed/"
#dir.create(output_dir, showWarnings = FALSE)



# Loop through each data frame in data_frames
for (i in seq_along(file_data)) {
  # Remove 1st and 2nd columns from each data frame
  file_data[[i]] <- file_data[[i]][,-c(1 : 11)]

  file_data[[i]] <- file_data[[i]] %>% mutate_if(is.character, as.numeric)

  # transpose the df
  file_data[[i]] <- as.data.frame(t(x = file_data[[i]]))

  # add the mean row
  file_data[[i]]$mean <- rowMeans(file_data[[i]], na.rm = FALSE)


  samples <- ncol(file_data[[i]])

  #calculate standard deviation of each row
  file_data[[i]]$stdev <- apply(file_data[[i]][,1:samples],
                                MARGIN = 1,
                                FUN = sd,
                                na.rm = TRUE)
  
  # Calculate standard error of the mean for each row
  file_data[[i]]$sem <- apply(file_data[[i]][,1: samples], 
                            MARGIN = 1, 
                            FUN = function(x) sd(x, na.rm=TRUE) / sqrt(sum(!is.na(x))))


  # create depth df
  depth <- as.data.frame(1:82, nm = "depth_cm")


  #bind both df's
  file_data[[i]] <- cbind(depth, file_data[[i]])
  
  # add project name 
  file_data[[i]]$name <- file_info[[i]]$data[1]


  # Save as CSV in specified directory
  csv_file <- paste0(output_dir, basename(gsub('.txt', '', filelist[i])), "_processed", ".csv")  # Name the CSV file with path

  write.csv(file_data[[i]], file = csv_file, row.names = FALSE)  # Save as CSV

  # Print message
  cat("Saved", csv_file, "\n")

}





# std <- function(x) sd(x)/sqrt(length(x))
#
# #calculate standard deviation of each row
# data$se <- apply(X = data,
#                            MARGIN = 1,
#                            FUN = std(data[,2:11]),
#                            na.rm=TRUE)



### 2.6 SPATIAL DATA ####


#### field data ####

## load shapefiles for the field and treatments.
bound.plots <- st_read("~/jcollins@earthroverprogram.org - Google Drive/Shared drives/DATA/site_data/joes_field/map_data/shp_files/boundry_and_plots/bound.and.full.plots.shp")

# add field boundary to treatments
bound.plots$Treatment[is.na(bound.plots$Treatment)] <- "Field"

# check spdf layers
plot(bound.plots, max.plot = 15)





#### stryde data ####

# Read the CSV file
stryde_positions <- read.csv("2024_05_31/stryde_deployed_positions.csv")

# Ensure Easting and Northing columns are numeric
stryde_positions$Easting <- as.numeric(stryde_positions$Easting)
stryde_positions$Northing <- as.numeric(stryde_positions$Northing)

# Create an sf object for the data, assuming the coordinates are UTM but we need to determine the zone
# Assuming the data is from a known region; we can deduce the UTM zone.
# For example, if we know the data is from a specific part of Europe, we might use EPSG:32632 (UTM Zone 32N) or EPSG:32633 (UTM Zone 33N)
# For this example, let's assume UTM Zone 33N (EPSG:32633)

# If the region is not well-known, one can alternatively loop through possible UTM zones to find the best match,
# or use additional metadata if available to pinpoint the exact UTM zone.

# Create sf object assuming UTM Zone 33N (adjust this based on your known region)
stryde_sf <- st_as_sf(stryde_positions, coords = c("Easting", "Northing"), crs = 32630)

# Transform the coordinates to CRS 4326
stryde_sf_4326 <- st_transform(stryde_sf, crs = 4326)

# Extract the coordinates back into the data frame
transformed_coords <- st_coordinates(stryde_sf_4326)
stryde_positions$longitude <- transformed_coords[, 1]
stryde_positions$latitude <- transformed_coords[, 2]

# Save the transformed coordinates to a new CSV file
# Save as CSV in specified directory
write.csv(x =  stryde_positions, 
          file = "2024_05_31/processed/stryde_positions_crs_4326.csv", 
          row.names = FALSE)



# Save as CSV in specified directory
write.csv(x =  coord_dat, 
          file = "2024_05_31/processed/2024_06_info.csv", 
          row.names = FALSE)







## 2.7 Long format processing ####

# Get the file list
filelist <- list.files(pattern = ".*.csv", 
                       path = "penetrologger_data/processed/long_format_data/", 
                       full.names = TRUE)

# Create an empty list to store the data frames from each file
file_data <- list()

# Loop through each file in the file list
for (file in filelist) {
  # Read the file using read_delim
  data <- read.csv(file = file)
  
  # Store the data in the list
  file_data[[file]] <- data
}




transpose_data <- function(df) {
  
  # Melt the dataframe to convert it into long format
  df_melted <- pivot_longer(df, 
                            cols = starts_with("X"), 
                            names_to = "depth", 
                            names_prefix = "X", 
                            values_to = "penetration_resistance")
  
  # Convert depth to numeric
  df_melted$depth <- as.numeric(df_melted$depth)
  
  # Order the dataframe
  df_melted <- df_melted[order(df_melted$name, df_melted$coordinates, df_melted$depth), ]
  
  # Select only the relevant columns
  df_transposed <- df_melted[, c("name", "coordinates", "depth", "penetration_resistance")]
  
  # Return the transposed dataframe
  return(df_transposed)
}

# Example usage:

 df_transposed <- transpose_data(df)
# head(df_transposed)



# Loop through each data frame in data_frames and run transpose function
for (i in seq_along(file_data)) {
 
  file_data[[i]] <- transpose_data(file_data[[i]]) 
}


long_data <- data.frame(matrix(nrow = 0, ncol = 4)) 
colnames(x = long_data) <- c("name", "coordinates", "depth", "penetration_resistance")

# Loop through each data frame and Cbind it
for (i in seq_along(file_data)) {
  
  long_data <- rbind(long_data, file_data[[i]])
}


# Extract and convert coordinates
long_data <- long_data %>%
  mutate(
    lat_deg = as.numeric(str_extract(coordinates, "(?<=N|S)\\d+")),
    lat_min = as.numeric(str_extract(coordinates, "(?<=N\\d{2} )\\d+\\.\\d+")),
    lon_deg = as.numeric(str_extract(coordinates, "(?<=W|E)\\d+")),
    lon_min = as.numeric(str_extract(coordinates, "(?<=W\\d{3} )\\d+\\.\\d+")),
    lat_dir = str_extract(coordinates, "^[NS]"),
    lon_dir = str_extract(coordinates, "[WE]$"),
    latitude = mapply(dms_to_decimal, lat_deg, lat_min, lat_dir),
    longitude = mapply(dms_to_decimal, lon_deg, lon_min, lon_dir)
  )



# Extract the plot number and replicate number from the 'name' column
long_data <- long_data %>%
  mutate(plot_number = sub("PLOT-(\\d{3})\\..*", "\\1", name),
         replicate_number = sub("PLOT-\\d{3}\\.(\\d)", "\\1", name))


write.csv(x = long_data, file = "penetrologger_data/processed/long_format_data/long_format_data.csv")









## 2.8 Bind data processing ####

# Get the file list
filelist <- list.files(pattern = ".*.csv", 
                       path = "penetrologger_data/processed/long_format_data/", 
                       full.names = TRUE)

# Create an empty list to store the data frames from each file
file_data <- list()

# Loop through each file in the file list
for (file in filelist) {
  # Read the file using read_delim
  data <- read.csv(file = file)
  
  # Store the data in the list
  file_data[[file]] <- data
}

# # Column bind all data frames together
 combined_df <- do.call(rbind, file_data)



# infodat <- read.csv(file = "penetrologger_data/info/24_10_info.csv")
# 
# combined_info_dat <- cbind(infodat, combined_df)  
# 
# write_csv(x = combined_info_dat, 
#           file = "data/processed/combined_info_dat.csv")  



## transpose it 


# Define the transpose function
transpose_data <- function(df) {
  
  # Identify the columns that are not depth-related
  non_depth_cols <- colnames(df)[!grepl("^X", colnames(df))]
  
  # Melt the dataframe to convert depth columns into long format
  df_melted <- pivot_longer(df, 
                            cols = starts_with("X"),  # Select depth columns (X1 to X82)
                            names_to = "Depth_cm",    # New column for depth
                            values_to = "Measurement") # New column for measurement values
  
  # Convert Depth_cm from 'X1', 'X2', ... 'X82' to numeric values (1 to 82)
  df_melted$Depth_cm <- as.numeric(sub("X", "", df_melted$Depth_cm))
  
  # Return the long-format dataframe
  return(df_melted)
}

# Apply the function to your dataframe
long_test <- transpose_data(combined_info_dat)

# Replace negative values in the Measurement column with 0
long_test <- long_test %>%
  mutate(Measurement = ifelse(Measurement < 0, 0, Measurement))


# Extract the plot number and replicate number from the 'name' column
long_test <- long_test %>%
  mutate(plot_number = sub("PLOT-(\\d{3})\\..*", "\\1", name),
         replicate_number = sub("PLOT-\\d{3}\\.(\\d)", "\\1", name))

# add treatment

# Corrected code to create the 'treatment' column
long_test$treatment <- ifelse(test = long_test$project %in% c(24060705, 24060706, 24060707), 
                              yes = "Conservation", 
                              no = "Conventional")

long_test$treatment <- ifelse(test = long_test$project %in% c(24053102), 
                              yes = "Soil Hall", 
                              no = long_test$treatment)




write.csv(x = long_test, file = "data/processed/long_format_data/combined_data_info.csv")
  

