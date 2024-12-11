### Plot of soil textural classification 
### J Collins 
### 2024/08/15


source(file = "scripts/01_packages.R")
source(file = "scripts/02_data.R")


clay_krige_data <- read.csv(file = "data/krige_data/clay_krige.csv", 
                            col.names = c("x", "y", "clay", "clay_var", "clay_stdev"))

sand_krige_data <- read.csv(file = "data/krige_data/sand_krige.csv", 
                            col.names = c("x", "y", "sand", "sand_var", "sand_stdev"))

silt_krige_data <- read.csv(file = "data/krige_data/silt_krige.csv", 
                            col.names = c("x", "y", "silt", "silt_var", "silt_stdev"))


all_krige_dat <- cbind(clay_krige_data, sand_krige_data[,3:5], silt_krige_data[,3:5])


# round down all values in column

all_krige_dat$clay <- floor(all_krige_dat$clay) 
all_krige_dat$sand <- floor(all_krige_dat$sand)
all_krige_dat$silt <- floor(all_krige_dat$silt)



all_krige_dat$sum <- rowSums( all_krige_dat[ , c("clay", "sand", "silt")])




# Function to classify soil texture
classify_soil_texture <- function(df) {
  # Check if the dataframe has sand, silt, and clay columns
  if (!all(c("sand", "silt", "clay") %in% colnames(df))) {
    stop("The dataframe must have 'sand', 'silt', and 'clay' columns")
  }
  
  # Normalize the values so that they sum to 100
  df$sum <- rowSums(df[, c("sand", "silt", "clay")])
  
  df$sand <- df$sand / df$sum * 100
  df$silt <- df$silt / df$sum * 100
  df$clay <- df$clay / df$sum * 100
  
  # Rename columns to match what the soiltexture package expects
  df <- df[, c("clay", "silt", "sand")]
  colnames(df) <- c("CLAY", "SILT", "SAND")
  
  # Apply the soil texture classification
  soil_textures <- TT.points.in.classes(
    tri.data = df,
    class.sys = "UK.SSEW.TT"
  )
  
  return(soil_textures)
}




classified_data <- classify_soil_texture(all_krige_dat)

all_class_dat <- cbind(all_krige_dat[,1:2], classified_data)

colnames(all_class_dat)

all_class_dat$class <- NA

all_class_dat$class <- ifelse(test = all_class_dat[,3] >= 1, "Clay", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,4] >= 1, "Sandy Clay", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,5] >= 1, "Silty Clay", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,6] >= 1, "Clay Loam", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,7] >= 1, "Silty Clay Loam", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,8] >= 1, "Sandy Clay Loam", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,9] >= 1, "Sand Loam", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,10] >= 1, "Sandy Silt loam", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,11] >= 1, "Silt loam", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,12] >= 1, "Loamy Sand", all_class_dat$class)
all_class_dat$class <- ifelse(test = all_class_dat[,13] >= 1, "Sand", all_class_dat$class)





all_class_dat$class <- as.factor(all_class_dat$class)


### plot the krige output ####

var_plot <- ggplot() + 
  geom_point(data = all_class_dat, 
             size = 1, 
             shape = 19, 
             aes(x = x, 
                 y = y, 
                 color = class, fill = class)) +  # Add color aesthetic for 'class'
  scale_color_viridis_d(option = "D") +  # Optional: Use a color scale that's suitable for discrete data
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"), 
        legend.position = "bottom", 
        legend.key.width = unit(1, 'cm')) + 
  labs(title = "Soil Texture Classification",
      subtitle = "UK Soil Survey of England and Wales texture classification",
       x = "Longitude",
       y = "Latitude",
      color = "Soil Class",     # Change legend title for color
      fill = "Soil Class") +
  annotation_north_arrow(
    location = "tl",
    pad_x = unit(0.5, "in"),
    pad_y = unit(0.25, "in"),
    style = north_arrow_fancy_orienteering) +
  annotation_scale(
    location = "br",
    pad_x = unit(1, "cm"),
    pad_y = unit(1, "cm")) +
  coord_sf(xlim = c(-2.6138, -2.604), 
           ylim = c(52.912, 52.917),
           crs = 4326)

print(var_plot)

#ggsave(filename = "plots/textural_class_plot.png", device = "png")
