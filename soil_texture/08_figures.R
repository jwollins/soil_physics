# 08 FIGURES ####


# Install and load necessary packages
install.packages("ggpubr")  # Uncomment if ggpubr is not installed
library(ggpubr)
library(png)
library(grid)

# Load the .png files
img1 <- readPNG("plots/plot_stats/_Clay_krige_plot.png")
img2 <- readPNG("plots/plot_stats/_sand_krige_plot.png")
img3 <- readPNG("plots/plot_stats/_Silt_krige_plot.png")

# Convert the images to grobs (graphical objects)
grob1 <- rasterGrob(img1, interpolate = TRUE)
grob2 <- rasterGrob(img2, interpolate = TRUE)
grob3 <- rasterGrob(img3, interpolate = TRUE)

# Use ggarrange to arrange the images in a grid
ggarrange(grob1, grob2, grob3, 
          ncol = 2, nrow = 2, labels = c("A", "B", "C"))

ggsave(filename = "plots/plot_stats/soil_tex_variograms.png")




# Install and load necessary packages
install.packages("ggpubr")  # Uncomment if ggpubr is not installed
install.packages("jpeg")    # Uncomment if jpeg is not installed
library(ggpubr)
library(png)
library(jpeg)
library(grid)

# Load the .png and .jpeg files
img1 <- readPNG("plots/soil_class_plot.png")     # PNG file
img2 <- readJPEG("plots/tex_tri_no_title_plot.png")  # JPEG file

# Convert the images to grobs (graphical objects)
grob1 <- rasterGrob(img1, interpolate = TRUE)
grob2 <- rasterGrob(img2, interpolate = TRUE)

# Use ggarrange to arrange the images in a grid
ggarrange(grob2, grob1, 
          ncol = 1, 
          nrow = 2, 
          align = "v", 
          labels = c("A", "B"), label.x = 0)

ggsave(filename = "plots/combi_soil_tex_plot.png", width = 5, height = 10)







t <- TT.plot( class.sys = "UK.SSEW.TT", 
         tri.data = soil_tex, 
         col = "blue", main = NA) # S

# Convert the images to grobs (graphical objects)
grob1 <- rasterGrob(t, interpolate = )



var_plot <- ggplot() + 
  geom_point(data = all_class_dat, 
             size = 1, 
             shape = 18, 
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
  labs(
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
           crs = 4326) + 
  guides(color = guide_legend(override.aes = list(size = 10))) 

var_plot

ggsave(filename = "plots/soil_class_plot.png", width = 6, height = 6)
