

TT.plot( class.sys = "none" )

TT.plot( class.sys = "UK.SSEW.TT", )

soil_tex<- read.csv(file = "data/soil_data/soil_texture_simple.csv")

omnia_dat <- read.csv("~/OneDrive - Harper Adams University/Data/Shapefiles/Omnia/omnia.data.csv")

grs_dat <- data.frame()
grs_dat <- cbind(omnia_dat$Clay, omnia_dat$Silt, omnia_dat$Sand)
grs_dat <- as.data.frame(grs_dat)
names(grs_dat) <- names(soil_tex[1:3])

TT.plot( class.sys = "UK.SSEW.TT", 
         tri.data = soil_tex, 
         main = "Soiltexturedata", 
         col = "blue") # S


# 1. Open jpeg file
jpeg("plots/textural_triangle_plot_uk.png", width = 1000, height = 1000)

# 2. Create the plot

TT.plot( class.sys = "UK.SSEW.TT", 
         tri.data = soil_tex, 
         col = "blue") # S

# 3. Close the file
dev.off()


# 1. Open jpeg file
jpeg("plots/tex_tri_no_title_plot.png", width = 1000, height = 1000)

# 2. Create the plot

soil_tex_plot <- TT.plot( class.sys = "UK.SSEW.TT", 
                 tri.data = soil_tex, 
                 col = "blue", main = NA) # S

# 3. Close the file
dev.off()


grs_plot <- TT.plot( class.sys = "UK.SSEW.TT", 
         tri.data = grs_dat, 
         col = "blue", main = NA) # S

# Arrange the plots using grid.arrange
grid.arrange(soil_tex_plot, grs_plot, ncol = 2)






# Save each plot as an image file first
png("plots/soil_tex_plot.png")
print(TT.plot(class.sys = "UK.SSEW.TT", tri.data = soil_tex, col = "blue", main = NA))
dev.off()

png("plots/grs_plot.png")
print(TT.plot(class.sys = "UK.SSEW.TT", tri.data = grs_dat, col = "blue", main = NA))
dev.off()

# Read these images back in and convert to grobs
soil_tex_grob <- rasterGrob(as.raster(png::readPNG("plots/soil_tex_plot.png")), interpolate = TRUE)
grs_grob <- rasterGrob(as.raster(png::readPNG("plots/grs_plot.png")), interpolate = TRUE)

png("plots/soil_tex_tri_combi_plot.png", width = 500, height = 250)
# Arrange the plots as images
grid.arrange(soil_tex_grob, grs_grob, ncol = 2)
dev.off()

ggarrange(soil_tex_grob, grs_grob, labels = c("A", "B"), label.y = 1)

ggsave(filename = "plots/soil_tex_tri_combi_plot.png", width = 10, height = 5)


## SUMMARY STATS ####
library(purrr)

# Define the dataset name
dataset_name <- "soil_tex"

# Iterate through all columns, processing only numeric columns
tex_sum <- map_dfr(names(soil_tex), ~ {
  # Check if the column is numeric
  if (is.numeric(soil_tex[[.x]])) {
    variable <- soil_tex[[.x]]
    
    # Compute summary statistics for each numeric variable
    summary <- soil_tex %>%
      summarise(
        n = sum(!is.na(variable)),
        mean = mean(variable, na.rm = TRUE),
        sd = sd(variable, na.rm = TRUE),
        median = median(variable, na.rm = TRUE),
        min = min(variable, na.rm = TRUE),
        max = max(variable, na.rm = TRUE),
        range = max(variable, na.rm = TRUE) - min(variable, na.rm = TRUE)
      ) %>%
      mutate(
        se = sd / sqrt(n),
        ic = se * qt((1 - 0.05) / 2 + .5, n - 1),
        variable = .x,                  # Add variable name
        dataset = dataset_name          # Add dataset name
      )
    summary
  }
})



# Define the dataset name
dataset_name <- "grs_data"

# Iterate through all columns, processing only numeric columns
grs_sum <- map_dfr(names(grs_dat), ~ {
  # Check if the column is numeric
  if (is.numeric(grs_dat[[.x]])) {
    variable <- grs_dat[[.x]]
    
    # Compute summary statistics for each numeric variable
    summary <- grs_dat %>%
      summarise(
        n = sum(!is.na(variable)),
        mean = mean(variable, na.rm = TRUE),
        sd = sd(variable, na.rm = TRUE),
        median = median(variable, na.rm = TRUE),
        min = min(variable, na.rm = TRUE),
        max = max(variable, na.rm = TRUE),
        range = max(variable, na.rm = TRUE) - min(variable, na.rm = TRUE)
      ) %>%
      mutate(
        se = sd / sqrt(n),
        ic = se * qt((1 - 0.05) / 2 + .5, n - 1),
        variable = .x,                  # Add variable name
        dataset = dataset_name          # Add dataset name
      )
    summary
  }
})

soil_texture_sum <- rbind(tex_sum, grs_sum)

write.csv(x = soil_texture_sum, file = "stats/soil_tex_comparison_stats.csv")






