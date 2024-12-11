## HEADER ####
## Who: J Collins
## what: Soil texture kriging - SILT
## Last edited: 2024-04-09

variable <- csvdat$silt
var_name <- "Silt"




## 02 Varigram fitting ####

# Varigram

class(csvdat)

vario <-  variogram(variable ~ 1, data = csvdat)


vario.fit = autofitVariogram(
  variable ~ 1,
  csvdat,
  model = c("Ste", "Sph", "Mat", "Exp", "Gau"),
  #The list of variogrammodels that will be tested.
  kappa = c(0.05, seq(0.2, 2, 0.1), 5, 10),
  # Smoothing parameter of the Matern model. Provide a list if you want to check more than one value.
  fix.values = c(NA, NA, NA),
  #nugget, range and sill respectively NA means that the value is not fixed.
  start_vals = c(NA, NA, NA),
  verbose = T) # if TRUE the function will give extra feedback on the fitting process



plot(vario.fit, sub = var_name)










## 06 Kriging ####

#Perform ordinary kriging and store results inside object of type "autoKrige" "list" 


kriging_result = autoKrige(variable ~ 1, csvdat, f_grid)





#open png for file save and define size and resolution
png(paste("plots/plot_stats/", 
          filename = paste0(var_name, "_krige_plot", ".png", sep="")),
    width = 1000,
    height = 1000, 
    res = 150)

plot(kriging_result,  sub = var_name)

dev.off()







# Cast the Spatial object to a data.frame


ggplot_data = as.data.frame(kriging_result$krige_output)

write.csv(x = ggplot_data, 
          file = "data/krige_data/silt_krige.csv", col.names = T, row.names = F)



### plot the krige output ####


var_plot <- ggplot()+ geom_point(data = df.image, aes(y = Latitude,
                                                      x = Longitude,
                                                      color = Values),
                                 show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = ggplot_data, aes(x = x, y = y, fill = var1.pred)) + 
  scale_fill_gradient(low = 'white',
                      high = 'orange') +
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"), 
        legend.position="bottom",
        legend.key.width= unit(1, 'cm')) + 
  labs(subtitle = paste("Soil Texture -", var_name, "Content (%)"), 
       fill = paste(var_name, "Content (%)"), 
       x = "Longitude", 
       y = "Latitude") +
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


var_plot

ggsave(filename = paste0(var_name, "_plot.png"), 
       plot = var_plot,  
       device = "png", 
       path = "plots/")






### plot the krige SD ####




var_sd_plot <- ggplot()+ geom_point(data = df.image, 
                                    aes(y=Latitude, 
                                        x=Longitude, 
                                        color=Values),
                                    show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = ggplot_data, aes(x = x, y = y, fill = var1.stdev)) + 
  scale_fill_gradient(low = 'white',
                      high = 'orange') +
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"), 
        legend.position="bottom",
        legend.key.width= unit(1, 'cm')) + 
  labs(subtitle = paste("Prediction Standard Deviation -", var_name, "Content (%)"), 
       fill = "Standard deviation", 
       x = "Longitude", 
       y = "Latitude") +
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

var_sd_plot

ggsave(filename = paste0(var_name, "_sd_plot.png"), 
       plot = var_sd_plot,  
       device = "png", 
       path = "plots/plot_stats/")




### plot the krige variance ####




var_variance_plot <- ggplot()+ geom_point(data = df.image, 
                                          aes(y=Latitude, 
                                              x=Longitude, 
                                              color=Values),
                                          show.legend = FALSE) +
  scale_colour_gradient(
    low = "white",
    high = "darkgrey",
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour") +
  geom_raster(data = ggplot_data, aes(x = x, y = y, fill = var1.var)) + 
  scale_fill_gradient(low = 'white',
                      high = 'orange') +
  xlim(-2.6138, -2.604) +
  ylim(52.912, 52.917) +
  theme(plot.title = element_text(size = 14, face = "bold"), 
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"), 
        legend.position="bottom",
        legend.key.width= unit(1, 'cm')) + 
  labs(subtitle = paste("Prediction Variance -", var_name, "Content (%)"), 
       fill = "Variance", 
       x = "Longitude", 
       y = "Latitude") +
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

var_variance_plot

ggsave(filename = paste0(var_name, "_variance_plot.png"), 
       plot = var_variance_plot,  
       device = "png", 
       path = "plots/plot_stats/")
