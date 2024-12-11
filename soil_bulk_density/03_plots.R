### ERP bulk density, moisture content and stone content data
### Deployment: November 2023
### J Collins 
### 2024-04-22


## 03 PLOTS ####

### 3.1 PACKAGES ####

setwd(dir = "~/Documents/ERP/scripts/erpsoiltools/")
source(file = "bulk_density/01_packages.R")


### 3.2 DATA ####

setwd(dir = "~/jcollins@earthroverprogram.org - Google Drive/Shared drives/DATA/FIELD_DEPLOYMENTS/")

dat <- read.csv("2024-06-Joesfield/soil_data/bulk_density/bulk_density_data.csv")

dir.create(path = "2024-06-Joesfield/soil_data/plots/")


### 3.3 PLOTS ####

#### moisture ####

# ggplot(data = dat, 
#        aes(x = experiment, 
#            y = `mositure_content_%`, 
#            fill = depth_cm)) +
#   geom_bar(stat="identity", 
#            color="black", 
#            position=position_dodge()) + 
#   labs(title = "ERP Deployment November 2023", 
#        subtitle = "Soil Moisture Content (%)", 
#        x = "Experiment Location", 
#        y = "Soil moisture content (%)", 
#        fill = "Depth (cm)") + 
#   scale_fill_discrete(labels=c('0 - 5', '10 - 15', '20 - 25', "30 - 35")) +
#   theme_minimal() + 
#   ggsave(filename = "2023_11_moisture_con.png", 
#          path = "Bulk_Density/plots/moisture_content/", 
#          device = "png")
# 
# 
# ### by treatment ####
# 
# ggplot(data = dat, 
#        aes(x = treatment, 
#            y = `mositure_content_%`, 
#            fill = depth_cm)) +
#   geom_bar(stat="identity", 
#            color="black", 
#            position=position_dodge()) + 
#   labs(title = "ERP Deployment November 2023", 
#        subtitle = "Soil Moisture Content (%)", 
#        x = "Experiment Location", 
#        y = "Soil moisture content (%)", 
#        fill = "Depth (cm)") + 
#   scale_fill_discrete(labels=c('0 - 5', '10 - 15', '20 - 25', "30 - 35")) +
#   theme_minimal() + 
#   ggsave(filename = "2023_11_moisture_con.treatment.png", 
#          path = "Bulk_Density/plots/moisture_content/", 
#          device = "png")





#### bulk density ####

title <- expression(Soil~Bulk~Density~("g"~"/"~"cm"^{3}))  # this is the legend title with correct notation

ggplot(data = dat, 
       aes(x = Treatment, 
           y = bulk_density_g_cm3, fill = Treatment)) +
  geom_bar(stat="identity", 
           color="black", 
           position=position_dodge()) + 
  scale_fill_manual(values=c(alpha("turquoise3",alpha = 1), 
                             alpha("tomato2",alpha = 1))) + 
  labs(title = "ERP Deployment November 2023", 
       subtitle = title, 
       x = "Treatment", 
       y = title, 
       fill = "Treatment") + 
  facet_wrap(facets = dat$Depth_cm, ncol = 5) +
  theme(legend.position="bottom", axis.text.x = element_blank())


 ggsave(filename = "bulk_density_deep.png", 
         path = "2024-06-Joesfield/soil_data/plots/", 
         device = "png")


ggplot(data = dat, 
       aes(x = bulk_density_g_cm3, 
           y = Depth_cm, 
           colour = Treatment)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") + # Add model line
  scale_y_reverse() + 
  scale_fill_manual(values=c(alpha("turquoise3",alpha = 1), 
                             alpha("tomato2",alpha = 1))) + 
  labs(title = "ERP Deployment November 2023", 
       subtitle = title, 
       x = "Bulk Density", 
       y = "Depth (cm)", 
       fill = "Treatment") + 
  theme(legend.position="bottom")




## PLOTS WITH PHOTOS ####


# Load images for each treatment
img_treatment1 <- image_read("~/jcollins@earthroverprogram.org - Google Drive/Shared drives/Science/FIELD_DEPLOYMENTS/2024_06_DEPLOYMENT/soil_pit_photos/tillage_profile_reduced.jpg") # Replace with path to image 1
img_treatment2 <- image_read("~/jcollins@earthroverprogram.org - Google Drive/Shared drives/Science/FIELD_DEPLOYMENTS/2024_06_DEPLOYMENT/soil_pit_photos/no_till_profile_reduced.jpg") # Replace with path to image 2

# Convert images to grobs
img_grob_treatment1 <- rasterGrob(as.raster(img_treatment1), interpolate = TRUE)
img_grob_treatment2 <- rasterGrob(as.raster(img_treatment2), interpolate = TRUE)

# Create a function to conditionally add the background image based on the treatment
add_background <- function(Treatment) {
  if (Treatment == "Conventional") {
    annotation_custom(img_grob_treatment1, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  } else if (Treatment == "Conservation") {
    annotation_custom(img_grob_treatment2, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  }
}

# Create the plot
p <- ggplot(data = dat, 
            aes(x = bulk_density_g_cm3, 
                y = Depth_cm, 
                colour = Treatment)) + 
  facet_wrap(~ Treatment) + 
  add_background("Conservation") + # Add background for Treatment1 facet
  add_background("Conventional") +  # Add background for Treatment2 facet
  geom_point(aes(fill = Treatment), colour = "white", pch = 21, size = 5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", aes(color = Treatment)) + 
  scale_y_reverse() + 
  scale_fill_manual(values = c(alpha("turquoise3", alpha = 1), 
                               alpha("tomato2", alpha = 1))) + 
  scale_color_manual(values = c("turquoise3", "tomato2")) + # Swap colors for the lines
  labs(title = "ERP Deployment November 2023", 
       subtitle = title, 
       x = "Bulk Density", 
       y = "Depth (cm)", 
       fill = "Treatment") + 
  theme(legend.position = "bottom") 

print(p)


## COMBINED WITH PHOTOS ####


# Load images for each treatment
img_tillage <- image_read("~/jcollins@earthroverprogram.org - Google Drive/Shared drives/Science/FIELD_DEPLOYMENTS/2024_06_DEPLOYMENT/soil_pit_photos/tillage_profile_reduced.jpg") # Replace with path to image 1
img_no_till <- image_read("~/jcollins@earthroverprogram.org - Google Drive/Shared drives/Science/FIELD_DEPLOYMENTS/2024_06_DEPLOYMENT/soil_pit_photos/no_till_profile_reduced.jpg") # Replace with path to image 2


# Convert images to grobs
img_grob_tillage <- rasterGrob(as.raster(img_tillage), interpolate = TRUE)
img_grob_no_till <- rasterGrob(as.raster(img_no_till), interpolate = TRUE)

# Filter data for each treatment
dat_tillage <- subset(dat, Treatment == "Conventional")
dat_no_till <- subset(dat, Treatment == "Conservation")

# Create separate plots for each treatment with their respective background images
plot_tillage <- ggplot(dat_tillage, 
                       aes(x = bulk_density_g_cm3, y = Depth_cm, colour = Treatment)) +
  annotation_custom(img_grob_tillage, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(aes(fill = Treatment), colour = "white", pch = 21, size = 5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 2) + 
  scale_y_reverse() +
  scale_fill_manual(values = "tomato2") + 
  scale_color_manual(values = "tomato2") + 
  labs(title = "ERP Deployment June 2024", 
       subtitle = title, 
       x = "Bulk Density", 
       y = "Depth (cm)", 
       fill = "Treatment") + 
  xlim(1.3, 1.7) +
  theme(legend.position = "none")

plot_no_till <- ggplot(dat_no_till, 
                       aes(x = bulk_density_g_cm3, y = Depth_cm, colour = Treatment)) +
  annotation_custom(img_grob_no_till, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  geom_point(aes(fill = Treatment), colour = "white", pch = 21, size = 5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 2) + 
  scale_y_reverse() + 
  scale_fill_manual(values = "turquoise3") + 
  scale_color_manual(values = "turquoise3") + 
  labs(title = "",
       x = "Bulk Density", 
       y = element_blank(), 
       fill = "Treatment") + 
  xlim(1.3, 1.7) +
  theme(legend.position = "none", axis.text.y=element_blank())

# Combine the plots side-by-side
combined_plot <- plot_tillage + plot_no_till + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

print(combined_plot)

ggsave("2024-06-Joesfield/soil_data/plots/combined_plot_image_background.png", plot = combined_plot, 
       width = 10, 
       height = 7.5, 
       dpi = 300)

  