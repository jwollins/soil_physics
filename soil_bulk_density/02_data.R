### ERP bulk density, moisture content and stone content data
### J Collins 
### 2024-10-22

## 02 DATA ####

### 2.1 PACKAGES ####

setwd(dir = "~/Documents/ERP/scripts/erpsoiltools/")
source(file = "bulk_density/01_packages.R")


### 2.2 DATA ####

setwd(dir = "~/jcollins@earthroverprogram.org - Google Drive/Shared drives/DATA/FIELD_DEPLOYMENTS/")

dat <- read_xlsx(path = "2024-06-Joesfield/soil_data/soil_pit/soil_pit_data.xlsx", sheet = 1, col_names = TRUE)


dat$date <- as.Date("2024-09-01")

dat$cylinder_volume_cm3 <- 100

stone_bd_g_cm3 <- 2.5


## Moisture Content ####

dat$moisture_content <- ((dat$fresh_mass - dat$dry_mass) / dat$dry_mass) * 100


## stone volume cm3 ####

#=[@[stone_mass_g]]/[@[stone_bd_g/cm3]]

# dat$stone_mass_g <- dat$ / stone_bd_g_cm3


## Bulk Density ####

# =[@[dry_soil_mass_g]]/[@[cylinder_volume_cm3]]

dat$bulk_density_g_cm3 <- dat$dry_mass / dat$cylinder_volume_cm3

dir.create(path = "2024-06-Joesfield/soil_data/bulk_density/")
write.csv(x = dat, file = "2024-06-Joesfield/soil_data/bulk_density/bulk_density_data.csv")








