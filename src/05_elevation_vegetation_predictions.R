# model elevation vs vegetation cover 
library(sf)
library(mgcv)
library(RStoolbox)
library(viridis)
library(raster)
library(ggplot2)
library(ggpubr)

df = st_read("data/field_data/fogo_plots.gpkg")
df = st_drop_geometry(df)

srtm = raster("data/srtm/fogo_srtm.tif")
names(srtm) = "ELEVgps"

lm_model = lm(Vegetation_Cover ~ ELEVgps, data = df)
fogo_lm = raster::predict(object = srtm, lm_model)

gam_model3 = mgcv::gam(Vegetation_Cover ~ poly(ELEVgps, 3), data = df)
fogo_gam3 = raster::predict(object = srtm, gam_model3)

gam_model15 = mgcv::gam(Vegetation_Cover ~ poly(ELEVgps, 15), data = df)
fogo_gam15 = raster::predict(object = srtm, gam_model15)


ggR(fogo_lm, geom_raster = TRUE)+
    scale_fill_gradient(name = "Vegetation\nCover", low = "grey90", high = "#008B00", na.value = "transparent", limits = c(0,100))+
    scale_x_continuous(name = NULL, expand = c(0,0))+
    scale_y_continuous(name = NULL, expand = c(0,0))+
    theme_pubr()+
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.line = element_blank(),
          legend.position = "bottom", axis.text = element_text(color = "black"),
          panel.border = element_rect(fill = NA))

ggsave("results/fogo_prediction_lm.png", dpi = 1000)

ggR(fogo_gam3, geom_raster = TRUE)+
    scale_fill_gradient(name = "Vegetation\nCover", low = "grey90",
                        high = "#008B00", na.value = "transparent", limits = c(0,100))+
    scale_x_continuous(name = NULL, expand = c(0,0))+
    scale_y_continuous(name = NULL, expand = c(0,0))+
    theme_pubr()+
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.line = element_blank(),
          legend.position = "bottom", axis.text = element_text(color = "black"),
          panel.border = element_rect(fill = NA))

ggsave("results/fogo_prediction_gam3.png", dpi = 1000)

ggR(fogo_gam15, geom_raster = TRUE)+
    scale_fill_gradient(name = "Vegetation\nCover", low = "grey90", high = "#008B00", na.value = "transparent",
                        limits = c(0,100))+
    scale_x_continuous(name = NULL, expand = c(0,0))+
    scale_y_continuous(name = NULL, expand = c(0,0))+
    theme_pubr()+
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.line = element_blank(),
          legend.position = "bottom", axis.text = element_text(color = "black"),
          panel.border = element_rect(fill = NA))

ggsave("results/fogo_prediction_gam15.png", dpi = 1000)


