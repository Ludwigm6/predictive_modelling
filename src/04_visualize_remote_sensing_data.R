library(RStoolbox)
library(viridis)
library(raster)
library(ggplot2)


srtm = raster::raster("data/srtm/fogo_srtm.tif")
landsat = stack("data/landsat/landsat_fogo.tif")

ndvi = (landsat[[5]] - landsat[[4]]) / (landsat[[5]] + landsat[[4]])



ggR(srtm, geom_raster = TRUE)+
    scale_fill_gradientn(name = "Elevation [m]", colors = cptcity::cpt("esri_hypsometry_eu_europe_4", n = 20), na.value = "transparent")+
    scale_x_continuous(name = NULL, expand = c(0,0))+
    scale_y_continuous(name = NULL, expand = c(0,0))+
    theme_pubr()+
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.line = element_blank(),
          legend.position = "bottom", axis.text = element_text(color = "black"),
          panel.border = element_rect(fill = NA))

ggsave("results/fogo_elevation.png")



ggR(ndvi, geom_raster = TRUE)+
    scale_fill_gradientn(name = "NDVI", colors = cptcity::cpt("rafi_sky_03", n = 20),na.value = "transparent")+
    scale_x_continuous(name = NULL, expand = c(0,0))+
    scale_y_continuous(name = NULL, expand = c(0,0))+
    theme_pubr()+
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.line = element_blank(),
          legend.position = "bottom", axis.text = element_text(color = "black"),
          panel.border = element_rect(fill = NA))

ggsave("results/fogo_ndvi.png")
