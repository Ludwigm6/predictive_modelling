# This script downloads and preprocesses SRTM Data for Fogo Island
library(raster)


fogo = raster::stack("data/landsat/landsat_fogo.tif")

dir.create("data/temp")

srtm = getData(name = "SRTM", lon = -24, lat = 15, path = "data/temp")
srtm2 = getData(name = "SRTM", lon = -24, lat = 16, path = "data/temp")

srtm = projectRaster(srtm, fogo)
srtm2 = projectRaster(srtm2, fogo)


srtm = crop(srtm, fogo)
srtm2 = crop(srtm2, fogo)

srtm = merge(srtm, srtm2)
plot(srtm)


writeRaster(srtm, "data/srtm/fogo_srtm.tif")


#### visualize height transect

tr = st_read("data/field_data/fogo_transect.gpkg")
transect = raster::extract(x = srtm, y = tr)
df = data.frame(Elevation = transect[[1]], x = seq(505,1,-1))


annos = df[df$x %in% c(122,130, 140,152,
                       266, 270,288, 295,310,
                       390, 420, 445),]
annos$Elevation = annos$Elevation + 30

ggplot(df, aes(x = x, y = Elevation))+
    scale_x_continuous(name = NULL, expand = c(0,0))+
    scale_y_continuous(name = "Elevation [m]", position = "right", expand = c(0,0))+
    geom_area(fill = "goldenrod1", alpha = 0.7)+
    geom_line(color = "black", lwd = 0.7)+
    geom_point(data = annos, color = "black", fill = "red", shape = 25, size = 3)+
    theme_pubr()+
    theme(axis.line = element_blank(),
          panel.border = element_rect(fill = NA),
          axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          axis.title.y.right = element_text(angle = 90))
ggsave("results/fogo_plots_transect.png")    


