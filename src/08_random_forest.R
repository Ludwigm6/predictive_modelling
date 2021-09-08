# random forest
library(raster)
library(sf)
library(ggplot2)
library(viridis)
library(caret)
library(RStoolbox)
library(ggpubr)


plots = st_read("data/field_data/fogo_plots.gpkg")

landsat = stack("data/landsat/landsat_fogo.tif")
landsat$ndvi = (landsat[[5]] - landsat[[4]]) / (landsat[[5]] + landsat[[4]])


df = plots[,"Vegetation_Cover"]
e = raster::extract(landsat, plots)

df = cbind(e, df)

df = st_drop_geometry(df)

rfmodel = train(Vegetation_Cover ~ ., data = df, method = "rf", 
                trControl = trainControl(method = "cv", number = 5))
rfmodel

fogo_vegetation = raster::predict(landsat, rfmodel)

plot(fogo_vegetation)




ggR(fogo_vegetation, geom_raster = TRUE)+
    scale_fill_gradient(name = "Vegetation\nCover", low = "grey90", high = "#008B00", na.value = "transparent")+
    scale_x_continuous(name = NULL, expand = c(0,0))+
    scale_y_continuous(name = NULL, expand = c(0,0))+
    theme_pubr()+
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.line = element_blank(),
          legend.position = "bottom", axis.text = element_text(color = "black"),
          panel.border = element_rect(fill = NA))


ggsave("results/fogo_prediction_rf.png")




