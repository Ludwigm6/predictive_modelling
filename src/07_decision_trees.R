# elevation + ndvi vs. vegetation cover

library(raster)
library(sf)
library(ggplot2)
library(viridis)


plots = st_read("data/field_data/fogo_plots.gpkg")

landsat = stack("data/landsat/landsat_fogo.tif")
ndvi = (landsat[[5]] - landsat[[4]]) / (landsat[[5]] + landsat[[4]])


plots$NDVI = raster::extract(ndvi, plots)
plots$NIR = raster::extract(landsat[[5]], plots)

ggplot(plots, aes(x = NDVI, y = Vegetation_Cover, color = ELEVgps))+
    geom_point(size = 3)


ggplot(plots, aes(x = ELEVgps, y = NDVI, fill = Vegetation_Cover)) + 
    scale_x_continuous(name = "Elevation [m]")+
    geom_point(size = 4, color = "black", shape = 21)+
    scale_fill_gradient(name = "Vegetation\nCover [%]", low = "grey90", high = "#008B00", na.value = "transparent")+
    ggtitle("Elevation + NDVI vs. Vegetation Cover")+
    theme_pubr()+
    theme(legend.position = "right")

ggsave("results/elevation_ndvi_vegetation.png")





# decision tree ----
library(tree)
library(parsnip)
library(parttree)
library(rpart)
library(rpart.plot)



plots$Vegetation_Coverf = as.factor(20*round(plots$Vegetation_Cover/20))
plots$Elevation = plots$ELEVgps



ti_tree = decision_tree() %>%
    set_engine("rpart") %>%
    set_mode("classification") %>%
    fit(Vegetation_Coverf ~ Elevation + NDVI, data = plots)

scales::seq_gradient_pal("grey90", "#008B00", "Lab")(seq(0,1,length.out=5))

    
ggplot(plots, aes(x = NDVI, y = ELEVgps, color = Vegetation_Cover)) + 
    scale_x_continuous(name = "NDVI")+
    scale_y_continuous(name = "Elevation [m]")+
    geom_parttree(data = ti_tree, aes(fill = Vegetation_Coverf),alpha = 0.4, show.legend = FALSE, inherit.aes = FALSE)+
    scale_fill_manual(values = c("0"= "#E5E5E5","40" =   "#B8CFAF","60" =  "#8AB97B","80" =  "#58A247","100" =  "#008B00"))+
    geom_point(size = 4)+
    geom_point(size = 5, color = "black", shape = 21, fill = "transparent")+
    scale_color_gradient(name = "Vegetation\nCover [%]", low = "grey90", high = "#008B00", na.value = "transparent")+
    ggtitle("Elevation + NDVI vs. Vegetation Cover: Decision Tree")+
    theme_pubr()+
    coord_flip()+
    theme(legend.position = "right")
    

ggsave("results/elevation_ndvi_vegetation_decisiontree.png")




png("results/decision_tree.png")
rpart.plot::rpart.plot(ti_tree$fit, type = 0, extra = 0, 
                       box.palette = as.list(scales::seq_gradient_pal("grey90", "#008B00", "Lab")(seq(0,1,length.out=5))))
dev.off()


p = stack(srtm, ndvi)
names(p) = c("Elevation", "NDVI")


# proper regression tree for prediction



ti_tree2 = decision_tree() %>%
    set_engine("rpart") %>%
    set_mode("regression") %>%
    fit(Vegetation_Cover ~ Elevation + NDVI, data = plots)




pval = as.data.frame(p)
tree_prediction = stats::predict(ti_tree2, pval)
tree_prediction = setValues(p, as.numeric(tree_prediction$.pred))



ggR(tree_prediction, geom_raster = TRUE)+
    scale_fill_gradient(name = "Vegetation\nCover", low = "grey90", high = "#008B00", na.value = "transparent")+
    scale_x_continuous(name = NULL, expand = c(0,0))+
    scale_y_continuous(name = NULL, expand = c(0,0))+
    theme_pubr()+
    theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
          axis.line = element_blank(),
          legend.position = "bottom", axis.text = element_text(color = "black"),
          panel.border = element_rect(fill = NA))
ggsave("results/fogo_prediction_regressiontree.png")

