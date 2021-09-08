library(tidyverse)
library(ggpubr)
library(sf)

df = st_read("data/field_data/fogo_plots.gpkg")


df %>% filter(Vegetation_Cover > 0) %>% 
    ggplot(aes(x = ELEVgps, y = Vegetation_Cover))+
    scale_x_continuous(name = "Elevation [m]")+
    scale_y_continuous(name = "Vegetation Cover [%]")+
    geom_point()+
    geom_smooth(method = "lm", se = FALSE)+
    stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label..,sep = "~~~~~~~")))+
    stat_cor(aes(label = ..p.label..), label.y = 150)+
    ggtitle("Elevation vs. Vegetation Cover: Linear Model")+
    theme_pubr()

ggsave("results/elevation_vegetation_lm.png")

df %>% filter(Vegetation_Cover > 0) %>% 
    ggplot(aes(x = ELEVgps, y = Vegetation_Cover))+
    scale_x_continuous(name = "Elevation [m]")+
    scale_y_continuous(name = "Vegetation Cover [%]")+
    geom_point()+
    geom_smooth(method = "gam", se = FALSE, formula = y ~ poly(x, 3))+
    stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~~~")), formula = y ~ poly(x,3))+
    ggtitle("Elevation vs. Vegetation Cover: Polynomial (3rd)")+
    theme_pubr()
ggsave("results/elevation_vegetation_gam3.png")

df %>% filter(Vegetation_Cover > 0) %>% 
    ggplot(aes(x = ELEVgps, y = Vegetation_Cover))+
    scale_x_continuous(name = "Elevation [m]")+
    scale_y_continuous(name = "Vegetation Cover [%]")+
    geom_point()+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth(method = "gam", se = FALSE, formula = y ~ poly(x, 15))+
    stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~~~")),
                          formula = y ~ poly(x,16))+
    stat_regline_equation(aes(label = ..rr.label..), formula = y ~ poly(x,16), label.y = 150)+
    ggtitle("Elevation vs. Vegetation Cover: Polynomial (15th)")+
    theme_pubr()
ggsave("results/elevation_vegetation_gam15.png")


# artifically good linear data

dfa = data.frame(Response = runif(n = 100, 5, 100))
dfa$Predictor = dfa$Response * rnorm(100,5,1)

dfa %>% 
    ggplot(aes(x = Predictor, y = Response))+
    scale_x_continuous(name = "Predictor")+
    scale_y_continuous(name = "Response")+
    geom_point()+
    geom_smooth(method = "lm", se = FALSE)+
    stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~~~~")))+
    ggtitle("Artificial Data: Linear Model")+
    theme_pubr()
ggsave("results/lm_artificial.png")