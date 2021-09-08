library(mgcv)
library(RStoolbox)
library(viridis)
library(Metrics)
library(tidyverse)
df = st_read("data/field_data/fogo_plots.gpkg")
df = st_drop_geometry(df)

# split into train and test
df$Test = 0
df$Test[sample(154, 50)] = 1
df$Test = as.factor(df$Test)


# build models with train set
lm_model = lm(Vegetation_Cover ~ ELEVgps, data = df[df$Test == 0,])
gam_model3 = mgcv::gam(Vegetation_Cover ~ poly(ELEVgps, 3), data = df[df$Test == 0,])
gam_model15 = mgcv::gam(Vegetation_Cover ~ poly(ELEVgps, 15), data = df[df$Test == 0,])

# predict on test data (here: predict all rows and filter for test data later)
df$pred_lm = predict(lm_model, df)
df$pred_gam3 = predict(gam_model3, df)
df$pred_gam15 =predict(gam_model15, df)

# filter test data
df_test = df[df$Test == 1,]

# calculate independent RMSE (from test data only)
Metrics::rmse(df_test$pred_lm, df_test$Vegetation_Cover)
Metrics::rmse(df_test$pred_gam3, df_test$Vegetation_Cover)
Metrics::rmse(df_test$pred_gam15, df_test$Vegetation_Cover)



df %>% filter(Vegetation_Cover > 0) %>% 
    ggplot(aes(x = ELEVgps, y = Vegetation_Cover))+
    scale_x_continuous(name = "Elevation [m]")+
    scale_y_continuous(name = "Vegetation Cover [%]")+
    geom_point()+
    geom_point(data = df_test, color = "red")+
    geom_smooth(method = "lm", se = FALSE)+
    stat_regline_equation(aes(label = ..rr.label..))+
    ggtitle("Elevation vs. Vegetation Cover: Linear Model")+
    theme_pubr()

ggsave("results/elevation_vegetation_lm_test.png")

df %>% filter(Vegetation_Cover > 0) %>% 
    ggplot(aes(x = ELEVgps, y = Vegetation_Cover))+
    scale_x_continuous(name = "Elevation [m]")+
    scale_y_continuous(name = "Vegetation Cover [%]")+
    geom_point()+
    geom_point(data = df_test, color = "red")+
    geom_smooth(method = "gam", se = FALSE, formula = y ~ poly(x, 3))+
    stat_regline_equation(aes(label = ..rr.label..), formula = y ~ poly(x,3))+
    ggtitle("Elevation vs. Vegetation Cover: Polynomial (3rd)")+
    theme_pubr()
ggsave("results/elevation_vegetation_gam3_test.png")

df %>% filter(Vegetation_Cover > 0) %>% 
    ggplot(aes(x = ELEVgps, y = Vegetation_Cover))+
    scale_x_continuous(name = "Elevation [m]")+
    scale_y_continuous(name = "Vegetation Cover [%]")+
    geom_point()+
    geom_point(data = df_test, color = "red")+
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_smooth(method = "gam", se = FALSE, formula = y ~ poly(x, 15))+
    stat_regline_equation(aes(label = ..rr.label..), formula = y ~ poly(x,16))+
    ggtitle("Elevation vs. Vegetation Cover: Polynomial (15th)")+
    theme_pubr()
ggsave("results/elevation_vegetation_gam15_test.png")




# lm with residuals

df %>% filter(Test == 0) %>% 
    ggplot(aes(x = ELEVgps, y = Vegetation_Cover))+
    scale_x_continuous(name = "Elevation [m]")+
    scale_y_continuous(name = "Vegetation Cover [%]")+
    geom_point()+
    geom_smooth(method = "lm", se = FALSE)+
    geom_point(data = df_test, color = "red")+
    geom_segment(data = df_test, aes(yend = pred_lm, xend = ELEVgps), color = "red")+
    ggtitle("Elevation vs. Vegetation Cover: Linear Model")+
    theme_pubr()
ggsave("results/elevation_vegetation_lm_test_residuals.png")






