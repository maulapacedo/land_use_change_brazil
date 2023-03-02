#LAND USE CHANGE IN 100 MUNICIPALITIES IN THE BRAZILIAN CERRADO:
#AN ANALYSIS BASED ON SOCIOECONOMIC INDICATORS
#28/02/2023

#Loading libraries 
pacman::p_load(dplyr, ggplot2, janitor, gridExtra, devtools, ggpubr, 
               tidyr, showtext, extrafont)

font_import()
loadfonts(device = "win")
loadfonts()

#Import Dataset 
dados_ISE <- read_excel("dados_brutos.xlsx", 2) #second sheet 

#Fonte 
font_add_google("Times New Roman")


###### ISE ######

#Cleaning Dataset
dados_ISE <- dados_ISE %>% 
  rename(vegetacao_nativa = `Vegetação Nativa`) %>% 
  rename(municipio = `Município (UF)`)

# Graphic Visualization 

### Theme Grey ####
# Scatterplot of ISE vs. NATIVE VEGATATION
plot_1 <-   ggplot(dados_ISE, aes(x = ISE, y = vegetacao_nativa))+
              geom_point(color = "dodgerblue3") +
              geom_smooth(method = lm, se = T, color = "darkred") +
              ylab("Native Vegetation") +
              theme_grey() +
              theme(text = element_text(size = 12,  family = "Times New Roman"))

plot_1

# Scatterplot of ISE vs. AGRICULTURE
plot_2 <-   ggplot(dados_ISE, aes(x = ISE, y = Agricultura)) +
              geom_point(color = "dodgerblue3") +
              geom_smooth(method = lm, se = T, color = "darkred") +
              ylab("Agriculture") + 
              theme_grey() +
              theme(text = element_text(size = 12,  family = "Times New Roman"))
plot_2

# Scatterplot of ISE vs. PASTURELAND          
plot_3 <-   ggplot(dados_ISE, aes(x = ISE, y = Pastagem))+
              geom_point(color = "dodgerblue3") +
              geom_smooth(method = lm, se = T, color = "darkred") +
              ylab("Pastureland") +
              theme_grey() + 
              theme(text=element_text(size = 12,  family = "Times New Roman"))

plot_3

figure_1 <- ggarrange(plot_1, plot_2, plot_3, 
                      ncol = 3, nrow = 1)

figure_1 <- annotate_figure(figure_1,
                bottom = text_grob("Data source: Mapbiomas, IBGE, Atlas Brasil", color = "black",
                                   hjust = 1, x = 1, face = "italic", size = 10))

figure_1

### Theme Minimal ####
# Scatterplot of ISE vs. NATIVE VEGATATION
plot_4 <-   ggplot(dados_ISE, aes(x = ISE, y = vegetacao_nativa))+
  geom_point(color = "dodgerblue3") +
  geom_smooth(method = lm, se = T, color = "darkred") +
  ylab("Native Vegetation") +
  theme_minimal() +
  theme(text = element_text(size = 12,  family = "Times New Roman"))

# Scatterplot of ISE vs. AGRICULTURE
plot_5 <-   ggplot(dados_ISE, aes(x = ISE, y = Agricultura)) +
  geom_point(color = "dodgerblue3") +
  geom_smooth(method = lm, se = T, color = "darkred") +
  ylab("Agriculture") + 
  theme_minimal() +
  theme(text = element_text(size = 12,  family = "Times New Roman"))

# Scatterplot of ISE vs. PASTURELAND          
plot_6 <-   ggplot(dados_ISE, aes(x = ISE, y = Pastagem))+
  geom_point(color = "dodgerblue3") +
  geom_smooth(method = lm, se = T, color = "darkred") +
  ylab("Pastureland") +
  theme_minimal() + 
  theme(text=element_text(size = 12,  family = "Times New Roman"))

figure_2 <- ggarrange(plot_4, plot_5, plot_6, 
                      ncol = 3, nrow = 1)

figure_2 <- annotate_figure(figure_2,
                            bottom = text_grob("Data source: Mapbiomas, IBGE, Atlas Brasil", color = "black",
                                               hjust = 1, x = 1, face = "italic", size = 10))

figure_2


### Theme Classic ####
# Scatterplot of ISE vs. NATIVE VEGATATION
plot_7 <-   ggplot(dados_ISE, aes(x = ISE, y = vegetacao_nativa))+
  geom_point(color = "dodgerblue3") +
  geom_smooth(method = lm, se = T, color = "darkred") +
  ylab("Native Vegetation") +
  theme_classic2() +
  theme(text = element_text(size = 12,  family = "Times New Roman"))

# Scatterplot of ISE vs. AGRICULTURE
plot_8 <-   ggplot(dados_ISE, aes(x = ISE, y = Agricultura)) +
  geom_point(color = "dodgerblue3") +
  geom_smooth(method = lm, se = T, color = "darkred") +
  ylab("Agriculture") + 
  theme_classic2() +
  theme(text = element_text(size = 12,  family = "Times New Roman"))

# Scatterplot of ISE vs. PASTURELAND          
plot_9 <-   ggplot(dados_ISE, aes(x = ISE, y = Pastagem))+
  geom_point(color = "dodgerblue3") +
  geom_smooth(method = lm, se = T, color = "darkred") +
  ylab("Pastureland") +
  theme_classic2() + 
  theme(text = element_text(size = 12,  family = "Times New Roman"))

figure_3 <- ggarrange(plot_7, plot_8, plot_9, 
                      ncol = 3, nrow = 1)

figure_3 <- annotate_figure(figure_3,
                            top = text_grob("Dispersion between ISE scores and percentages of areas covered with native vegetation, 
                                agriculture and pastureland", 
                                            face = "bold", 
                                            size = 14, 
                                            family = "Times New Roman"),
                            bottom = text_grob("Data source: ......", color = "black",
                                               hjust = 1, x = 1, face = "italic", size = 10))

figure_3

### Theme Grey ####
# Scatterplot of ISE vs. NATIVE VEGATATION
plot_10 <-   ggplot(dados_ISE, aes(x = ISE, y = vegetacao_nativa))+
  geom_point(color = "dodgerblue3") +
  geom_smooth(method = lm, se = F, color = "darkred") +
  ylab("Native Vegetation") +
  theme_grey() +
  theme(text = element_text(size = 12,  family = "Times New Roman"))

plot_10

# Scatterplot of ISE vs. AGRICULTURE
plot_11 <-   ggplot(dados_ISE, aes(x = ISE, y = Agricultura)) +
  geom_point(color = "dodgerblue3") +
  geom_smooth(method = lm, se = F, color = "darkred") +
  ylab("Agriculture") + 
  theme_grey() +
  theme(text = element_text(size = 12,  family = "Times New Roman"))
plot_11

# Scatterplot of ISE vs. PASTURELAND          
plot_12 <-   ggplot(dados_ISE, aes(x = ISE, y = Pastagem))+
  geom_point(color = "dodgerblue3") +
  geom_smooth(method = lm, se = F, color = "darkred") +
  ylab("Pastureland") +
  theme_grey() + 
  theme(text=element_text(size = 12,  family = "Times New Roman"))

plot_3

figure_1 <- ggarrange(plot_10, plot_11, plot_12, 
                      ncol = 3, nrow = 1)

figure_1 <- annotate_figure(figure_1,
                            top = text_grob("Dispersion between ISE scores and percentages of areas covered with native vegetation, 
                                agriculture and pastureland", 
                                            face = "bold", 
                                            size = 14, 
                                            family = "Times New Roman"),
                            bottom = text_grob("Data source: ......", color = "black",
                                               hjust = 1, x = 1, face = "italic", size = 10))

figure_1




