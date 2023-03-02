#LAND USE CHANGE IN 100 MUNICIPALITIES IN THE BRAZILIAN CERRADO:
#AN ANALYSIS BASED ON SOCIOECONOMIC INDICATORS
#01/03/2023

#Loading packages 

pacman::p_load(dplyr, ggplot2, janitor, gridExtra, devtools, ggpubr, 
               tidyr, showtext, extrafont, scales, lubridate)

#Changes in land use class areas between 1985 and 2019 

###### Land Use class ######

#Cleaning dataset 

dados_brutos <- read_excel("dados_brutos.xlsx")

dados_cobertura_long <- dados_brutos %>% 
  gather(Year, soil_usage, 3:37) %>% 
  rename(`Land Usage`  = `Cobertura e uso da terra`) %>% 
  mutate(
    `Land Usage`  = case_when(
    `Land Usage` == "Agricultura" ~ "Agriculture", 
    `Land Usage` == "Vegetação Nativa" ~ "Native Vegetation",
    `Land Usage` == "Pastagem" ~ "Pastureland",
    TRUE ~ as.character(`Land Usage`)
  ))
  
#Changing year to Date 

dados_cobertura_long$Year <- leap_year(dados_cobertura_long$Year)

dados_cobertura_long$Year <- as.Date(as.character(dados_cobertura_long$Year), format = "%Y")
dados_cobertura_long$Year <- year(dados_cobertura_long$Year) 

#Goiás 
cobertura_GO <- dados_cobertura_long %>% 
  filter(Estado == "GO")

#Mato Grosso 
cobertura_MT <- dados_cobertura_long %>% 
  filter(Estado == "MT")

#Mato Grosso do Sul 
cobertura_MS <- dados_cobertura_long %>% 
  filter(Estado == "MS")

#Minas Gerais 
cobertura_MG <- dados_cobertura_long %>% 
  filter(Estado == "MG")



# Graphic Visualization #

plot_GO <- cobertura_GO %>%
              ggplot(aes(x = Year, y = soil_usage, group = `Land Usage`, color = `Land Usage`)) +
              geom_line() +
              theme_grey() +
              labs(title = "Goiás (GO)") +
              theme(text = element_text(size = 12,  family = "Times New Roman")) +
              theme(plot.title = element_text(lineheight=.8, face = "bold")) +
              theme(axis.text.x = element_text(size = 8,  family = "Times New Roman")) +
              scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
              ylab("Land Use (Mha)")

plot_GO
            
plot_MG <-  cobertura_MG %>%
              ggplot(aes(x = Year, y = soil_usage, group = `Land Usage`, color = `Land Usage`)) +
              geom_line() +
              theme_grey() +
              labs(title = "Minas Gerais (MG)") +
              theme(text = element_text(size = 12,  family = "Times New Roman")) +
              theme(plot.title = element_text(lineheight = .8, face = "bold")) +
              theme(axis.text.x = element_text(size = 8,  family = "Times New Roman")) +
              scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
              ylab("Land Use (Mha)")

plot_MG
            
plot_MS <-  cobertura_MS %>%
                ggplot(aes(x = Year, y = soil_usage, group = `Land Usage`, color = `Land Usage`)) +
                geom_line() +
                labs(title = "Mato Grosso do Sul (MS)") +
                theme(text = element_text(size = 12,  family = "Times New Roman")) +
                theme(plot.title = element_text(lineheight = .8, face = "bold")) +
                theme(axis.text.x = element_text(size = 8,  family = "Times New Roman")) +
                scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
                ylab("Land Use (Mha)")

plot_MS
            
plot_MT <-  cobertura_MT %>%
                ggplot(aes(x = Year, y = soil_usage, group = `Land Usage`, color = `Land Usage`)) +
                geom_line() +
                labs(title = "Mato Grosso (MT)") +
                theme(text = element_text(size = 12,  family = "Times New Roman")) +
                theme(plot.title = element_text(lineheight = .8, face = "bold")) +
                theme(axis.text.x = element_text(size = 8,  family = "Times New Roman")) +
                scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) +
                ylab("Land Use (Mha)")

plot_MT

figure_1 <- ggarrange(plot_GO, plot_MG,
                      ncol = 2, nrow = 1, 
                      common.legend = TRUE, 
                      legend = "bottom"
                      )
figure_1

figure_2 <- ggarrange(plot_MS, plot_MT,
                      ncol = 2, nrow = 1, 
                      common.legend = TRUE, 
                      legend = "bottom")


figure_2


