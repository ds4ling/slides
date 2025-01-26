# 1. load data
# Load libraries
source(here::here("scripts", "libs.R"))

ice_cream <- read_csv(here("data", "poisson_data.csv")) %>% 
  mutate(city = as.factor(city), 
         temp_c = temp - mean(temp))


# 2. check structure
head(ice_cream)
summary(ice_cream)

p_ic <- ice_cream %>% 
  ggplot() + 
  aes(x = temp, y = units, color = city) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "glm", method.args = list(family = "poisson")) + 
  scale_color_viridis_d(option = "C", begin = 0.2, end = 0.9) + 
  theme_dark() + 
  theme(legend.position = c(0.15, 0.85))

ggsave(
  filename = here("figs", "p_ic.png"), 
  plot = p_ic, 
  dpi = 300
)

# 3. fit inclusive and nested models
#    test for interactions/main effects




# 4. summary of best model




# 5. write up of output



# 6. generate and save plot




