# 1. Install packages:
#    - untidydata
#    - plot3D
#    - xaringan
# 2. Load language_diversity dataset
# 3. Explore variables, tidy (long to wide)
# 4. Check normality, transform, plot
# 5. Fit model (MRC, 3 params)
# 6. Write up results
# 7. Convert to html presentation (xaringan)
# 8. Create github repo, make website





library(tidyverse)
library(untidydata)



glimpse(language_diversity)
head(language_diversity)

ld <- language_diversity %>% 
  filter(., Continent == "Africa") %>% 
  pivot_wider(names_from = "Measurement", values_from = "Value") %>% 
  mutate(., logPop = log(Population))


my_mod <- lm(Langs ~ Area + logPop, data = ld)
summary(my_mod)
plot(my_mod, which = 1:4)
ds4ling::diagnosis(my_mod)

hist(ld$Population)
hist(ld$logPop)



ggplot(ld, aes(x = logPop, y = Langs, 
               color = Area, label = Country)) + 
  geom_text() + 
  geom_smooth(method = lm)


x <- ld$logPop
y <- ld$Area
z <- ld$Langs

plot3D::scatter3D(x, y, z, 
    pch = 21, cex = 1, expand = 0.75, colkey = F,
    theta = 45, phi = 20, ticktype = "detailed",
    xlab = "logPop", ylab = "Area", zlab = "Langs")
