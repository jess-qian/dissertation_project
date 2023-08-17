install.packages("segmented")
install.packages("readxl")
install.packages("ggplot2") 

library(segmented)
library(readxl)
library(ggplot2)

#import data
data <- read_excel("~/desktop/results/forest3.xlsx")
data <- data.frame(data)

# Piecewise Regression
seg_model <- segmented(lm(IIC ~ forest_cover, data = data), seg.Z = ~ forest_cover)
summary(seg_model)

plot(data$forest_cover, data$IIC, main = "Site 3",
     xlab = "Forest Cover (0.0-1.0)", ylab = "IIC", pch = 16) +
  lines(data$forest_cover, seg_model$fit, col = "red3", lwd = 2)+
  lines(seg_model, col = "red3")+theme_classic()


ggsave(filename = "~/desktop/results/piecewise.png", plot = reg, width = 6, height = 4, dpi = 300,  bg = "white")
