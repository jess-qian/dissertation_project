install.packages("readxl")
install.packages("ggplot2") 
library(readxl)
library(ggplot2)

#import data
data <- read_excel("~/desktop/results/forest3.xlsx")
data <- data.frame(data)

#plot temporal changes in landscape metrics 
metric <- ggplot(data[-nrow(data), ], aes(x = year))+
  geom_smooth(aes(y = Site_1, color = "Site 1", linetype = "Site 1"), linewidth = 0.6) +
  geom_smooth(aes(y = Site_2, color = "Site 2", linetype = "Site 2"), linewidth = 0.6)+
  geom_smooth(aes(y = Site_3, color = "Site 3", linetype = "Site 3"), linewidth = 0.6)+
  labs(x = "Year", y = "ENN_MN (m)", title = "Mean Nearest Neighbourhood Distance", color = "Legend") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 15, margin = margin(t = 15)), plot.title = element_text(hjust = 0.5, size = 17))+
  scale_color_manual(name = "Legend",
                     values = c("Site 1" = "green4", "Site 2" = "blue2", "Site 3" = "red3"),
                     labels = c("Site 1", "Site 2", "Site 3"))+
  scale_linetype_manual(name = "Legend",
                        values = c("Site 1" = "dashed", "Site 2" = "dashed", "Site 3" = "dashed"),
                        labels = c("Site 1", "Site 2", "Site 3"))+
  xlab("Year") +
  xlim(1985, 2050)+
  geom_point(data = subset(data, year == 2050), aes(y = Site_1), shape = 16, size = 3, color = "green4") +
  geom_point(data = subset(data, year == 2050), aes(y = Site_2), shape = 16, size = 3, color = "blue2") +
  geom_point(data = subset(data, year == 2050), aes(y = Site_3), shape = 16, size = 3, color = "red3")+
  scale_x_continuous(breaks = seq(1980, 2050, by = 10), limits = c(1980, 2050))


ggsave(filename = "~/desktop/results/ENN.png", plot = metric, width = 6, height = 4, dpi = 300,  bg = "white")


 
 # Create a new data frame for custom labels
 custom_labels <- data.frame(year = c(1985, 2021, 2050),
                             IIC_custom = c(0.18, 0.035, 0.078),
                             forest_cover_custom = c(0.6746, 0.4444, 0.4535))
 
 # Create a new column "IIC_custom" with specific values for 1985, 2021, and 2050
 data$IIC_custom <- data$IIC
 data$IIC_custom[data$year == 2021] <- 0.035
 data$IIC_custom[data$year == 2050] <- 0.078
 data$IIC_custom[data$year == 1985] <- 0.18
 # Create a new column "forest_cover_custom" with specific values for 1985, 2021, and 2050
 data$forest_cover_custom <- data$forest_cover
 data$forest_cover_custom[data$year == 2021] <- 0.4444
 data$forest_cover_custom[data$year == 2050] <- 0.4535
 data$forest_cover_custom[data$year == 1985] <- 0.6746
 
 #plot temporal changes in foret cover and IIC
forest_IIC <- ggplot(data[-nrow(data), ], aes(x = year)) +
   geom_line(aes(y = IIC_custom, color = "IIC"), size = 0.6) +
   geom_line(aes(y = forest_cover_custom, color = "Forest Cover"), size = 0.6) +
   scale_y_continuous(
     name = "IIC",
     limits = c(0, 0.8),
     breaks = seq(0, 0.8, by = 0.2),
     sec.axis = sec_axis(~.* 100, name = "Forest Cover (%)", breaks = seq(0, 80, by = 20))
   ) +
   labs(title = "Site 3", color = "Legend") +
   xlab("Year") +
   ylab(NULL) +
   xlim(1985, 2050) +
   geom_point(data = subset(data, year %in% c(1985, 2021, 2050)), aes(y = IIC_custom), shape = 16, size = 2, color = "blue2") +
   geom_text(data = subset(data, year %in% c(1985, 2021, 2050)), aes(y = IIC_custom, label = sprintf("%.3g", IIC_custom)), vjust = -1, hjust= 0.8, color = "blue2") +
   geom_text(data = subset(data, year == 2050), aes(y = IIC_custom, label = sprintf("%.3g", IIC_custom)), hjust = -2, vjust = -1, color = "blue2") +
   geom_point(data = subset(data, year %in% c(1985, 2021, 2050)), aes(y = forest_cover_custom), shape = 16, size = 2, color = "green4") +
   geom_text(data = subset(data, year %in% c(1985, 2021, 2050)), aes(y = forest_cover_custom, label = sprintf("%.2f%%", forest_cover_custom * 100)), hjust = 0.7, vjust = -1.5, color = "green4") +
   scale_x_continuous(breaks = seq(1980, 2050, by = 10), limits = c(1980, 2050)) +
   theme_classic() +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_text(size = 12),
         axis.title.x = element_text(size = 15, margin = margin(t = 15)), plot.title = element_text(hjust = 0.5, size = 17)) +
   scale_color_manual(name = "Legend",
                      values = c("IIC" = "blue2", "Forest Cover" = "green4"),
                      labels = c("Forest Cover", "IIC"))
 
 

ggsave(filename = "~/desktop/results/forest3.png", plot = IIC, width = 6, height = 4, dpi = 300,  bg = "white")
