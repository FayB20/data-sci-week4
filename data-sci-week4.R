# loading required packages
library(ggplot2)
library(Rmisc)
library(ggpubr)

# loading in the tidy chaff data:
chaff <- read.table("chaff.txt", header = T)

# checking the chaff data structure:
str(chaff)

# looking at the chaff summary:
chaffsummary <- summarySE(chaff, measurevar = "mass",
                             groupvar = "sex")
chaffsummary

# analysing the data using paired sample t-test
t.test(data = chaff, mass ~ sex, paired = TRUE)
# Paired t-test

# data:  mass by sex
# t = -2.5168, df = 19, p-value = 0.02098
# so there is a significant difference in mass between male and female chaffinches.

fig1 <- ggplot(data = chaffsummary, aes(x = sex, y = mass) ) +
  geom_errorbar(aes( ymin = mass, 
                     ymax = mass), 
                width = .5, 
                colour = "red",
                size = 1) +
  geom_errorbar(aes( ymin = mass - se, 
                     ymax = mass + se), 
                width = .7, 
                colour = "red") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))

units <- "in"  
fig_w <- 3.5
fig_h <- fig_w
dpi <- 300
device <- "tiff" 

ggsave("fig1",
       plot = fig1,
       device = device,
       width = fig_w,
       height = fig_h,
       units = units,
       dpi = dpi)
