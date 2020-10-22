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

ggplot(data = chaffsummary, aes(x = sex, y = mass) ) +
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

ggplot() +
  geom_point(data = chaffsummary, aes(x = sex, y = mass)) +
  geom_errorbar(chaffsummary, 
                aes(ymin = mass, 
                    ymax = mass), 
                width = 0.2, size = 1) + 
  geom_errorbar(chaffsummary,
                aes(ymin = mass - se, 
                    ymax = mass + se), 
                width = 0.3) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))

ggplot() +
  geom_point(data = chaff, aes(x = sex, y = mass)) +
  geom_jitter(width = 0.1, colour = "#8c8c8c") +
  geom_errorbar(data = chaffsummary, 
                aes(ymin = mass - se, ymax = mass + se),
                width = 0.3) +
  geom_errorbar(data = chaffsummary, 
                aes(ymin = mass, ymax = mass),
                width = 0.2) +
  ylab("Chaffinch Mass") +
  xlab(NULL) +
  ylim(0, 30) +
  scale_x_discrete(labels = c("Male", "Female")) +
  theme_classic()
