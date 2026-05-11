install.packages("tidyverse")
install.packages("paletteer")
install.packages("ggpubr")
install.packages("lmboot")
library(tidyverse)
library(paletteer)
library(ggpubr)
library(lmboot)

#Bar charts for baselines. Easiest graph to do if we keep the datasheet wide,
#rather than keeping it long. This way we also do not have to account for weight!


#Read in file
Baseline <- read.csv(file = "Baseline Data.csv", header = T)


#Grouping the average by genus
sum.baseline <- Baseline %>%
  group_by(Genus) %>%
  summarise(
    N = n(),
    mean = mean(Percent.change),
    sd = sd(Percent.change),
    se = sd / sqrt(N),
  )

#Making the BAR CHART!

ggplot(data=sum.baseline, aes(x = Genus, y = mean, fill = Genus))+
  geom_col(position="dodge", width = 0.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1, position= position_dodge((0.5))) +
  scale_fill_manual(values = c("Blank" = "grey85", "Carcinus"="turquoise4","Hemigrapsus"="darkgoldenrod"))+
  labs(x=" ", y = "Percent Change", fill = "Genus")+
  theme_bw()+
  theme(legend.position="none", text=element_text(size=16,family="serif"),)


#Making a line graph. To do this, the datasheet has to be 
#reconfigured to by long, not wide. It can stay wide if we're doing % change !!

Baseline_line <- read.csv(file = "Baseline.csv", header = T)

sum.baselineII <- Baseline_line %>%
  group_by(Time, Genus) %>%
  summarise(
    N = n(),
    mean = mean(Fluorescence),
    sd = sd(Fluorescence),
    se = sd / sqrt(N),
  )

#Making line plot (Rachel did this one!)
ggplot(data=sum.baselineII, aes(x = Time, y = mean, group= Genus, color = Genus))+
  geom_line()+
  geom_point(size=1.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  stat_regline_equation(label.x= c(10),label.y = c(4500, 5500, 6500)) + 
  theme_minimal()+
  labs(x = "Time",
       y= "Fluorescence")+
  theme(text=element_text(size=12,family="serif"))+
  scale_color_manual(values = c("Blank" = "grey85", "Carcinus"="turquoise4","Hemigrapsus"="darkgoldenrod"))


#Line graph normalized by weight; blanks are divided by the weight of the amount of water, 
#So either 8 grams or 40 grams, depending on which blank it was

sum.normal.fluor <- Baseline_line %>%
  group_by(Time, Genus) %>%
  summarise(
    N = n(),
    mean = mean(Normal.Fluor),
    sd = sd(Normal.Fluor),
    se = sd / sqrt(N),
  )

ggplot(data=sum.normal.fluor, aes(x = Time, y = mean, group= Genus, color = Genus))+
  geom_line()+
  geom_point(size=1.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1) +
  geom_smooth(method = "lm", se = FALSE, linewidth=0.5) +
  stat_regline_equation(label.x= c(10),label.y = c(2500, 3000, 3500)) + 
  theme_minimal()+
  labs(x = "Time",
       y= "Fluorescence/gram body weight")+
  theme(text=element_text(size=12,family="serif"))+
  scale_color_manual(values = c("Blank" = "grey85", "Carcinus"="turquoise4","Hemigrapsus"="darkgoldenrod"))
 
