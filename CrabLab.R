install.packages("tidyverse")
install.packages("paletteer")
install.packages("ggpubr")
install.packages("lmboot")
install.packages("patchwork")
library(tidyverse)
library(paletteer)
library(ggpubr)
library(lmboot)
library(patchwork)

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
    mean = mean(Norm.Minus),
    sd = sd(Norm.Minus),
    se = sd / sqrt(N),
  )

#Making line plot (Rachel did this one!)
ggplot(data=sum.baselineII, aes(x = Time, y = mean, group= Genus, color = Genus))+
  geom_line()+
  geom_point(size=1.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.5) +
  stat_regline_equation(label.x= c(10),label.y = c(2500,3000)) + 
  theme_minimal()+
  labs(x = "Time",
       y= "Fluorescence")+
  theme(text=element_text(size=12,family="serif"))+
  scale_color_manual(values = c("Carcinus"="turquoise4","Hemigrapsus"="darkgoldenrod"))


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

#Week 2 & 3 Data

Wide <- read.csv("Week2Wide.csv")

sum.wide <- Wide %>%
  group_by(Species, Temp, Pre.Post) %>%
  summarise(
    N = n(),
    mean = mean(Percent.Change),
    sd = sd(Percent.Change),
    se = sd / sqrt(N),
  )



#change the order of the variables so they're not alphabetical

sum.wide$Pre.Post <- factor(sum.wide$Pre.Post, levels = c("Pre","Post"))



#This is good, not the final one. It also won't work until Week 3's data is inputted into 
#the datasheet

ggplot(data=sum.wide, aes(x = Pre.Post, y = mean, group= interaction(Species, Temp), fill = interaction(Species,Temp)))+
  geom_col(position="dodge", width = 0.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1, position= position_dodge((0.5))) +
  scale_fill_manual(values = c("Carcinus.24 C"="turquoise4","Hemigrapsus.24 C"="brown4", "Hemigrapsus.14 C"="brown1","Carcinus.14 C"="lightblue2"))+
  labs(x=" ", y = "Percent Change", fill = "Genus")+
  theme_bw()+
  theme(text=element_text(size=16,family="serif"),)+
  scale_y_continuous(expand = c(0,0))





#Line charts
#an important thing we need to do is figure out which graph type tells the story better!
#the line graphs are pretty but i'm not all that certain they convey the message

PreDes <- read.csv("PreDesLong.csv")

sum.predes <- PreDes %>%
  group_by(Time, Genus, Temp) %>%
  summarise(
    N = n(),
    mean = mean(Norm.by.weight),
    sd = sd(Norm.by.weight),
    se = sd / sqrt(N),
  )

PostDes <- read.csv("PostDesLong.csv")

sum.postdes <- PostDes %>%
  group_by(Time, Temp, Genus) %>%
  summarise(
    N = n(),
    mean = mean(Norm.by.weight),
    sd = sd(Norm.by.weight),
    se = sd / sqrt(N),
  )




P1 <- ggplot(data=sum.predes, aes(x = Time, y = mean, group= interaction(Genus, Temp), color = interaction(Genus,Temp)))+
  geom_line()+
  geom_point(size=1.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1) +
  geom_smooth(method = "lm", se = FALSE, linewidth=0.5) +
  scale_color_manual(values = c("Carcinus.24 C"="turquoise4","Hemigrapsus.24 C"="brown4", "Hemigrapsus.14 C"="brown1","Carcinus.14 C"="lightblue2"))+
  stat_regline_equation(label.x= c(5),label.y = c(2500, 3000, 3500, 4000)) + 
  theme_minimal()+
  labs(x = "Time",
       y= "Fluorescence/gram body weight", color = "Genus")+
  theme(legend.position="none", text=element_text(size=12,family="serif"))



P2 <- ggplot(data=sum.postdes, aes(x = Time, y = mean, group= interaction(Genus, Temp), color = interaction(Genus,Temp)))+
  geom_line()+
  geom_point(size=1.5)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1) +
  geom_smooth(method = "lm", se = FALSE, linewidth=0.5) +
  scale_color_manual(values = c("Carcinus.24 C"="turquoise4","Hemigrapsus.24 C"="brown4", "Hemigrapsus.14 C"="brown1","Carcinus.14 C"="lightblue2"))+
  stat_regline_equation(label.x= c(5),label.y = c(2500, 3000, 3500, 4000)) + 
  theme_minimal()+
  labs(x = "Time",
       y= "  ", color = "Genus")+
  theme(text=element_text(size=12,family="serif"))
 
P1+P2



#Using slopes calculated on spreadsheet to make bar charts 
#for these, you need the patchwork library! the P# <- commands allow you to 
#stitch graphs together


SlopesHemi <- read.csv("Wide Hemi.csv")

sum.slopeshemi <- SlopesHemi %>%
  group_by(Temp, Pre.Post) %>%
  summarise(
    N = n(),
    mean = mean(Weight.Slope),
    sd = sd(Weight.Slope),
    se = sd / sqrt(N),
  )

sum.slopeshemi$Pre.Post <- factor(sum.slopeshemi$Pre.Post, levels = c("Pre","Post"))

SlopesCarci <- read.csv("Wide Carcinus.csv")

sum.slopesCarci <- SlopesCarci %>%
  group_by(Temp, Pre.Post) %>%
  summarise(
    N = n(),
    mean = mean(Weight.Slope),
    sd = sd(Weight.Slope),
    se = sd / sqrt(N),
  )

sum.slopesCarci$Pre.Post <- factor(sum.slopesCarci$Pre.Post, levels = c("Pre","Post"))


#Hemigrapsus plot 

P3 <- ggplot(data=sum.slopeshemi, aes(x = Temp, y = mean, group= Temp, fill = Temp))+
  geom_col(position="dodge", width = 0.5)+
  facet_wrap(~Pre.Post)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1, position= position_dodge((0.5))) +
  scale_fill_manual(values = paletteer_d("vapoRwave::sunSet"))+
  labs(title= "A",x=" ", y = "Metabolism (fold change fluorescence/mm)", fill = "Temperature")+
  theme_bw()+
  theme(axis.title.y = element_text(size = 12), legend.position="none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),text=element_text(size=16,family="serif"),)+
  scale_y_continuous(expand = c(0,0.5))

#Carcinus plot

P4 <- ggplot(data=sum.slopesCarci, aes(x = Temp, y = mean, group= Temp, fill = Temp))+
  geom_col(position="dodge", width = 0.5)+
  facet_wrap(~Pre.Post)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1, position= position_dodge((0.5))) +
  scale_fill_manual(values = paletteer_d("vapoRwave::seaPunk"))+
  labs(title= "B",x=" ", y = "  ", fill = "Temperature")+
  theme_bw()+
  theme(legend.position="none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),text=element_text(size=16,family="serif"),)+
  scale_y_continuous(expand = c(0,0.001))

P3+P4



#Percent change bar charts 

ChangeSlope <- read.csv("Hemigrapsus Change.csv")

sum.changeslope <- ChangeSlope %>%
  group_by(Temp, Species) %>%
  summarise(
    N = n(),
    mean = mean(Slope.Change),
    sd = sd(Slope.Change),
    se = sd / sqrt(N),
  )

ggplot(data=sum.changeslope, aes(x = Species, y = mean, group= Species, fill = Species))+
  geom_col(position="dodge", width = 0.5)+
  facet_wrap(~Temp)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width = 0.1, position= position_dodge((0.5))) +
  scale_fill_manual(values = c("Carcinus"="seagreen", "Hemigrapsus"= "violetred"))+
  labs(title= "Change in Respiration After Desiccation",x=" ", y = "Percent Change", fill = "Temperature")+
  theme_bw()+
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),text=element_text(size=16,family="serif"),)+
  scale_y_continuous(expand = c(0,1))



#Stats 
#Note: This may not work properly until i've added week 3's data!!
anovaWide <- aov(mean ~ Species * Temp, data = sum.wide)
summary(anovaWide)
TukeyHSD(anovaWide)






