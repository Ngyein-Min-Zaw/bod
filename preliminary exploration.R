library(readxl)
library(tidyverse)
Book1 <- read_excel("Blood Sugar Reduction in Mouse with Selected Plant Extracts/Book1.xlsx", 
                    sheet = "Sheet2")

Book1 %>% dim()
trts <- Book1 %>% distinct(Treatments) %>% pull(Treatments)

View(Book1)
initial <- Book1 %>% filter(time==45) %>% pull(BGL)
df<-Book1 %>% filter(time!=0) %>% mutate(Change2=case_when(
  Treatments==trts[1] ~ (BGL-initial[1:5])/initial[1:5]*100,
  Treatments==trts[2] ~ (BGL-initial[6:10])/initial[6:10]*100,
  Treatments==trts[3] ~ (BGL-initial[11:15])/initial[11:15]*100,
  Treatments==trts[4] ~ (BGL-initial[16:20])/initial[16:20]*100,
  Treatments==trts[5] ~ (BGL-initial[21:25])/initial[21:25]*100,
  Treatments==trts[6] ~ (BGL-initial[26:30])/initial[26:30]*100
)) 
df%>%
  
  ggplot(aes(Treatments, Change2, fill = Treatments))+
  #geom_point()+
  #3geom_smooth(se=F, span =1)#+
  geom_bar(stat='summary', fun=mean, position='dodge')+
  facet_wrap(.~time)
  scale_x_continuous(limits = c(45,225),
                     breaks = seq(45,225, 45))

mod<-aov(BGL~time+Treatments, data=Book1)
summary(mod)

with(Book1, interaction.plot(time, Treatments, BGL))
with(df, interaction.plot(time, Treatments, Change2))

library(agricolae)
h<-HSD.test(mod, trt = c("Treatments", 'time'), alpha = 0.05)
h
