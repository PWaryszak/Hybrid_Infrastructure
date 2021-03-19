library(tidyverse)
library(readxl)

cost_data <- read_excel("HybridData.xlsx", sheet = "DATA")
names(cost_data)
cost_data$Infrastructure <- factor(cost_data$Infrastructure , levels = c("Grey", "Green","Hybrid"))
#AV +SE of COST DATA========
cost_summary <- cost_data %>% 
  filter(Habitat != "Both")%>%
  group_by(Infrastructure, Habitat) %>%
  summarise(AV = mean(cost),
            SD = sd(cost),
            N= length(cost),
            SE = SD/sqrt(N))

cost_summary 

#Compute grey cost line values:
cost_grey <- cost_data %>% 
  filter(Habitat == "Both")%>%
  summarise(AV = mean(cost),
            SD = sd(cost),
            N= length(cost),
            SE = SD/sqrt(N)) %>%
  mutate(grey_min = AV-SE,
         grey_max = AV+SE)

cost_grey
#######AV    SD     N    SE grey_min grey_max
#    5237. 6584.    17 1597.    3641.    6834.

#New Boxplot as per Ashley's advice:=======
#change grey dataset to bind with cost_summary:

grey <- cost_grey %>%
  select(AV,SD,N,SE) %>%
  mutate (Infrastructure = factor("Grey"),
          Habitat = factor("All"))

a <- as.data.frame(grey)
b <- as.data.frame(cost_summary)

cost_summary2 <- rbind (a,b)
cost_summary2

plot_cost2 <-  ggplot(cost_summary2, aes(x= "", y=AV, fill = Infrastructure))+
  geom_bar(position="identity", stat="identity")+
  geom_errorbar( aes(ymin= AV+SE, ymax = AV-SE), width=.4)+
  facet_grid(.~Infrastructure+Habitat)+
  scale_fill_manual(values = c("grey","green","blue"))+
  labs( x ="", y  = bquote('Cost ' (USD*~m^-1)))+
  theme_bw()+
  theme(axis.text.x=element_text(vjust=0.5,size=12, color="black"),
        axis.text.y=element_text(size=12, color="black"),
        axis.title.y=element_text(size=18),
        legend.position = "none",
        axis.ticks.x=element_blank(),
        strip.text = element_text(size=18))



plot_cost2
ggsave(plot_cost2, dpi=600, width = 7, height = 5, filename = "HybridCostFigureGOOD.png")