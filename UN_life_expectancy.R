options(repr.plot.width =6, repr.plot.height = 6)
library(dplyr)
library(tidyr)
library(ggplot2)
life_expectancy <- read.csv("UNdata.csv")
life_expectancy %>% head(10)

#subsetting
subdata <- life_expectancy %>% filter(Year == "2000-2005") %>%
  select(Country.or.Area, Subgroup, Value) %>% 
  spread(Subgroup, Value )
head(subdata, 10)
ggplot(subdata, aes(Male, Female))+geom_point()+
  geom_abline(intercept = 0, slope = 1, linetype =2)+
  scale_x_continuous(limits = c(35,80))+
  scale_y_continuous(limits = c(35,80))

#Let's improve the plot
better_plot <- ggplot(subdata, aes(Male, Female))+
  geom_point(color = "white", fill = "chartreuse3", 
             shape =21, alpha =0.55, size =5)+
  geom_abline(intercept = 0, slope = 1, linetype =2)+
  scale_x_continuous(limits = c(35,80))+
  scale_y_continuous(limits = c(35,80))+
  labs(title = "Life Expectancy at Birth by Country", 
       subtitle = "Years. Period: 200-2005. Average.", 
       caption = "Source: United Nations Statistics Division", 
       x= "Males", y = "Females")
better_plot  
#Highlighting remarkable countries
# Subseting data to obtain countries of interest
top_male <- subdata %>% arrange(Male-Female) %>% head(3)
top_female <- subdata %>% arrange(Female-Male) %>% head(3)

# Adding text to the previous plot to label countries of interest
ggplot(subdata, aes(x=Male, y=Female, label = Country.or.Area))+
  geom_point(color="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
  geom_abline(intercept = 0, slope = 1, linetype=2)+
  scale_x_continuous(limits=c(35,85))+
  scale_y_continuous(limits=c(35,85))+
  labs(title="Life Expectancy at Birth by Country",
       subtitle="Years. Period: 2000-2005. Average.",
       caption="Source: United Nations Statistics Division",
       x="Males",
       y="Females")+
  geom_text(data = top_male, size = 3)+
  geom_text(data = top_female, size = 3)+
  theme_bw()

#How has life expectancy by gender evolved?
subdata2 <- life_expectancy %>% 
  filter(Year %in% c("1985-1990", "2000-2005"))%>%
  mutate(Sub_Year = paste(Subgroup, Year, sep = "_"))%>%
  mutate(Sub_Year = gsub("-", "_", Sub_Year)) %>%
  select(-Subgroup, -Year) %>%
  spread(Sub_Year, Value) %>%
  mutate(diff_Female = Female_2000_2005 - Female_1985_1990, 
         diff_Male = Male_2000_2005 - Female_1985_1990)
head(subdata2)  

#visualize 
top <- subdata2 %>% arrange(diff_Male + diff_Female) %>% head(3)
bottom <- subdata2 %>% arrange(-(diff_Male +diff_Female)) %>% head(3)
ggplot(subdata2, aes(diff_Male, diff_Female, label = Country.or.Area))+
  geom_point(color = "white", fill = "chartreuse3", 
             shape =21, alpha =.55, size = 5)+
  geom_abline(intercept = 0, slope =1, linetype =2)+
  labs(title = "Life Expectancy at Birth by Country in Years", 
       subtitle = "Difference between 1985-1990 and 2000-2005. Average", 
       caption = "Source: United Nations Statistics Division", 
       x = "Males", y = "Females")+
  theme_bw()+
  scale_x_continuous(limits = c(-25, 25))+
  scale_y_continuous(limits = c(-25, 25))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_vline(xintercept = 0, linetype =2)+
  geom_text(data = top, size = 3)+
  geom_text(data = bottom, size = 3)
  
  
  
  
  
  
  
  
  


