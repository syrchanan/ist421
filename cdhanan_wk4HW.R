library(tidyverse)
#install.packages("ggrepel")
library(ggrepel)

crime <- read_csv('http://datasets.flowingdata.com/crimeRatesByState2005.csv')

head(crime)

options(scipen=999)

crime %>% filter(state!="District of Columbia" & state!="United States") %>% 
ggplot(aes(murder, burglary,size=population,label=state)) + 
  geom_point(fill="red",shape=21,color='white',stroke=1,alpha=.75) +
  scale_size_area(max_size=30) +
  geom_text_repel(size=3) +
  ggtitle("Murders Versus Burglaries in the United States") +
  xlab("Murders (per 100,000 population)") +
  ylab("Burglaries (per 100,000 population)")+
  xlim(c(1,10)) +
  ylim(c(200,1500)) +
  theme(panel.background = element_blank())
  
ggsave("cdhanan_wk4HW_plot1.pdf")

birth <- read_csv("http://datasets.flowingdata.com/birth-rate.csv")

select(birth,`2008`) %>% 
  filter(!is.na(`2008`)) %>% 
  ggplot(aes(x=`2008`)) +
  geom_histogram(binwidth = 4,fill = 'blueviolet', color='white') +
  ggtitle(label='Global Distribution of Birth Rates',
          subtitle = "In 2008, most countrieshad birth rates less than 25 live births per 1,000 population. \nThere are, however, many developing countries where women tend to bear more children.") +
  labs(caption = "Source: The World Bank") +
  xlab('Live births per 1,000 population') +
  ylab('Country Count') +
  xlim(c(0,60))

ggsave("cdhanan_wk4HW_plot2.pdf")

select(birth,`2008`) %>% 
  filter(!is.na(`2008`)) %>% 
  ggplot(aes(x=`2008`)) +
  geom_density(fill='firebrick4', color = 'white') +
  ggtitle("GLOBAL DISTRIBUTION OF BIRTH RATES IN 2008") +
  xlab('Live births per 1,000 population') +
  ylab('Density') +
  labs(caption = "Source: The World Bank") +
  scale_x_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60))

ggsave("cdhanan_wk4HW_plot3.pdf")

art <- read_csv("art.csv")
library(lubridate)

art %>% mutate(format.date = as.Date(art$date, format = "%m/%e/%Y")) -> art
art %>% mutate(month.sep = month(art$format.date, label=TRUE)) %>% 
  group_by(month.sep,store) %>% summarize(mean=mean(total.sale)) %>% 
ggplot(aes(x=store, y=mean, fill= factor(store))) +
  geom_bar(stat='identity') +
  facet_wrap(~month.sep) +
  ylim(c(0,25)) +
  ggtitle("Average Sale by Store Each Month")+
  theme_minimal()+
  theme(legend.position = "none")

ggsave("cdhanan_wk4HW_plot4.pdf")

art %>% group_by(rep, paper.type,paper) %>% 
  summarize(mean=mean(total.sale)) %>% 
  ggplot(aes(x=paper, y=mean)) +
  geom_bar(stat='identity', position = 'dodge',aes(fill = paper.type)) +
  scale_fill_discrete(type = c('forestgreen','darkorchid4','darkgoldenrod1','brown4','dodgerblue4','deeppink3')) +
  facet_wrap(~rep) +
  ggtitle("Average Units Sold by Paper and Rep")+
  theme_minimal()

ggsave("cdhanan_wk4HW_plot5.pdf")



