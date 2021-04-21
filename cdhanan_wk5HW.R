library(tidyverse)
library(lubridate)

sales <- read_csv("sales.csv")

View(sales)

sales %>% 
  group_by(sales.rep) %>% 
  summarise(receipt = sum(recipt)) %>%
  arrange(receipt = desc(receipt)) -> ordered.factors

sales %>% 
  mutate(sales.date = floor_date(mdy(sales$sale.date), unit='month')) %>% 
  group_by(sales.date,sales.rep) %>% 
  summarize(receipt = sum(recipt)) %>% 
  mutate(sales.rep = factor(sales.rep, ordered = TRUE,levels = ordered.factors$sales.rep)) %>% 
  ggplot() +
  geom_tile(aes(sales.rep,sales.date,fill=receipt)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,hjust=1.1))+
  labs(fill = "Receipts")+
  ggtitle("Receipts per Sales Rep over time")+
  ylab('Month')+
  xlab("Sales Rep")

ggsave("cdhanan_wk5HW_plot1.pdf")
