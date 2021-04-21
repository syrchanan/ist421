##### INIT #####

setwd("~/School/Syracuse/Jr - Sem 2/IST 421")
us <- ggplot2::map_data("state")
library(tidyverse)
library(lubridate)

storms<-read_csv("StormEvents.gz",col_types = cols(EPISODE_NARRATIVE="c",EVENT_NARRATIVE="c",WFO='c',EPISODE_ID='c',TOR_OTHER_CZ_STATE='c',SOURCE='c',BEGIN_LOCATION='c',END_LOCATION='c',TOR_OTHER_CZ_FIPS='c',TOR_OTHER_CZ_NAME='c',BEGIN_AZIMUTH='c',END_AZIMUTH='c',DAMAGE_PROPERTY="c",DAMAGE_CROPS='c',MAGNITUDE_TYPE='c',FLOOD_CAUSE='c',TOR_OTHER_WFO='c',CATEGORY='i'))

storms <- storms %>% 
  # Do this all in one mutate command
  mutate(
    # Replace all K,M,B with empty string and assign result to new column dam_num
    dam_num=as.numeric(gsub("[KMB]","",DAMAGE_PROPERTY)),
    # Replace all digits / decimal point with empty string and assign result to new column dam_let
    dam_let=gsub("[0-9.]+","",DAMAGE_PROPERTY),
    # Use case_when to get the multiplier based on the letter and assign to colun dam_mult
    dam_mult=case_when(
      dam_let=="K" ~ 1000,
      dam_let=="M" ~ 10^6,
      dam_let=="B" ~ 10^9,
      # This last bit tells us to return a '1' if we haven't matched anything above
      TRUE ~ 1),
    # Calculate actual damage number and assign the result to prop damage
    prop_damage = dam_num*dam_mult)

storms %>% 
  filter(EVENT_TYPE == 'Flood') -> flood

#install.packages('maps')
library(maps)
library(mapproj)
#install.packages('ggmap')
library(ggmap)

##### Regions #####
NE.name <- tolower(c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania"))
NE <- data.frame(STATE=NE.name,region=rep("ne",9))
MW.name <- tolower(c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota"))
MW <- data.frame(STATE=MW.name,region=rep("mw",12))
S.name <- tolower(c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas"))
S <- data.frame(STATE=S.name,region=rep("s",17))
W.name <- tolower(c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington"))
W <- data.frame(STATE=W.name,region=rep("w",13))
region_us <- rbind(NE,MW,S,W)

##### State Mapping #####
us$state <- tolower(us$region)
flood$STATE <- tolower(flood$STATE)

flood %>% 
  group_by(STATE) %>% 
  summarise(count=n()) %>% 
  filter(!STATE %in% c('american samoa','district of columbia',
                    'guam','puerto rico','virgin islands','alaska','hawaii')) -> flood.state

us %>% 
  inner_join(.,flood.state, by = c('state' = 'STATE')) -> us.flood

ggplot(us.flood)+
  aes(long,lat,group=group)+
  geom_polygon(aes(fill = count))+
  coord_map(projection = 'albers', parameters = c(25,50))+
  scale_fill_viridis_c()
  



##### Heat MAP #####

flood %>% 
  filter(!is.na(DAMAGE_PROPERTY)) -> flood.damage

flood.damage %>%
  mutate(STATE = tolower(STATE)) %>% 
  select(STATE,prop_damage,YEAR) %>% 
  filter(!STATE %in% c('american samoa','district of columbia',
                       'guam','puerto rico','virgin islands')) -> state.flood.damage

state.flood.damage %>% 
  group_by(STATE,YEAR) %>% 
  summarize(total_damage = sum(prop_damage)) %>% 
  ungroup() %>% 
  filter(YEAR != 1998) -> flood.sum.year
  
ggplot(flood.sum.year)+
  geom_tile(aes(YEAR,STATE,fill=total_damage))+
  #scale_fill_viridis_c()+
  scale_fill_gradient(trans = 'log')+
  theme_minimal()
  
#install.packages("treemap")
library(treemap)
#install.packages('viridis')
library(viridis)

flood.sum.year %>% 
  group_by(STATE) %>% 
  summarize(total_damage = sum(total_damage)) %>% 
  left_join(.,region_us) %>% 
  mutate(STATE = str_to_title(STATE)) %>% 
treemap(index = "STATE",
        vSize = "total_damage",
        vColor = "region",
        type = 'categorical',
        align.labels = c('right','bottom'),
        overlap.labels = 0.01,
        bg.labels=c("transparent"),
        palette = viridis(5))

##### Count Flood By Month #####

order <- c('January','February','March','April','May','June','July','August','September','October','November','December')
ndata<-flood %>%
  group_by(MONTH_NAME) %>%
  summarise(count=n()) %>%
  mutate(MONTH_NAME = factor(MONTH_NAME,levels=order))
ggplot(ndata) + geom_col(aes(MONTH_NAME,count)) + theme(axis.text.x = element_text(angle=45,hjust=1))

##### Flood Length #####

flood_length <- flood %>% mutate(total_days = END_DAY - BEGIN_DAY) %>% arrange(-total_days)
flood_length_hist <- ggplot(flood_length) + geom_histogram(aes(x=total_days), binwidth=3, fill="#296D98") + ggtitle("Number of Days per Flood")
flood_length_hist
