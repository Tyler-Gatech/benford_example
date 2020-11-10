library(tidyverse)
library(ggplot2)

#County population data
data = read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv", header = T)

# filter out columns
data_filt = data %>% filter(COUNTY != 0) %>% select(c(STNAME, CTYNAME, CENSUS2010POP))

r_perc <- runif(nrow(data_filt), .5, .6)

data_filt %>% 
  mutate(D_perc = r_perc, 
         D_count = floor(D_perc * CENSUS2010POP), 
         R_count = CENSUS2010POP - D_count) %>% 
  mutate(first_pop = substr(as.character(CENSUS2010POP),1,1), 
         first_dcount = substr(as.character(D_count),1,1), 
         first_rcount  = substr(as.character(R_count),1,1)) %>% 
  sample_n(1500) %>% 
  count(first_dcount)  %>% 
  ggplot(aes(x=first_dcount, y = n)) + 
  geom_bar(stat = "Identity") 



