library(tidyverse)
library(lubridate)
library(countrycode)
library(scales)

# download flu deaths manually from from CDC: https://gis.cdc.gov/grasp/fluview/mortality.html
# read in file
dat0 <- read.csv(file = 'data/National_2015-20_Data.csv')

file_dg = file.path('data', 'time_series_covid19_deaths_global.csv')
# download US deaths from COVID 19 dataset
{
  # URL for Johns Hopkins timeseries of deaths from covid-19
  URL_dg <- 'https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
  
  # download the data
  dg0 <- read.csv(URL_dg)
  
  # pivot the data to long format
  dg1 <- dg0 %>% 
    pivot_longer(cols = -c('Province.State', 'Country.Region', 'Lat', 'Long'), 
                 names_to = 'date0',
                 names_prefix = 'X',
                 values_to = 'total_deaths') %>% 
    mutate(date = lubridate::mdy(date0),
           country = ifelse(as.character(Country.Region) == 'Kosovo',
                            'Serbia', 
                            as.character(Country.Region)),
           continent = countrycode::countrycode(sourcevar = country, 
                                                origin = 'country.name',
                                                destination = 'continent'))
  
  # some countries have more than just country summaries:
  countries_with_region_data <- c(
    dg1 %>% filter(Province.State != '') %>%
      pull(country) %>% unique(),
    'Serbia') # add in Serbia b/c I combined "Serbia" with "Kosovo"
  
  # create an empty list to hold the country summaries
  country_sums <- list()
  
  # summarize by country
  for(c in countries_with_region_data){
    # summarize 
    dat_summ <- dg1 %>% 
      filter(country == c) %>% 
      group_by(date) %>% 
      summarise(country = unique(country),
                continent = unique(continent),
                Lat = mean(unique(Lat)),
                Long = mean(unique(Long)),
                total_deaths = sum(total_deaths))
    
    country_sums[[c]] <- dat_summ
  }
  
  # data that WAS summarized by country already
  dg_summ <- dg1 %>% 
    filter(!country %in% countries_with_region_data)
  
  # group together all countries (now summarized by country)
  # and calculate the number of new cases each day. 
  dg <- rbind(dg_summ %>% 
                select(names(country_sums[[1]])),
              do.call(what = rbind, country_sums)) %>% 
    group_by(country) %>% 
    arrange(country, date) %>% 
    mutate(new_deaths = total_deaths - lag(total_deaths, order_by = date))
  
  # save the data to file
  write.csv(x = dg, file = file_dg)
}

# read in saved dataset & filter to US data
d0 <- read.csv(file_dg) %>% 
  filter(country == 'US') %>% 
  mutate(date = lubridate::ymd(date),
         week = lubridate::week(date),
         location = as.character(country))

# summarize by week
d1 <- d0 %>%  
  group_by(week) %>% 
  summarize(n = n(),
            deaths = (sum(new_deaths) / n) * 7)


# organize flu dataset & make "week" line up with flu season instead of calendar year
dat <- dat0 %>% 
  mutate(flu_deaths = as.numeric(sub(pattern = ',', replacement = '', x = NUM.INFLUENZA.DEATHS)),
         pneu_deaths = as.numeric(sub(pattern = ',', replacement = '', x = NUM.PNEUMONIA.DEATHS)),
         deaths = flu_deaths + pneu_deaths,
         week = (WEEK+12) %% 52 + 1,
         Year = paste('Flu', SEASON))


# plot flu dataset
(p1 <- ggplot(dat, aes(x = week, 
                       y = deaths, 
                       color = Year)) + 
    geom_point()+
    geom_line()+
    theme_bw()+
    ylab('Deaths per week')+
    xlab("Week")+
    scale_y_continuous(labels = scales::comma))

# add on covid-19 data
p1 + 
  geom_line(data = d1, 
            aes(x = week, 
                y = deaths, 
                color = 'covid-19'),
            size = 1)+
  geom_point(data = d1, 
             aes(x = week, 
                 y = deaths, 
                 color = 'covid-19'))