###CSV cleaning

library (tidyverse)
library (readxl)

options(timeout = max(300, getOption("timeout")))

download.file('https://s3.amazonaws.com/itao-30230/inspections.csv',
              'inspections.csv')
inspections <- read_csv('https://s3.amazonaws.com/itao-30230/inspections.csv')

names <- c('ID', 'DBAName', 'AKAName', 'License', 'FacilityType', 'Risk',
           'Address', 'City', 'State', 'ZIP', 'InspectionDate',
           'InspectionType', 'Results', 'Violations', 'Latitude', 
           'Longitude', 'Location')

inspections <- read_csv('inspections.csv', skip=1)

glimpse(inspections)
summary(inspections)

unique(inspections$Results)

inspections %>%
  filter(Results=='Fail') %>%
  group_by(DBAName) %>%
  summarize(Failures)
  arrange()
  
download.file('https://s3.amazonaws.com/itao-30230/inpatient.tsv',
               'inpatient.tsv')

inpatient <- read_tsv('inpatient.tsv')
names <- c('DRG', 'ProviderID', 'Name', 'Address', 'City', 'State', 'ZIP',
           'Region', 'Discharges', 'AverageCharges', 'AverageTotalPayments',
           'AverageMedicarePayments')

types <- 'ccccccccinnn'

inpatient <- read_tsv('inpatient.tsv', col_names = names, skip=1)

inpatient %>%
  group_by(DRG) %>%
  summarize(Discharges=sum(Discharges)) %>%
  arrange(desc(Discharges))

download.file('https://s3.amazonaws.com/itao-30230/breakfast.xlsx',
              'breakfast.xlsx')

breakfast <- read_excel('breakfast.xlsx')

names <- c('Year', 'FreeStudents', 'ReducedStudents', 'PaidStudents',
           'TotalStudents', 'MealsSurved', 'PercentFree')
glimpse(breakfast)

breakfast <- breakfast %>%
  mutate(PercentFree=PercentFree/100)

breakfast <- breakfast %>%
  mutate(FreeStudents=FreeStudents*1000000,
         PaidStudents=PaidStudents*1000000,
         ReducedStudents=ReducedStudents*1000000,
         TotalStudents=TotalStudents*1000000,
         MealsServed=MealsServed*1000000)

library(tidyverse)

pew <- read_csv('https://s3.amazonaws.com/itao-30230/pew.csv')

colnames(pew) <- c('religion', 'under10', '10to20', '20to30', '30to40',
                   '40to50', '50to75', '75to100', '100to150', 'over150',
                   'refused')
pew %>%
  pivot_longer(cols=!religion, names_to='income', values_to = 'freq')


weather <- read_csv('http://s3.amazonaws.com/itao-30230/weather.csv')
summary(weather)

weather <- weather %>%
  mutate(station=as.factor(station),
         element=as.factor(element))
summary(weather)

weather <- weather %>%
  filter(element!='PRCP')

colnames(weather) <- str_replace_all(colnames(weather), "d", "")

weather %>%
  pivot_longer(cols=!c(station, year, month, element),
               names_to = 'day' ,
               values_to = 'value') %>%
  select(station, day, month, year, element, value)
  
weather <- weather %>%
  mutate(date=paste(0(month, '/', day, '/', year)) %>%
  mutate(date=mdy(date))
  
which(is.na(weather$date))
weather [31169,]

weather <- weather %>%
  filter(value!=-9999)
  summary(weather)

weather <- weather %>%
    select(-month, -year, -day)

weather <- weather %>%
    mutate(value=value/10)

weather <- weather %>%
    mutate(value=value*(9/5)+32)

weather <- weather%>%
    pivot_wider(names_from = element, values_from = value)

weather %>%
  filter
  ggplot(mapping=aes(x=date()) +
           geom_smooth(mapping=aes(y=TMAX), color = 'red') +
           geom_smooth(mapping=aes(y=tmin), color = 'blue') +
           geom_point(mapping=aes(y=tmax), color = 'red', alpha=0.01) +
           geom_point(mapping=aes(y=tmin), color = 'blue', alpha=0.01)
         