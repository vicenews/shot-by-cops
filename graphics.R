#libraries
require('dplyr')
require('tidyr')
require('ggplot2')
require('lubridate')
require('forcats')
require('waffle')
require('zoo')

#set directory, load data
full_db <- read.csv("incident_data.csv")
subject_db <- read.csv("subject_data.csv")

#parse dates
full_db$fdate <- parse_date_time(full_db$fdate, "%m/%d/%y")
subject_db$fdate <- parse_date_time(subject_db$Date, "%m/%d/%Y")

#### GRAPHIC: COUNTS OF ALL SUBJECTS #####

#check for blanks in Fatal column, convert to "U
subject_db <- subject_db %>% mutate_if(is.factor, funs(factor(replace(., .=="", NA))))

#check for NAs in Fatal column, convert to "U"
subject_NAs <- filter(subject_db, is.na(Fatal))
subject_db$Fatal[is.na(subject_db$Fatal)] <- "U"

#remove any whitespace
subject_db <- subject_db %>% mutate(Fatal = gsub(' ', '', Fatal))

#tally subjects by Fatal, Non-fatal or Unknown
subject_counts <- subject_db %>% group_by(Fatal) %>% summarize(total=n())

Fatal <- filter(subject_counts, Fatal=="F")
Nonfatal <- filter(subject_counts, Fatal=="N")
Unknown <- filter(subject_counts, Fatal=="U")

#count how many squares in total
sum(subject_counts$total)
subjects <- c('Fatal'=Fatal$total, 'Non-fatal'=Nonfatal$total, "Unknown"=Unknown$total)

#create waffle chart for desktop
waffle(subjects, rows=45, size=0.5, 
       colors=c("#0F1187", "#C2C9F2","#cdcdcd"), 
       xlab="1 square = 1 person")


#### GRAPHIC: FATAL, NONFATAL, UNKNOWN SUBJECTS OVER TIME #####

#count fatal vs. non-fatal by year
subject_counts_byyear <-
  subject_db %>% group_by(year = year(fdate), Fatal) %>%
  summarize(total = n())

subject_counts_byyear_all <-
  subject_db %>% group_by(year = year(fdate)) %>%
  summarize(total = n())

#get yearly totals
y2010 <- filter(subject_counts_byyear_all, year==2010)
y2011 <- filter(subject_counts_byyear_all, year==2011)
y2012 <- filter(subject_counts_byyear_all, year==2012)
y2013 <- filter(subject_counts_byyear_all, year==2013)
y2014 <- filter(subject_counts_byyear_all, year==2014)
y2015 <- filter(subject_counts_byyear_all, year==2015)
y2016 <- filter(subject_counts_byyear_all, year==2016) 

#barchart of subjects over time
subjects_overtime <- ggplot(subject_counts_byyear, aes(x=year, y=total)) +
  geom_bar(aes(fill=Fatal), stat="identity")
print(subjects_overtime)

#subjects over time chart as squares
values <- c(y2010$total,y2011$total,y2012$total,y2013$total, y2014$total,y2015$total,y2016$total)
barplot(values)
max_val <- max(values)
bar_width <- 10
padding <- 1
xlim <- c( 0, (bar_width+padding) * length(values) )
ylim <- c( 0, ceiling(max_val / bar_width) )

plot(0, 0, type="n", xlab="", ylab="", bty="n", xlim=xlim, ylim=ylim, asp=1, axes=FALSE)

for (i in 1:length(values)) {
  xleft <- (i-1)*(bar_width+padding)
  num_rows <- ceiling(values[i] / bar_width)
  x <- rep(xleft:(xleft+bar_width-1), num_rows)[1:values[i]]
  y <- rep(1:num_rows, each=bar_width)[1:values[i]]
  symbols(x, y, squares=rep(1, length(x)), inches=FALSE, add=TRUE, bg="black", fg="white")
}

#each year as individual waffle chart

#2010
subjects2010 <- filter(subject_counts_byyear, year==2010)
subjects2010F <- filter(subjects2010, Fatal=="F")
subjects2010N <- filter(subjects2010, Fatal=="N")
subjects2010U <- filter(subjects2010, Fatal=="U")
subjects2010_values <- c(subjects2010F$total,subjects2010N$total, subjects2010U$total)
waffle2010 <- waffle(subjects2010_values, rows=10, size=0.5, colors=c("#022d4b", "#2fbde2","#cdcdcd"))
print(waffle2010)

#2011
subjects2011 <- filter(subject_counts_byyear, year==2011)
subjects2011F <- filter(subjects2011, Fatal=="F")
subjects2011N <- filter(subjects2011, Fatal=="N")
subjects2011U <- filter(subjects2011, Fatal=="U")
subjects2011_values <- c(subjects2011F$total,subjects2011N$total, subjects2011U$total)
waffle2011 <- waffle(subjects2011_values, rows=10, size=0.5, colors=c("#022d4b", "#2fbde2","#cdcdcd"))
print(waffle2011)

#2012
subjects2012 <- filter(subject_counts_byyear, year==2012)
subjects2012F <- filter(subjects2012, Fatal=="F")
subjects2012N <- filter(subjects2012, Fatal=="N")
subjects2012U <- filter(subjects2012, Fatal=="U")
subjects2012_values <- c(subjects2012F$total,subjects2012N$total, subjects2012U$total)
waffle2012 <- waffle(subjects2012_values, rows=10, size=0.5, colors=c("#022d4b", "#2fbde2","#cdcdcd"))
print(waffle2012)

#2013
subjects2013 <- filter(subject_counts_byyear, year==2013)
subjects2013F <- filter(subjects2013, Fatal=="F")
subjects2013N <- filter(subjects2013, Fatal=="N")
subjects2013U <- filter(subjects2013, Fatal=="U")
subjects2013_values <- c(subjects2013F$total,subjects2013N$total, subjects2013U$total)
waffle2013 <- waffle(subjects2013_values, rows=10, size=0.5, colors=c("#022d4b", "#2fbde2","#cdcdcd"))
print(waffle2013)

#2014
subjects2014 <- filter(subject_counts_byyear, year==2014)
subjects2014F <- filter(subjects2014, Fatal=="F")
subjects2014N <- filter(subjects2014, Fatal=="N")
subjects2014U <- filter(subjects2014, Fatal=="U")
subjects2014_values <- c(subjects2014F$total,subjects2014N$total, subjects2014U$total)
waffle2014 <- waffle(subjects2014_values, rows=10, size=0.5, colors=c("#022d4b", "#2fbde2","#cdcdcd"))
print(waffle2014)

#2015
subjects2015 <- filter(subject_counts_byyear, year==2015)
subjects2015F <- filter(subjects2015, Fatal=="F")
subjects2015N <- filter(subjects2015, Fatal=="N")
subjects2015U <- filter(subjects2015, Fatal=="U")
subjects2015_values <- c(subjects2015F$total,subjects2015N$total, subjects2015U$total)
waffle2015 <- waffle(subjects2015_values, rows=10, size=0.5, colors=c("#022d4b", "#2fbde2","#cdcdcd"))
print(waffle2015)

#2016
subjects2016 <- filter(subject_counts_byyear, year==2016)
subjects2016F <- filter(subjects2016, Fatal=="F")
subjects2016N <- filter(subjects2016, Fatal=="N")
subjects2016U <- filter(subjects2016, Fatal=="U")
subjects2016_values <- c(subjects2016F$total,subjects2016N$total, subjects2016U$total)
waffle2016 <- waffle(subjects2016_values, rows=10, size=0.5, colors=c("#022d4b", "#2fbde2","#cdcdcd"))
print(waffle2016)


####GRAPHIC: SUBJECTS BY RACE###

#load in population data
city_pops <- read.csv("census_pop_2013.csv")

#exclude population of cities without race info
city_pops_race <- filter(city_pops, !city_name%in%c("Philadelphia","Detroit","Atlanta","Honolulu","Cleveland","Newark","San Francisco", "Boston", "MiamiDade", "LasVegas", "Kansas City", "FairfaxCounty", "St. Louis", "City of Miami", "Albuquerque", "Tampa"))

#get rid of cities with insufficient race info
subject_db_clean <- filter(subject_db, !city%in%c("Philadelphia","Detroit","Atlanta","Honolulu","Cleveland","Newark","San Francisco", "Boston", "MiamiDade", "LasVegas", "Kansas City", "FairfaxCounty", "St. Louis", "City of Miami", "Albuquerque", "Tampa"))

#get rid of NA SubjectRace
subject_db_race <- filter(subject_db_clean, tr==TRUE)

#count subjects by race
subject_counts_race <- subject_db_race %>% group_by(SubjectRace) %>% summarize(total_race=n())

city_pops_race_totals <- data.frame(
  'A'=sum(city_pops_race$asian), 
  'B'=sum(city_pops_race$black), 
  'L'=sum(city_pops_race$hispanic),
  'W'=sum(city_pops_race$white))

city_pops_race_wide <- gather(city_pops_race_totals, race, total_pop)

#join subject race with city pop by race
subject_race_pop <- left_join(city_pops_race_wide, subject_counts_race, by = c("race" = "SubjectRace"))

#calculate rate per race per 100K
subject_race_pop_rate <- subject_race_pop %>% mutate(rate_100k = ((total_race/total_pop)/7)*100000)

#set levels
subject_race_pop_rate$race <- factor(subject_race_pop_rate$race, levels = subject_race_pop_rate$race[order(subject_race_pop_rate$rate_100k)])

#OIS rates by rate per year
ratesByRaceChart <- ggplot(subject_race_pop_rate, aes(x=race, y=rate_100k)) +
  geom_bar(stat = 'identity') +
  ylab('Rate of shootings per year by race, per 100K')
print(ratesByRaceChart)

### CHART: ARMED BY RACE###

#exclude cities for which we have insufficent armed data
armedRace_clean <- filter(subject_db, !city%in%c("Chicago","Tampa","City of Miami","LosAngeles","Indianapolis","Detroit","Newark","BaltimoreCity","DekalbCounty","SanJose","Columbus","Cleveland"))

#filter cities with insufficient race data
armedRace_clean2 <- filter(armedRace_clean, !city%in%c("Philadelphia","Detroit","Atlanta","Honolulu","Cleveland","Newark","San Francisco", "Boston", "MiamiDade", "LasVegas", "Kansas City", "FairfaxCounty", "St. Louis", "City of Miami", "Albuquerque", "Tampa"))

#how many cities left
armedRace_cities <- armedRace_clean2 %>% group_by(city) %>% summarize(total=n())

#check for SubjectRace NAs, turn into U
armedRace_NAs <- filter(armedRace_clean2, is.na(SubjectRace))
armedRace_clean2$SubjectRace[is.na(armedRace_clean2$SubjectRace)] <- "U"

#armed and race totals
armedRace_counts <- armedRace_clean2 %>%  group_by(SubjectRace, weapon) %>% summarize(total=n())

#get percentage of subjects Armed
blackArmed_pct <- armedRace_counts %>% filter(SubjectRace=='B') %>% mutate(pct = total/(sum(total)))
whiteArmed_pct <- armedRace_counts %>% filter(SubjectRace=='W') %>% mutate(pct = total/(sum(total)))
asianArmed_pct <- armedRace_counts %>% filter(SubjectRace=='A') %>% mutate(pct = total/(sum(total)))
latinoArmed_pct <- armedRace_counts %>% filter(SubjectRace=='L') %>% mutate(pct = total/(sum(total)))
otherArmed_pct <- armedRace_counts %>% filter(SubjectRace=='O') %>% mutate(pct = total/(sum(total)))
unknownArmed_pct <- armedRace_counts %>% filter(SubjectRace=='U') %>% mutate(pct = total/(sum(total)))

#bind all race pct together
all_race_pcts <- rbind(blackArmed_pct, whiteArmed_pct,asianArmed_pct,latinoArmed_pct,otherArmed_pct,unknownArmed_pct)

#set levels
all_race_pcts$weapon <- factor(all_race_pcts$weapon, levels=c("unarmed","U", "replica","other", "knife", "gun"))

raceArmedChart <- ggplot(all_race_pcts, aes(x = "", y = pct, fill=weapon)) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values=c("#fdcc8a","#cdcdcd","#bdd7e7","#6baed6","#3182bd","#08519c")) +
  facet_wrap(~SubjectRace,ncol=1) +
  coord_flip()
print(raceArmedChart)

#Filter out asian, other, unknown for which there is insufficient data & bind all race pct together
all_race_pcts2 <- all_race_pcts %>% filter(!(SubjectRace=='U'| SubjectRace=='A' | SubjectRace=='O'))

#set levels
all_race_pcts$weapon <- factor(all_race_pcts$weapon, levels=c("unarmed","U", "replica","other", "knife", "gun"))

raceArmedChart2 <- ggplot(all_race_pcts2, aes(x = "", y = pct, fill=weapon)) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values=c("#fdcc8a","#cdcdcd","#bdd7e7","#6baed6","#3182bd","#08519c")) +
  facet_wrap(~SubjectRace,ncol=1) +
  coord_flip()
print(raceArmedChart2)

###INCIDENT-LEVEL ARMED VS. UNARMED CHARTS###

#exclude cities for which we have insufficent armed data
incident_db_clean <- filter(full_db, !city%in%c("Chicago","Tampa","City of Miami","LosAngeles","Indianapolis","Detroit","Newark","BaltimoreCity","DekalbCounty","SanJose","Columbus","Cleveland"))

#check for SubjectArmed NAs, turn into U
incident_armed_NAs <- filter(incident_db_clean, is.na(SubjectArmed))
incident_db_clean$SubjectArmed[is.na(incident_db_clean$SubjectArmed)] <- "U"

#remove any whitespace in VictmArmed 
incident_db_clean <- incident_db_clean %>% mutate(SubjectArmed = gsub(' ', '', SubjectArmed))

#get total incident counts with weapon
incident_counts <- incident_db_clean %>% group_by(weapon) %>% summarize(total=n())

#get percentage of incidents subject is armed
incident_counts_pct <- incident_counts %>% mutate(pct = total/(sum(total)))

#set levels
incident_counts_pct$weapon <- factor(incident_counts_pct$weapon, levels=c("unarmed","U", "replica","other", "knife", "gun"))

incidentsArmedChart <- ggplot(incident_counts_pct, aes(x = "", y = pct, fill=weapon)) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values=c("#fdcc8a","#cdcdcd","#bdd7e7","#6baed6","#3182bd","#08519c")) +
  coord_flip()
print(incidentsArmedChart)


### CHART: RATE OF SHOOTINGS OVER TIME BY DEPARTMENT###

#check for blanks in Fatal column, convert to "U
subject_db <- subject_db %>% mutate_if(is.factor, funs(factor(replace(., .=="", NA))))

#filter cities for which we have incomplete data for all years
subject_db_fullyears <- filter(subject_db, !city%in%c("Detroit"))

#City raw counts over time
city_counts_byyear <- subject_db_fullyears %>% group_by(city, year) %>% summarize(total=n())

#remove San Diego 2016 incomplete data
city_counts_byyear <- city_counts_byyear %>% filter(!(city=="SanDiego"&year=="2016"))

#join with population data
city_count_pop <- left_join(city_pops, city_counts_byyear, by = c("city_name" = "city"))
city_count_pop <- city_count_pop %>% select(department, city_pop_2013, year, total) %>% filter(department !='Detroit (MI) Police')

#calculate rates per 100k in each City
city_rates <- city_count_pop %>% mutate(rate_100k=(total/city_pop_2013)*100000)
city_rates <- city_rates %>% select(department, year:rate_100k)

#make data wide format
city_wide <- spread(city_rates, year, rate_100k) %>% select(-total) %>% 
  group_by(department) %>% 
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  select(department, `2010`:`2016`)

#calculate 2010-13 rate vs. 2014-2016 rate
city_rate_avgs <- city_wide %>% mutate(rateavg1013=ifelse(department=='Washington (DC) Metropolitan Police' | department == 'Louisville (KY) Metro Police ', (`2011`+`2012`+`2013`)/3, (`2010`+`2011`+`2012`+`2013`)/4)) %>%
  mutate(rateavg1416=ifelse(department=='San Diego (CA) Police ' | department =='Atlanta (GA) Police', (`2014`+`2015`)/2, (`2014`+`2015`+`2016`)/3)) %>% 
  mutate(ratechg=rateavg1416-rateavg1013)

#select top 10 cities only and reorder by largest decrease in OIS
city_top10decreases <- city_rate_avgs %>% top_n(-10, ratechg)
city_top10decreases$department <- factor(city_top10decreases$department, levels = city_top10decreases$department[order(city_top10decreases$ratechg)])

#select just rate columns
city_rates_only <- city_top10decreases %>% select(department,`2010`:`2016`)

#make long, clean up years
city_rates_long <- gather(city_rates_only, year, rate, -department)

#chart of only top 10 decrease cities
chart_cityrates <- ggplot() +
  geom_line(data=city_rates_long, aes(x=year, y=rate, group=department, color=factor(department))) +
  facet_wrap(~department, ncol=5) +
  ylab("rate per 100k") +
  theme(legend.position="none")
print(chart_cityrates)

#### ALL CITIES ####

#select just rate columns
city_rates_all <- city_rate_avgs %>% select(department,`2010`:`2016`)

#make long, clean up years
city_rates_long_all <- gather(city_rates_all, year, rate, -department)

#chart of all cities
chart_cityrates_all <- ggplot() +
  geom_line(data=city_rates_long_all, aes(x=year, y=rate, group=department, color="#ff011f")) +
  facet_wrap(~department, ncol=3) +
  scale_y_continuous(breaks=seq(0,10,2)) +
  geom_hline(yintercept = 0) +
  theme(axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none", 
        strip.background = element_blank())
print(chart_cityrates_all)

#Chart over time for just Detroit PD

#filter cities for which we have incomplete data for all years
detroit_ois <- filter(subject_db, city=='Detroit')

#join with pop data
detroit_ois <- left_join(city_pops, detroit_ois, by = c("city_name" = "city"))
detroit_yearly <- detroit_ois %>% group_by(year, city_pop_2013) %>% summarize(total=n()) %>% mutate(rate_100k=(total/city_pop_2013)*100000)

#chart detroit
chart_detroit <- ggplot(detroit_yearly, aes(x=year, y=rate_100k, group=1, color="#ff011f")) +
  geom_line() +
  scale_y_continuous(breaks=seq(0,10,2)) +
  geom_hline(yintercept = 0) +
  theme(axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none", 
        strip.background = element_blank())
print(chart_detroit)

