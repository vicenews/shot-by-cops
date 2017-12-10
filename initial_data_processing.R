##Processing and Standardizing.
##libraries
library(lubridate)
library(ggplot2)
library(stringr)
library(lme4)

##data input
v <- list()
for(i in 1:length(
grep("csv", list.files(), value=T)
)
)
{
v[[i]] <- read.csv(grep("csv", list.files(), value=T)[i],
stringsAsFactors=F)[,
c("Date","NumberOfSubjects","Fatal","SubjectArmed",
"Notes","SubjectRace","SubjectGender","SubjectAge",
"NatureOfStop", "NumberOfShots","NumberOfOfficers",
"OfficerRace","OfficerGender","Department",
"FullNarrative")]
v[[i]]$Date <- as.character(v[[i]]$Date)
v[[i]]$Notes <- as.character(v[[i]]$Notes)
v[[i]]$city <- substr(unlist(lapply(strsplit(grep("csv", 
list.files(), value=T), "_"), "[[", 1))[i], 9, 100)
}

grep("csv", list.files(), value=T)[i]

dat <- do.call(rbind, v)
dat$Date[grepl("T", dat$Date)] <- 
unlist(lapply(strsplit(as.character(
dat$Date[grepl("T", dat$Date)]),
split="T"), "[[", 1))

##need to fix some mis-entered dates here
dat$year <- year(mdy(dat$Date))
dat$year[!is.na(dmy(dat$Date))&is.na(mdy(dat$Date))] <- 
year(dmy(dat$Date))[!is.na(dmy(dat$Date))&is.na(mdy(dat$Date))]
dat$year[!is.na(ymd(dat$Date))&is.na(mdy(dat$Date))] <- 
year(ymd(dat$Date))[!is.na(ymd(dat$Date))&is.na(mdy(dat$Date))]
#dat$year[dat$city=="Albuquerque"] <- year(dmy(dat$Date)[dat$city=="Albuquerque"])
#dat$year[dat$city=="Tucson"] <- year(ymd(subset(dat, city=="Tucson")$Date))
dat$year[dat$city=="Tampa"] <- subset(dat, city=="Tampa")$Date
dat$year[dat$city=="Newark"] <- subset(dat, city=="Newark")$Date
dat$year[dat$city=="Detroit"] <- subset(dat, city=="Detroit")$Date
dat$year[dat$city=="Memphis"&is.na(dat$year)] <- 
subset(dat, city=="Memphis"&is.na(year))$Date
dat$year[dat$city=="New York"&is.na(dat$year)] <- 
subset(dat, city=="New York"&is.na(year))$Date
dat$year[dat$city=="LasVegas"&is.na(dat$year)] <- 
subset(dat, city=="LasVegas"&is.na(year))$Date
dat$year[dat$city=="MiamiDade"&is.na(dat$year)] <- 
subset(dat, city=="MiamiDade"&is.na(year))$Date
dat$year[dat$city=="NewOrleans"&is.na(dat$year)] <- 
subset(dat, city=="NewOrleans"&is.na(year))$Date
dat$year[dat$city=="Cleveland"&is.na(dat$year)] <- 
subset(dat, city=="Cleveland"&is.na(year))$Date
dat$year[dat$city=="Portland"] <- 
year(mdy(unlist(lapply(strsplit(subset(dat, city=="Portland")$Date,
split=" "), "[[", 1))))
dat$year[dat$city=="CharlotteMecklenburg"] <- 
(unlist(lapply(strsplit(subset(dat, city=="CharlotteMecklenburg")$Date,
split="-"), "[[", 1)))


dat$month <- month(mdy(dat$Date))
dat$month[!is.na(dmy(dat$Date))&is.na(mdy(dat$Date))] <- 
month(dmy(dat$Date))[!is.na(dmy(dat$Date))&is.na(mdy(dat$Date))]
dat$month[!is.na(ymd(dat$Date))&is.na(mdy(dat$Date))] <- 
month(ymd(dat$Date))[!is.na(ymd(dat$Date))&is.na(mdy(dat$Date))]
#dat$month[dat$city=="Albuquerque"] <- month(dmy(dat$Date)[dat$city=="Albuquerque"])
#dat$month[dat$city=="Tucson"] <- month(ymd(subset(dat, city=="Tucson")$Date))
dat$month[dat$city=="Portland"] <- 
month(mdy(unlist(lapply(strsplit(subset(dat, city=="Portland")$Date,
split=" "), "[[", 1))))
dat$month[dat$city=="CharlotteMecklenburg"] <- 
(unlist(lapply(strsplit(subset(dat, city=="CharlotteMecklenburg")$Date,
split="-"), "[[", 2)))


dat$day <- day(mdy(dat$Date))
dat$day[!is.na(dmy(dat$Date))&is.na(mdy(dat$Date))] <- 
day(dmy(dat$Date))[!is.na(dmy(dat$Date))&is.na(mdy(dat$Date))]
dat$day[!is.na(ymd(dat$Date))&is.na(mdy(dat$Date))] <- 
day(ymd(dat$Date))[!is.na(ymd(dat$Date))&is.na(mdy(dat$Date))]
#dat$month[dat$city=="Albuquerque"] <- month(dmy(dat$Date)[dat$city=="Albuquerque"])
#dat$day[dat$city=="Albuquerque"] <- day(dmy(dat$Date)[dat$city=="Albuquerque"])
#dat$day[dat$city=="Tucson"] <- day(ymd(subset(dat, city=="Tucson")$Date))
dat$day[dat$city=="Portland"] <- 
day(mdy(unlist(lapply(strsplit(subset(dat, city=="Portland")$Date,
split=" "), "[[", 1))))

##add a fixed date.
dat$fdate <- paste(dat$month, paste(dat$day, 
dat$year, sep="/"), sep="/")

dat$fdate[dat$city=="Cleveland"&is.na(dat$month)] <- 
paste("1", 
paste("1",
subset(dat, city=="Cleveland"&is.na(month))$Date,
sep="/"), sep="/")

dat$fdate[dat$city=="New York"&is.na(dat$month)] <- 
paste("1", 
paste("1",
subset(dat, city=="New York"&is.na(month))$Date,
sep="/"), sep="/")

dat$fdate[dat$city=="Tampa"&is.na(dat$month)] <- 
paste("1", 
paste("1",
subset(dat, city=="Tampa"&is.na(month))$Date,
sep="/"), sep="/")

dat$fdate[dat$city=="CharlotteMecklenburg"] <- 
paste(subset(dat, city=="CharlotteMecklenburg")$month, 
paste("1",
subset(dat, city=="CharlotteMecklenburg")$year,
sep="/"), sep="/")

dat$fdate[dat$city=="Detroit"&is.na(dat$month)] <- 
paste("1", 
paste("1",
subset(dat, city=="Detroit"&is.na(month))$Date,
sep="/"), sep="/")

dat$fdate[dat$city=="Memphis"&is.na(dat$month)] <- 
paste("1", 
paste("1",
subset(dat, city=="Memphis"&is.na(month))$Date,
sep="/"), sep="/")

dat$fdate[dat$city=="Newark"&is.na(dat$month)] <- 
paste("1", 
paste("1",
subset(dat, city=="Newark"&is.na(month))$Date,
sep="/"), sep="/")

dat$fdate[dat$city=="LasVegas"&is.na(dat$month)] <- 
paste("1", 
paste("1",
subset(dat, city=="LasVegas"&is.na(month))$Date,
sep="/"), sep="/")

dat$fdate[dat$city=="MiamiDade"&is.na(dat$month)] <- 
paste("1", 
paste("1",
subset(dat, city=="MiamiDade"&is.na(month))$Date,
sep="/"), sep="/")

dat$fdate[dat$city=="NewOrleans"&is.na(dat$month)] <- 
paste("1", 
paste("1",
subset(dat, city=="NewOrleans"&is.na(month))$Date,
sep="/"), sep="/")

##recoding the fatal field.
dat$Fatal[dat$Fatal=="NF"] <- "N"
dat$Fatal[dat$Fatal=="Fatal"] <- "F"
dat$Fatal[dat$Fatal=="Neither"] <- "N;N"
dat$Fatal <- gsub("Y", "F", dat$Fatal)
dat$Fatal <- gsub(",", ";", dat$Fatal)
dat$Fatal <- gsub(":", ";", dat$Fatal)
dat$Fatal[dat$Fatal=="Non-Fatal"] <- "N"
dat$Notes[dat$Fatal%in%c(
"Suicide")] <- paste(dat$Notes, "Suicide", sep=". ")[dat$Fatal%in%c(
"Suicide")]
dat$Fatal[dat$Fatal=="Suicide"] <- "N"
dat$Fatal[dat$Fatal=="Continued"] <- "U"
dat$Notes[dat$Fatal%in%c(
"No hit")] <- paste(dat$Notes, "No hit", sep=". ")[dat$Fatal%in%c(
"No hit")]
dat$Notes[dat$Fatal%in%c(
"Miss")] <- paste(dat$Notes, "No hit", sep=". ")[dat$Fatal%in%c(
"Miss")]
dat$Fatal[dat$Fatal=="No Hit"] <- "N"
dat$Fatal[grepl("Miss", dat$Fatal)] <- gsub("Miss", "N",
dat$Fatal)[grepl("Miss", dat$Fatal)]
dat$Fatal <- gsub("Unknown", "U", dat$Fatal)
dat$Fatal <- gsub("1 F 1 N", "F;N", dat$Fatal)

dat$Fatal[grepl("no hits", tolower(dat$Notes))] <- "N"

#recoding race to fix some outliers
dat$SubjectRace[dat$SubjectRace=="Balck"] <- "B"
dat$SubjectRace[dat$SubjectRace=="BLACK"] <- "B"
dat$SubjectRace[dat$SubjectRace=="Black"] <- "B"
dat$SubjectRace[dat$SubjectRace=="Black or African American"] <- "B"
dat$SubjectRace[dat$SubjectRace=="LISPANIC/LATINO"] <- "L"
dat$SubjectRace[dat$SubjectRace=="Pacific Islander"] <- "A"
dat$SubjectRace[dat$SubjectRace=="Samoan?"] <- "A"
dat$SubjectRace[dat$SubjectRace=="UNKNOWN"] <- "U"
dat$SubjectRace[dat$SubjectRace=="WLITE"] <- "W"
dat$SubjectRace[dat$SubjectRace=="White"] <- "W"
dat$SubjectRace[dat$SubjectRace=="Filipino"] <- "A"
dat$SubjectRace[dat$SubjectRace=="F"] <- "A"
dat$SubjectRace[dat$SubjectRace=="Cambodian"] <- "A"
dat$SubjectRace[dat$SubjectRace=="Cambodia?"] <- "A"
dat$SubjectRace[dat$SubjectRace=="Indian"] <- "A"
dat$SubjectRace[dat$SubjectRace=="ASIAN"] <- "A"
dat$SubjectRace[dat$SubjectRace=="Asian or PaciFc islander"] <- "A"
dat$SubjectRace[dat$SubjectRace=="Hispanic"] <- "L"
dat$SubjectRace[dat$SubjectRace=="H"] <- "L"
dat$SubjectRace[dat$SubjectRace=="K"] <- ""
dat$SubjectRace[dat$SubjectRace=="V"] <- ""
dat$SubjectRace[dat$SubjectRace=="UNK"] <- ""
dat$SubjectRace[dat$SubjectRace=="N/A"] <- ""
dat$SubjectRace[dat$SubjectRace=="N\\A"] <- ""
dat$SubjectRace[dat$SubjectRace=="Unknown"] <- ""
dat$SubjectRace[dat$SubjectRace=="M"] <- ""
dat$SubjectRace[dat$SubjectRace=="p/m"] <- ""
dat$SubjectRace[dat$SubjectRace=="Native American"] <- ""
dat$SubjectRace[dat$SubjectRace=="Pending"] <- ""
dat$SubjectRace <- gsub("H", "L", dat$SubjectRace)
dat$SubjectRace <- gsub("Unknown", "U", dat$SubjectRace)

#recoding "subject armed" field
dat$Notes <- as.character(dat$Notes)

v <- c(
"Assault Rifle", "Handgun", "Sawed off Rifle",
"Shotgun","Gun","Vehicle","Y (BB Gun)", "Y (BB gun)",
"Y (Taser)","Y (Pellet Gun)",
"Scissors","Replica Gun","Simulated Gun",
"Knife","Sim","Rock","Y (Knife)","Car",
"Stolen CHP Vehicle", 
"Y (Knife/Cutting Instrument)",
"Yes (Knife/Cutting Instrument)",
"Y (knife)", 
"Knife, Machete", "Y (Blunt Object",
"Yes (Blunt Object)",
"Firearm", "Rifle", "Rifle, Handgun",
"Yes - Replica", "Yes - Knife",
"Yes - Knife & Replica", "Yes-Knife",
"Yes - golf clubs", "Yes - Ax",
"Yes-Vehicle", "Yes - Hatchet",
"Knife/ cutting instrument",
"Motor vehicle",
"Motor Vehicle", "Yes - Knife & Vehicle",
"Yes - replica","BB Gun",
"Knife/Saw", "Exacto knife",
"Metal Pole", "Metal Rod",
"Utility knife",
"Believed to be armed with phone, had cel phone instead.",
"Handgun; Handgun", "Handgun; None",
"Y (but discarded gun before actual OIS)",
"Umarex 40 XP CO2 BB gun, with a metal slide and a 19-round magazine",
"Y (with a rock)", "Y (with a rock)",
"Taser", "Y (replica handgun)", "N (car)",
"Knife, Gun", "Shotgun, knife", "Samurai sword",
"BB gun", ".38 caliber revolver",
"Semi-automatic plastic replica handgun",
"sword", "two handguns",
"Unknown", "rifle", "shotgun", "knives",
"knife", "gun", "handgun",
"Fake handgun","Large knife",
"Machete", "Pocketknife", "Double-bladed knife",
"Colt Defender BB air pistol",
"N(weapon)", "N(object", "N(club)",
"N;N(SteakKnife)","N(wrench)","N(taser)",
"N(WalkingCane)","N(SteakKnife)",
"N(PocketKnife)", "N(KitchenKnife)",
"N(pickaxe and hatchet)",
"N(crowbar);N;N(crowbar);N",
"N(ChefKnife)","N(BladeAndHammer)",
"N(2KitchenKnives)", "N (sword)",
"N (screwdriver)", "N (samuri sword)",
"N (replica gun)", "N (pruning shears)",
"N (pipe)", "N (pellet gun)", "N (military sword)",
"N (metal pipe)", "N (Machete)", "N (knife)",
"N (grenade)", "N (BB gun)", 
"No info -- Covert operation ",
"N;N(Misc Weapon)", "N(Misc Weapon)",
"N(Knife)", "N (Knife)",
"Fake gun i.e. black phone", "Crowbar",
"N(vehicle)","N(motorized vehicle)",
"N(knife/cutting instrument)","N(knife)",
"Handgun Replica", "No Weapon",
"Rifle & Handgun", "Y (grenade)"
)

dat$Notes[dat$SubjectArmed%in%v] <- 
paste(dat$Notes[dat$SubjectArmed%in%v], sep=". ")

wep <- c(
"Assault Rifle", "Handgun", "Sawed off Rifle",
"Shotgun","Gun","Y.", "yes", "Yes",
"Firearm", "Rifle", "Rifle, Handgun",
"Handgun; Handgun", "Handgun; None",
"Y (but discarded gun before actual OIS",
"rifle","gun","handgun",".38 caliber revolver",
"two handguns","Shotgun, knife", "Knife, Gun",
"shotgun","Rifle & Handgun", "Y (grenade)")

nwep <- c(
"Y (BB Gun)", "Y (BB gun)","Y (Taser)","Y (Pellet Gun)",
"Scissors","Replica Gun","Simulated Gun",
"Knife","Sim","Y (Knife)","Unarmed","Rock",
"Hands/feet","Car","Vehicle","Unarmed",
"Stolen CHP Vehicle", 
"Y (Knife/Cutting Instrument)",
"Yes (Knife/Cutting Instrument)",
"Y (knife)", "None",
"Knife, Machete", "Y (Blunt Object",
"Yes (Blunt Object)", "No",
"Yes - Replica", "Yes - Knife",
"Yes - Knife & Replica", "Yes-Knife",
"Yes - golf clubs", "Yes - Ax",
"Yes-Vehicle", "Yes - Hatchet",
"Knife/ cutting instrument",
"Motor vehicle", 
"Motor Vehicle", "Yes - Knife & Vehicle",
"Yes - replica", "BB Gun",
"Knife/Saw", "Exacto knife",
"Metal Pole", "Metal Rod",
"Utility knife",
"Believed to be armed with phone, had cel phone instead.",
"Umarex 40 XP CO2 BB gun, with a metal slide and a 19-round magazine",
"Semi-automatic plastic replica handgun",
"Samurai sword","BB gun","N (car)",
"Taser", "Y (replica handgun)", "Y (sword)",
"Y (with a rock)", "knife", "knives", "sword",
"Y (but discarded gun before actual OIS)",
"Fake handgun","Large knife",
"Machete", "Pocketknife", "Double-bladed knife",
"Colt Defender BB air pistol",
"N(weapon)", "N(object", "N(club)",
"N;N(SteakKnife)","N(wrench)","N(taser)",
"N(WalkingCane)","N(SteakKnife)",
"N(PocketKnife)", "N(KitchenKnife)",
"N(pickaxe and hatchet)",
"N(crowbar);N;N(crowbar);N",
"N(ChefKnife)","N(BladeAndHammer)",
"N(2KitchenKnives)", "N (sword)",
"N (screwdriver)", "N (samuri sword)",
"N (replica gun)", "N (pruning shears)",
"N (pipe)", "N (pellet gun)", "N (military sword)",
"N (metal pipe)", "N (Machete)", "N (knife)",
"N (grenade)", "N (BB gun)", 
"No info -- Covert operation ",
"N;N(Misc Weapon)", "N(Misc Weapon)",
"N(Knife)", "N (Knife)",
"Fake gun i.e. black phone", "Crowbar",
"N(vehicle)","N(motorized vehicle)",
"N(knife/cutting instrument)","N(knife)",
"Handgun Replica","No Weapon")

#reclassifying them appropriately
dat$SubjectArmed[dat$SubjectArmed%in%wep] <- "Y"

dat$SubjectArmed[dat$SubjectArmed%in%nwep] <- "N"

#adding an unknown category
dat$SubjectArmed[dat$SubjectArmed%in%c("UNK","Unclear",
"Pending","Other","Armed - Unknown Weapon",
"Y (Misc Weapon)", "Y (Y (Misc Weapon))",
"","BFT","N/A", "N;N;Uncleaer",
"Unknown","Assault on Officer")] <- "U"

#should now be only Y's, N's, and U's 
#(the "Uncleaer" is fine)
dat$SubjectArmed <- factor(dat$SubjectArmed)
table(dat$SubjectArmed)

#make the right field separators for race
dat$SubjectRace <- gsub(",", ";", dat$SubjectRace)
dat$SubjectRace <- gsub(" ", "", dat$SubjectRace)

#add race by category:
dat$b <- str_count(dat$SubjectRace, "B")
dat$l <- str_count(dat$SubjectRace, "L")
dat$w <- str_count(dat$SubjectRace, "W")
dat$a <- str_count(dat$SubjectRace, "A")
dat$tr <- dat$b+dat$l+dat$w+dat$a

#adding in age...
dat$SubjectAge <- gsub(",", ";", dat$SubjectAge)

table(unlist(lapply(strsplit(dat$SubjectAge, split=";"), function(x) 
{as.numeric(as.character(x))}))<18)
dat$AvgAge <- unlist(lapply(
lapply(strsplit(dat$SubjectAge, split=";"), function(x) 
{as.numeric(as.character(x))}), 
mean, na.rm=T))

#convert number of subjects to numeric
dat$NumberOfSubjects <- as.numeric(dat$NumberOfSubjects)

#create new "armed" field for analysis,
#which is set to "YES" if *any* subject is armed.
dat$VA <- grepl("Y", dat$SubjectArmed)

#create new fatality field for analysis,
#set to "YES" if *any* subject died.
dat$F <- grepl("F", dat$Fatal)

#number of shots
dat$nshots <- unlist(lapply(str_extract_all(
dat$NumberOfShots, "[0-9]*"), function(x)
{sum(as.numeric(x), na.rm=T)}))
dat$nshots[dat$nshots==0] <- NA

#Categorization of nature of stops
dat$sit <- NA
dat$sit[grepl("robbery|burglary|theft|shoplifting|robbey", 
tolower(dat$NatureOfStop))] <- "robbery"
dat$sit[grepl("suspect|wanted|warrant", tolower(dat$NatureOfStop))] <- "suspect"
dat$sit[grepl("car|vehicle|speeding|traffic", 
tolower(dat$NatureOfStop))] <- "traffic"
dat$sit[grepl("stolen vehicle|stolen car", tolower(dat$NatureOfStop))] <- "stolenvehicle"
dat$sit[grepl("suspicious|welfare|proactive|investigation|pursuit", 
tolower(dat$NatureOfStop))] <- "suspicious"
dat$sit[grepl("call for service|radio call", 
tolower(dat$NatureOfStop))] <- "call"
dat$sit[grepl("crime|assault|stabbing", 
tolower(dat$NatureOfStop))] <- "crime"
dat$sit[grepl("domestic", 
tolower(dat$NatureOfStop))] <- "dv"
dat$sit[grepl("off duty|off-duty", 
tolower(dat$NatureOfStop))|
grepl("off duty|off-duty", tolower(dat$Notes))] <- "off-duty"
dat$sit[grepl("narcots|drugs|drug", 
tolower(dat$NatureOfStop))] <- "drugs"
dat$sit[grepl("shots|shoot", 
tolower(dat$NatureOfStop))] <- "shooting"
dat$sit[grepl("man with a gun|weapon|armed", 
tolower(dat$NatureOfStop))] <- "weapon"
dat$sit[grepl("mental", tolower(dat$NatureOfStop))|
grepl("mental", tolower(dat$Notes))] <- "mental"
dat$sit[grepl("suicid", 
tolower(dat$NatureOfStop))] <- "suicide"
dat$sit[is.na(dat$sit)] <- "misc"

aggregate(F~sit, data=dat, mean)






