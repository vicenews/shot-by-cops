nrow(vic) - nrow(subset(vic, grepl("no hit", tolower(Notes))))
#That?s an average of more thanclose to 500 people per year.
(nrow(vic) - nrow(subset(vic, grepl(
  "no hit", tolower(Notes)
)))) / 7
#On more than 700 other occasions, police fired at citizens and missed.
nrow(subset(vic, grepl("no hit", tolower(Notes))))
#Two-thirds of people shot at by cops survive.
nrow(subset(vic, grepl("F", Fatal))) /
  nrow(subset(vic, grepl("F|N", Fatal)))

nrow(subset(vic, grepl("F", Fatal) &
              !grepl("no hit", tolower(Notes)))) /
  nrow(subset(vic, grepl("F|N", Fatal)))

#In all, our data set includes information on TK incidents
nrow(r)
#and 4,316205 subjects over seven years:
nrow(vic)

#Many people were unarmed. TK percent of the people police
#shot were unarmed.
#first remove cities with no armed status info.
r_va <- subset(
  r,
  !city %in% c(
    "Chicago",
    "Tampa",
    "City of Miami",
    "LosAngeles",
    "Indianapolis",
    "Detroit",
    "Newark",
    "BaltimoreCity",
    "DekalbCounty",
    "SanJose",
    "Columbus",
    "Cleveland",
    "Atlanta",
    "Philadelphia",
    "Boston",
    "San Francisco",
    "MiamiDade",
    "LasVegas",
    "Kansas City",
    "FairfaxCounty",
    "St. Louis",
    "Albuquerque",
    "Honolulu"
  )
)
vic_va <- subset(
  vic,
  !city %in% c(
    "Chicago",
    "Tampa",
    "City of Miami",
    "LosAngeles",
    "Indianapolis",
    "Detroit",
    "Newark",
    "BaltimoreCity",
    "DekalbCounty",
    "SanJose",
    "Columbus",
    "Cleveland",
    "Atlanta",
    "Philadelphia",
    "Boston",
    "San Francisco",
    "MiamiDade",
    "LasVegas",
    "Kansas City",
    "FairfaxCounty",
    "St. Louis",
    "Albuquerque",
    "Honolulu"
  )
)
#
vic_va <- subset(
  vic,
  !city %in% c(
    "Chicago",
    "Tampa",
    "City of Miami",
    "LosAngeles",
    "Indianapolis",
    "Detroit",
    "Newark",
    "BaltimoreCity",
    "DekalbCounty",
    "SanJose",
    "Columbus",
    "Cleveland"
  )
)
prop.table(table(vic_va$weapon))
#That?s *quadruple* the unarmed rate the Washington Post
#found for the country as a whole.
#wapo rate was ~5%
#In roughly TK percent of cases, we don?t know
#if the subject was armed or not.
prop.table(table(vic_va$weapon))
#About TK of shootings occurred when officers
#encountered a subject with a gun.
prop.table(table(vic_va$weapon))
#Another TK percent of subjects were armed with a knife or something else.
sum(prop.table(table(vic_va$weapon))[2:4])

#Police shot at least TK black people from 2010 through 2016,
nrow(subset(vic, b))
#TK percent of the total and more than
nrow(subset(vic, b)) / nrow(subset(vic, tr))
#TK the share of the black population in these communities.
#That?s a 28 percent higher rate than the rate Washington Post
#found for fatal shootings in the same communities.

#Black subjects shot by police were TK
prop.table(table(subset(vic_va, b)$weapon))
prop.table(table(subset(vic_va, w)$weapon))
#and were more likely to be shot during incidents that
#began with routine traffic or pedestrian stops.
nrow(subset(r, b > 0 & sit %in% c("traffic", "suspicious"))) /
  nrow(subset(r, b > 0 & !sit == "misc"))
nrow(subset(r, w > 0 & sit %in% c("traffic", "suspicious"))) /
  nrow(subset(r, w > 0 & !sit == "misc"))

#Total police shootings are down across these departments.
table(subset(
  r,
  !city %in% c("Atlanta", "Detroit", "SanDiego",
               "Louisville", "Washington DC")
)$year)
#That?s mostly a function of the big-city departments reducing
# their number of nonfatal shootings
table(subset(
  r,
  city %in% c(
    "New York",
    "Chicago",
    "LosAngeles",
    "Philadelphia",
    "Houston",
    "Washington DC",
    "Dallas",
    "Phoenix",
    "BaltimoreCity",
    "MiamiDade"
  ) & grepl("N", Fatal)
)$year)

table(subset(
  r,
  !city %in% c(
    "New York",
    "Chicago",
    "LosAngeles",
    "Philadelphia",
    "Houston",
    "Washington DC",
    "Dallas",
    "Phoenix",
    "BaltimoreCity",
    "MiamiDade",
    "Detroit",
    "Atlanta",
    "SanDiego",
    "Louisville",
    "Washington DC",
    "LasVegas",
    "Memphis",
    "Milwaukee",
    "San Antonio",
    "San Francisco",
    "Boston"
  )
)$year)

#In our data, big departments with the most shootings had
#had on average TK per year,
a <- data.frame(table(r$city))
mean(head(a[order(a$Freq, decreasing = T), 2], 10)) / 7
#and smaller departments saw as few as TK.
mean(head(a[order(a$Freq), 2], 10)) / 7

#A dozen departments would not release data on race or said
#they did not keep it. Race information was available for
#about TK percent of incidents
nrow(subset(r, tr > 0)) / nrow(r)
#and TK subjects.
nrow(subset(vic, tr))

#This data shows a much graver impact on black people
#than previous efforts to track police shootings.
#Police shot black people two and a half times more often than white people.
(sum(vic$b, na.rm = T) / nrow(subset(vic, tr))) /
  (sum(vic$w, na.rm = T) / nrow(subset(vic, tr)))
#Police shot Hispanics slightly more often than whites and
(sum(vic$l, na.rm = T) / nrow(subset(vic, tr))) /
  (sum(vic$w, na.rm = T) / nrow(subset(vic, tr)))
#Asians far less often than any other race. (Keep in mind that more people of color live in these communities than in the country as a whole, so rates are likely to be higher.)
nrow(subset(vic, a))
#Cases where cops shoot unarmed people often draw outrage,
#and looking only at fatal shootings excludes many of these.
#TK unarmed people were shot by cops in this data.
nrow(subset(vic_va, weapon == "unarmed"))
#A TK of them were black.
nrow(subset(vic_va, weapon == "unarmed" & b))
#In another TK percent of cases there was no information about
#whether the subject was armed
nrow(subset(vic_va, weapon == "U")) / nrow(vic_va)
#(TK departments including Chicago and Los Angeles didn?t
#reliably provide the armed status of the people police shot,
#so they aren?t included here.)
a <- aggregate(weapon == "U" ~ city, data = r, mean)
colnames(a)[2] <- "wep"
head(a[order(a$wep, decreasing = T), ], 20)
#Not every department kept descriptions or full narratives
#of officer-involved shootings, but information was available
#on almost TK incidents.
nrow(subset(r,!is.na(FullNarrative) & !FullNarrative == ""))

#A TK of shootings of black people began as
#relatively innocuous pedestrian or traffic stops,
#compared to TK percent for white people.
nrow(subset(r, b > 0 & sit %in% c("traffic", "suspicious"))) /
  nrow(subset(r, b > 0 & !sit == "misc"))
nrow(subset(r, w > 0 & sit %in% c("traffic", "suspicious"))) /
  nrow(subset(r, w > 0 & !sit == "misc"))
#On the other hand, black subjects were more likely be
#committing a robbery or involved in a shooting.
prop.table(table(subset(vic, b & !sit == "misc")$sit))
#Whites were more often involved in mental health crisis or
#domestic violence incidents and other serious crimes.
prop.table(table(subset(vic, w & !sit == "misc")$sit))

#Black subjects tended to be younger,
mean(subset(r, b > 0)$AvgAge, na.rm = T)
mean(subset(r, w > 0)$AvgAge, na.rm = T)
#and almost TK percent were under the age of 18,
prop.table(table(unlist(lapply(strsplit(subset(r, b > 0 &
                                                 !w > 0 & !l > 0 & !a > 0)$SubjectAge,
                                        split = ";"), function(x)
                                        {
                                          as.numeric(as.character(x))
                                        })) < 18))
#compared with TK percent of white subjects.
prop.table(table(unlist(lapply(strsplit(subset(r, w > 0 &
                                                 !b > 0 & !l > 0 & !a > 0)$SubjectAge,
                                        split = ";"), function(x)
                                        {
                                          as.numeric(as.character(x))
                                        })) < 18))

#In the three years since Ferguson, the total number of police
#shootings has fallen by about TK percent.
table(subset(
  r,
  !city %in% c("Atlanta", "Detroit", "SanDiego",
               "Louisville", "Washington DC")
)$year)

#
cons <- subset(
  r,
  city %in% c(
    "Seattle",
    "NewOrleans",
    "Miami",
    "Albuquerque",
    "Portland",
    "BaltimoreCity",
    "Cleveland",
    "Newark"
  )
)
cops <- subset(
  r,
  city %in% c(
    "Milwaukee",
    "Philadelphia",
    "Memphis",
    "LasVegas",
    "Chicago",
    "San Francisco",
    "BaltimoreCity"
  )
)
y <- aggregate(Date ~ year + city, data = cops, length)
y <- rbind(y, data.frame(
  year = c(2017),
  city = "Chicago",
  Date = c(27)
))
y$id <- paste(y$city, y$year, sep = "-")

z <- aggregate(Date ~ year + city, data = cons, length)
z <- rbind(z, data.frame(
  year = c(2017),
  city = "BaltimoreCity",
  Date = c(5)
))
z$id <- paste(z$city, z$year, sep = "-")

cops <- c(
  "San Francisco-2016",
  "Philadelphia-2013",
  "Chicago-2017",
  "Milwaukee-2015",
  "Memphis-2016",
  "LasVegas-2012",
  "BaltimoreCity-2014"
)
y$cops <- y$id %in% cops

cons <- c(
  "Portland-2012",
  "Albuquerque-2014",
  "Miami-2016",
  "NewOrleans-2012",
  "Seattle-2012",
  "BaltimoreCity-2017",
  "Cleveland-2015",
  "Newark-2016"
)
z$cons <- z$id %in% cons
v <- vector()
for (i in 1:nrow(z))
{
  v[i] <- as.numeric(z$year[i]) -
    as.numeric(subset(z, city == z$city[i] & cons)$year)
}
z$ys <- v

v <- vector()
for (i in 1:nrow(y))
{
  v[i] <- as.numeric(y$year[i]) -
    as.numeric(subset(y, city == y$city[i] & cops)$year)
}
y$ys <- v

##create normalized numbers for each city
v <- aggregate(Date ~ city, data = z, mean)
z$norm <- as.numeric(z$Date) / v[match(z$city, v$city), 2]

v <- aggregate(Date ~ city, data = y, mean)
y$norm <- as.numeric(y$Date) / v[match(y$city, v$city), 2]

##Cities that voluntarily adopted DOJ-recommended reforms
a <- aggregate(Date ~ city, data = subset(y, ys < 0), mean)
b <- aggregate(Date ~ city, data = subset(y, ys == 0), mean)
a$aft <- b[match(a$city, b$city), 2]
v <- a
mean((a$Date - a$aft) / a$Date)
#
a <- aggregate(Date ~ city, data = subset(z, ys < 0), mean)
b <- aggregate(Date ~ city, data = subset(z, ys == 0), mean)
a$aft <- b[match(a$city, b$city), 2]
mean((a$Date - a$aft) / a$Date)
a <- rbind(a, v)
mean((a$Date - a$aft) / a$Date)
#(without Balt/Chicago.)
a <- a[c(1, 4:7), ]
mean((a$Date - a$aft) / a$Date)










