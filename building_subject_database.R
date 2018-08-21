#libraries
library(lme4)
library(lubridate)
library(stringr)
library(ggplot2)

#data input/subsetting
#limit to focal window: 2010-2016
r <- subset(dat, year > 2009 & year < 2017)
#UPDATE! Don't remove no hits.
#remove no hit incidents
#r <- subset(r, !grepl("no hits", tolower(r$Notes)))
#r <- subset(r, !grepl("Miss", r$Fatal))
#r <- subset(r, !grepl("No Hit", r$Fatal))

#add official department names

#Do screen out accidental discharges with no subjects.
r <- subset(r,!(
  grepl("accidental discharge|accidental fire", FullNarrative) &
    grepl("no hit", tolower(Notes))
))

#Remove suicides.
r <-
  subset(r,!grepl("self-inflicted|self inflicted", FullNarrative))
r <- subset(r,!grepl("self-inflicted|self inflicted", Notes))
r <-
  subset(
    r,
    !grepl(
      "https://www.lvmpd.com/en-us/InternalOversightConstitutionalPolicing/Documents/Non-Fatal%20OIS/FITReport_120316-4534_NonFatal_FINAL.pdf",
      FullNarrative
    )
  )
r <-
  subset(
    r,
    !grepl(
      "Full narrative via news reports at: http://www.elpasotimes.com/story/news/crime/2015/09/19/autopsy-charles-bertram-death-suicide/72440626/",
      FullNarrative
    )
  )
r <-
  subset(r,!grepl("Suspect then committed suicide.", FullNarrative))
r <- subset(r,!grepl("Subject committed suicide.", FullNarrative))

#changing one incident in Vegas to remove the person who committed suicide
r$NumberOfSubjectss[r$Notes == "2 of 5 cops died; 5 people died in total; victim committed suicide"] <-
  1
r$Fatal[r$Notes == "2 of 5 cops died; 5 people died in total; victim committed suicide"] <-
  "F"
r$SubjectArmed[r$Notes == "2 of 5 cops died; 5 people died in total; victim committed suicide"] <-
  "Y"
r$SubjectGender[r$Notes == "2 of 5 cops died; 5 people died in total; victim committed suicide"] <-
  "M"
r$SubjectAge[r$Notes == "2 of 5 cops died; 5 people died in total; victim committed suicide"] <-
  31
r$SubjectRace[r$Notes == "2 of 5 cops died; 5 people died in total; victim committed suicide"] <-
  "U"

#remove all the incidents from non-top-50 cities
r <- subset(
  r,
  !city %in% c(
    "Colorado Springs",
    "Oklahoma City",
    "Wichita",
    "Oakland",
    "Raleigh",
    "Virginia",
    "Long Beach"
  )
)

#and remove some rogue departments
r <- subset(
  r,
  !Department %in% c(
    "Fairfield County Sheriff's Office",
    "Franklin County Adult probation officer",
    "Franklin County Sheriff's Office",
    "Ohio State Highway Patrol",
    "Westerville Police",
    "Denver Sheriffs Department",
    "Unknown"
  )
)

#setting cases with unknown victim numbers equal to one
r$NumberOfSubjects[is.na(r$NumberOfSubjects)] <- 1

#gonna do it in two steps: one for all the
#single-subject incidents, one for multiple
v <- subset(r, NumberOfSubjects == 1)
x <- subset(r, NumberOfSubjects > 1)

#found minor data entry issues
x$SubjectRace <- gsub("'", ";", x$SubjectRace)
x$Fatal <- gsub("1 F 1 N", "F;N", x$Fatal)
x$Fatal <- gsub("Unknown", "U;U", x$Fatal)

#now multiple
repr <- function(X, Y)
{
  Z <- vector()
  for (i in 1:length(X))
  {
    Z <- append(Z,
                rep(as.character(X[i]), Y[i]))
  }
  return(Z)
}
repr(x$Date, x$NumberOfSubjects)

y <- data.frame(
  Date = repr(x$Date, x$NumberOfSubjects),
  NumberOfSubjects = 1,
  Fatal = NA,
  SubjectArmed = repr(x$SubjectArmed, x$NumberOfSubjects),
  Notes = repr(x$Notes, x$NumberOfSubjects),
  SubjectRace = NA,
  SubjectGender = NA,
  SubjectAge = NA,
  NatureOfStop = repr(x$NatureOfStop, x$NumberOfSubjects),
  NumberOfShots = repr(x$NumberOfShots, x$NumberOfSubjects),
  NumberOfOfficers = repr(x$NumberOfOfficers, x$NumberOfSubjects),
  OfficerRace = repr(x$OfficerRace, x$NumberOfSubjects),
  OfficerGender = repr(x$OfficerGender, x$NumberOfSubjects),
  FullNarrative = repr(x$FullNarrative, x$NumberOfSubjects),
  Department = repr(x$Department, x$NumberOfSubjects),
  city = repr(x$city, x$NumberOfSubjects),
  year = repr(x$year, x$NumberOfSubjects),
  month = repr(x$month, x$NumberOfSubjects),
  day = repr(x$day, x$NumberOfSubjects),
  fdate = repr(x$fdate, x$NumberOfSubjects),
  b = NA,
  l = NA,
  w = NA,
  a = NA,
  tr = NA,
  AvgAge = NA,
  VA = NA,
  F = NA,
  nshots = NA,
  sit = NA
)

#adding fatality
z <- vector()
for (i in 1:nrow(x))
{
  if (!length(unlist(strsplit(x$Fatal[i],
                              split = ";"))) == x$NumberOfSubjects[i])
  {
    z <- append(z, rep(x$Fatal[i], x$NumberOfSubjects[i]))
    
    next
  }
  if (!is.na(x$Fatal[i]) & !x$Fatal[i] == "" &
      !x$Fatal[i] == "U")
  {
    z <- append(z, unlist(strsplit(x$Fatal[i],
                                   split = ";")))
  }
}
y$Fatal <- z

#adding race
z <- vector()
for (i in 1:nrow(x))
{
  if (!length(unlist(strsplit(x$SubjectRace[i],
                              split = ";"))) == x$NumberOfSubjects[i])
  {
    z <- append(z, rep(x$SubjectRace[i], x$NumberOfSubjects[i]))
    
    next
  }
  if (!is.na(x$SubjectRace[i]) & !x$SubjectRace[i] == "")
  {
    z <- append(z, unlist(strsplit(x$SubjectRace[i],
                                   split = ";")))
  }
  else
  {
    z <- append(z, rep(NA, x$NumberOfSubjects[i]))
  }
}
y$SubjectRace <- z

#subject-armed-ness
x$SubjectArmed <- gsub(",", ";", x$SubjectArmed)
x$SubjectArmed <- gsub(":", ";", x$SubjectArmed)
z <- vector()
for (i in 1:nrow(x))
{
  if (!is.na(x$SubjectArmed[i]) & !x$SubjectArmed[i] == "" &
      x$NumberOfSubjects[i] == length(unlist(strsplit(as.character(x$SubjectArmed[i]), split =
                                                      ";"))))
  {
    z <- append(z, unlist(strsplit(
      as.character(x$SubjectArmed[i]),
      split = ";"
    )))
  }
  if (!x$NumberOfSubjects[i] == length(unlist(strsplit(as.character(x$SubjectArmed[i]), split =
                                                       ";"))) &
      !is.na(x$SubjectArmed[i]) & !x$SubjectArmed[i] == "")
  {
    z <- append(z, rep(as.character(x$SubjectArmed[i]),
                       x$NumberOfSubjects[i]))
  }
  if (is.na(x$SubjectArmed[i]) | x$SubjectArmed[i] == "")
  {
    z <- append(z, rep(NA, x$NumberOfSubjects[i]))
  }
}
#ironing out some issues.
z <- gsub("N;N", "N", z)
z <- gsub("N;N;N", "N", z)
y$SubjectArmed <- z


##will add other features as necessary.
#for now, a complete subject-based dataset.

vic <- rbind(v, y)

vic$VA <- grepl("Y", vic$SubjectArmed)
vic$F <- grepl("F", vic$Fatal)

vic$b <- vic$SubjectRace == "B"
vic$a <- vic$SubjectRace == "A"
vic$l <- vic$SubjectRace == "L"
vic$w <- vic$SubjectRace == "W"
vic$tr <- vic$SubjectRace %in% c("B", "A", "L", "W")
#

vic$other <- grepl("club|vehicle as weapon|baseball bat",
                   tolower(paste(vic$Notes, vic$FullNarrative, sep = ". "))) |
  grepl(
    "vehicle|rock|taser|blunt|pipe|misc weapon|screwdriver|crowbar|shears|cane|pole|rod|scissors",
    tolower(vic$Notes)
  )
vic$replica <- grepl("replica|simulated|bb|imitation|paintball",
                     tolower(paste(vic$Notes, vic$FullNarrative, sep = ". ")))
vic$knife <- grepl("knife|knives|sword| ax|hatchet|machete|blade",
                   tolower(paste(vic$Notes, vic$FullNarrative, sep = ". ")))

r$other <- grepl("club|vehicle as weapon|baseball bat",
                 tolower(paste(r$Notes, r$FullNarrative, sep = ". "))) |
  grepl(
    "vehicle|rock|taser|blunt|pipe|misc weapon|screwdriver|crowbar|shears|cane|pole|rod|scissors",
    tolower(r$Notes)
  )
r$replica <- grepl("replica|simulated|bb|imitation|paintball",
                   tolower(paste(r$Notes, r$FullNarrative, sep = ". ")))
r$knife <- grepl("knife|knives|sword| ax|hatchet|machete|blade",
                 tolower(paste(r$Notes, r$FullNarrative, sep = ". ")))

r$weapon <- NA
r$weapon[r$SubjectArmed %in% c("U", "U;U", "U;U;U")] <- "U"
r$weapon[is.na(r$SubjectArmed)] <- "U"
r$weapon[r$other] <- "other"
r$weapon[r$replica] <- "replica"
r$weapon[r$knife] <- "knife"
r$weapon[grepl("Y", r$SubjectArmed)] <- "gun"
r$weapon[is.na(r$weapon)] <- "unarmed"

vic$weapon <- NA
vic$weapon[vic$SubjectArmed %in% c("U", "U;U", "U;U;U")] <- "U"
vic$weapon[is.na(vic$SubjectArmed)] <- "U"
vic$weapon[vic$other] <- "other"
vic$weapon[vic$replica] <- "replica"
vic$weapon[vic$knife] <- "knife"
vic$weapon[grepl("Y", vic$SubjectArmed)] <- "gun"
vic$weapon[is.na(vic$weapon)] <- "unarmed"


write.csv(vic, "../SubjectBased_Final.csv")
write.csv(r, "../IncidentBased_Final.csv")











