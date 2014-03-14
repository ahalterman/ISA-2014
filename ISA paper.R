# This code is a tremendously messy first draft. It makes a big assumption about the data, namely that you have the full GDELT set and it's in a SQLite database

library(ggplot2)
library(grid)   # only for tweaking legend margin sizes
#library(ggmap)
#library(mapproj)
library(countrycode)
library(dplyr)
library(RSQLite)
library(RSQLite.extfuns)
library(gdeltr)
library(reshape2)
library(scales)
library(extrafont)
# loadfonts()  # Do this when you run it the first time. Needed for Gill Sans fonts

# Fix for dplyr compliler: http://stackoverflow.com/questions/19503995/error-when-with-xcode-5-0-and-rcpp-command-line-tools-are-installed

gdelt.db <- src_sqlite("~/gdelt.db")
daily.db <- tbl(gdelt.db, "GDELT_DAILYUPDATES")
hist.db <- tbl(gdelt.db, "GDELT_HISTORICAL")

## Civil society activity for each country, showing +/- 3 years, combined CVL/OPP and EventCode. Aggregate by month. Get number, get avg. tone.

################  CivilSociety subset function  ###################
CivilSoc <- function(country, startdate, enddate, output, save){
  ## put the dates in yyyymmdd format. (numeric)
  ## "output" will take c("raw", "month.percent", "month.count", "tone")
  if(enddate > 20130331){
    stop("This range is not inside the historical GDELT table")
  }
  ## Add in padder so you can just feed it years
  fips <- countrycode(country, "country.name", "fips104")
  iso <- countrycode(country, "country.name", "iso3c")
  
  ## THIS IS DEPRECIATED. WE ONLY USE ACTOR CODES, NOT EVENT CODES
  # Four filters:   1. Inside the date range.
  #                 2. Actors have to be from the country or unspecified.
  #                 3. Action has to be inside the country.
  #                 4. Actors have to be civil/political society OR doing something obviously civil society.
  # "Obviously civil society" means:
  # 0243  Appeal for rights
  # 0244  Appeal for change in institutions, regime
  # 104  Demand political reform, not specified below
  # 1041  Demand change in leadership
  # 1042  Demand policy change
  # 1043  Demand rights
  # 1044	Demand change in institutions, regime
  # 105	Demand mediation
  # 1051	Demand easing of administrative sanctions
  # 113  Rally opposition against
  
  # 024	Appeal for political reform, not specified below
  # 0241	Appeal for change in leadership
  # 0242	Appeal for policy change
  # 0243	Appeal for rights
  # 0244	Appeal for change in institutions, regime
  # 0251	Appeal for easing of administrative sanctions
  # 0253	Appeal for release of persons or property
  # 1121	Accuse of crime, corruption
  # 1122	Accuse of human rights abuses
  # 1242	Refuse to ease popular dissent
  # 133	Threaten political dissent, protest
  
 # civsoc <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""), (Actor1Type1Code=="NGO" |Actor1Type1Code=="CVL" | Actor1Type1Code=="OPP" | Actor1Type1Code=="LAB" | Actor1Type1Code=="MED" | EventRootCode=="14" | EventCode=="0243" | EventCode=="0244" | EventCode=="104" | EventCode=="1041" | EventCode=="1042" | EventCode=="1043" | EventCode=="1044" | EventCode=="105" | EventCode=="1051" | EventCode=="113" | EventCode=="024" | EventCode=="0241" | EventCode=="0242" | EventCode=="0243" | EventCode=="0244" | EventCode=="0251" | EventCode=="0253" | EventCode=="1121" | EventCode=="1122" | EventCode=="1242" | EventCode=="133"))
  
  civsoc <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""), (Actor1Type1Code=="NGO" | Actor1Type1Code=="CVL" | Actor1Type1Code=="OPP" | Actor1Type1Code=="LAB" | Actor1Type1Code=="MED" | Actor1Type1Code=="HRI"))
  
  if(output=="raw"){
    return(civsoc)
    ## pipe this into a new object, then you can filter that object before as.data.framing it.
  }
  
  if(output=="month.count"){
    civsoc <- select(civsoc, MonthYear)
    civsoc <- group_by(civsoc, MonthYear)
    civsoc <- summarise(civsoc, count=n())
    civsoc <- as.data.frame(civsoc)
    if(save==TRUE){
      filenm <- paste0(country, ".civsoc.csv")
      write.csv(civsoc, file=filenm)
    }
    if(save==FALSE){
      return(civsoc)
    }    ## read in rda of pre-calculated sums
  }
  
  if(output=="month.percent"){
    civsoc <- select(civsoc, MonthYear)
    civsoc <- group_by(civsoc, MonthYear)
    civsoc <- summarise(civsoc, count=n())
    count <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""))
    count <- group_by(count, MonthYear)
    count <- summarise(count, total=n())
    count <- as.data.frame(count)
    civsoc <- merge(civsoc, count, by="MonthYear")
    civsoc$Country <- country
    if(save==TRUE){
      filenm <- paste0(country, ".civsoc.csv")
      write.csv(civsoc, file=filenm)
    }
    if(save==FALSE){
      return(civsoc)
    }
  }
  if(output=="month.percent.fast"){
    civsoc <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""))
      
    civsoc <- select(hist.db, MonthYear)
    civsoc <- group_by(civsoc, MonthYear)
    civsoc <- summarise(civsoc, count=n())
    count <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""))
    count <- group_by(count, MonthYear)
    count <- summarise(count, total=n())
    count <- as.data.frame(count)
    civsoc <- merge(civsoc, count, by="MonthYear")
  }
}



##########  GOV to CVL  ###########

GovToCiv <- function(country, startdate, enddate, output){
  ## put the dates in yyyymmdd format. (numeric)
  ## "output" will take c("raw", "month.percent", "month.count", "tone")
  if(enddate > 20130331){
    stop("This range is not inside the historical GDELT table")
  }
  
  fips <- countrycode(country, "country.name", "fips104")
  iso <- countrycode(country, "country.name", "iso3c")
  civsoc <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""), (Actor1Type1Code=="GOV" | Actor1Type1Code=="COP" | Actor1Type1Code=="MIL"), (Actor2Type1Code=="CVL" | Actor2Type1Code=="OPP" | Actor2Type1Code=="MED" | Actor2Type1Code=="LAB" | Actor2Type1Code=="NGO" | Actor2Type1Code=="HRI"))
                                                                                                                                                                                                   
  if(output=="raw"){
    return(civsoc)
    ## pipe this into a new object, then you can filter that object before as.data.framing it.
  }
  
  if(output=="month.count"){
    civsoc <- select(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, QuadClass)
    civsoc <- summarise(civsoc, count=n())
    civsoc <- as.data.frame(civsoc)
    return(civsoc)
  }
  if(output=="month.percent"){
    civsoc <- select(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, QuadClass)
    civsoc <- summarise(civsoc, count=n())
    count <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""))
    count <- group_by(count, MonthYear)
    count <- summarise(count, total=n())
    count <- as.data.frame(count)
    civsoc <- merge(civsoc, count, by="MonthYear")
  }
}
  

###############   Baseline function  ###############
# This is included in the CivilSociety function in modified form.

baseline <- function(country, startdate, enddate, period){
  ## calculate the number of total events for a given country, occurring domestically, in a given date range.
  if(enddate > 20130331){
    stop("This range is not inside the historical GDELT table")
  }
  fips <- countrycode(country, "country.name", "fips104")
  iso <- countrycode(country, "country.name", "iso3c")
  count <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""))
  if(period=="month"){
    count <- group_by(count, MonthYear)
    count <- summarise(count, total=n())
    count <- as.data.frame(count)
    name.count <- paste0(iso, ".", "month.csv")
    return(count)
    #write.csv(count, file=name.count)
  }
  if(period=="day"){
    count <- group_by(count, SQLDATE)
    count <- summarise(count, total=n())
    count <- as.data.frame(count)
    name.count <- paste0(iso, ".", "day.csv")
    count <- as.data.frame(count)
    return(count)
  }
}


GDELT.tone <- function(tbl){
  ###  ???
}


test <- select(hist.db, SQLDATE, ActionGeo_CountryCode, Actor1CountryCode)
test <- filter(test, SQLDATE < 20001231, ActionGeo_CountryCode=="UP")
test <- group_by(test, Actor1CountryCode)
test <- summarise(test, count=n())
system.time(test <- as.data.frame(test, n=-1))


## baseline with just OPP and CVL
test <- select(hist.db, SQLDATE, EventCode, ActionGeo_CountryCode, Actor1CountryCode, Actor2CountryCode, MonthYear)
test <- filter(test, SQLDATE >= 19940101, SQLDATE <= 19991231, ActionGeo_CountryCode=="RO", (Actor1CountryCode=="ROU" | Actor1CountryCode==""), (Actor2CountryCode=="ROU" | Actor2CountryCode==""), (Actor1Type1Code=="CVL" | Actor1Type1Code=="OPP"))
system.time(test <- as.data.frame(summarise(group_by(test, MonthYear), count=n())))

df <- as.data.frame(summarise(group_by(filter(select(hist.db, SQLDATE), SQLDATE > 19800401), SQLDATE), count=n()), n=-1)
df$SQLDATE <- gDate(df$SQLDATE)
ggplot(data=df, aes(x=SQLDATE, y=count)) + geom_line() + theme_bw() 


## Could also include GOV/COP/MIL codes acquising to CVL/OPP.

# Romania (1996)  1993-1999
romania <- CivilSoc("Romania", "1994", "1996")

# Croatia
system.time(croatia <- CivilSoc("croatia", 19950101, 20041231, output="month.count"))
system.time(bline <- baseline("croatia", 19950101, 20041231, period="month"))
croatia <- merge(croatia, bline, by="MonthYear")
croatia$Date <- gDate(paste0(croatia$MonthYear, "01"))
croatia$percent <- croatia$count / croatia$total
ggplot(croatia, aes(x=Date, y=percent)) + geom_line() + theme_bw() + ggtitle("Croatia: Civil Society Events as Percentage of Total")

## Serbia
system.time(serbia <- CivilSoc("serbia", 19950101, 20041231, output="month.count"))
system.time(bline <- baseline("serbia", 19950101, 20041231, period="month"))
serbia <- merge(serbia, bline, by="MonthYear")
serbia$Date <- gDate(paste0(serbia$MonthYear, "01"))
serbia$percent <- serbia$count / serbia$total
ggplot(serbia, aes(x=Date, y=percent)) + geom_line() + theme_bw() + ggtitle("Serbia: Civil Society Events as Percentage of Total")

# Ukraine
system.time(ukraine <- CivilSoc("ukraine", 19960101, 20051231, output="month.count"))
system.time(bline <- baseline("ukraine", 19960101, 20051231, period="month"))
ukraine <- merge(ukraine, bline, by="MonthYear")
ukraine$Date <- gDate(paste0(ukraine$MonthYear, "01"))
ukraine$percent <- ukraine$count / ukraine$total
ggplot(ukraine, aes(x=Date, y=percent)) + geom_line() + theme_bw() + ggtitle("Ukraine: Civil Society Events as Percentage of Total")

system.time(croatia.gtc <- GovToCiv("croatia", 19950101, 20050101, output="month.count"))
croatia <- croatia.gtc

croatia$Date <- gDate(paste0(croatia$MonthYear, "01"))
croatia$QuadClass <- as.factor(croatia$QuadClass)
total <- summarise(group_by(croatia, MonthYear), count=sum(count))
names(total) <- c("MonthYear", "total")
croatia <- merge(croatia, total, by="MonthYear")
croatia$percent <- (croatia$count / croatia$total) *100
levels(croatia$QuadClass)[levels(croatia$QuadClass)=="1"] <- "Verbal Coop."
levels(croatia$QuadClass)[levels(croatia$QuadClass)=="2"] <- "Material Coop."
levels(croatia$QuadClass)[levels(croatia$QuadClass)=="3"] <- "Verbal Confl."
levels(croatia$QuadClass)[levels(croatia$QuadClass)=="4"] <- "Material Confl."
722croatia.gg <- ggplot(data=croatia, aes(x=Date, y=percent, color=QuadClass)) + geom_line(size=1, alpha=0.6) + theme_bw() + stat_smooth(method="loess", se=FALSE, span=0.3, size=2)

fipsify <- function(country){
  require(countrycode)
  print(countrycode(country, "country.name", "fips104"))
}

df <- as.data.frame(filter(hist.db, SQLDATE==19950101))




############  OTI Section  ##############
# We want to compare the civil society percentage in the two years OTI funding with the 2 years after AND
#  compare to baseline EE/postcommunist countries that didn't get OTI funding.

# return a df with country, month, number of CS events, total events, % for each PC/EE country, 1992-2005.

system.time(croatia <- CivilSoc("croatia", "19920101", "20051231", "month.percent"))
# 989.811 

easterneurope <- c("Georgia", "Armenia", "Azerbaijan", "Ukraine", "Belarus", "Moldova", "Croatia", "Czech Republic", "Slovakia", "Hungary", "Poland", "Serbia", "Slovenia", "Bosnia", "Albania", "Bulgaria", "Romania", "Macedonia")

CivilSoc(country="Georgia", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Armenia", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Azerbaijan", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Ukraine", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Belarus", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Moldova", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Czech Republic", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Slovakia", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Hungary", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Poland", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Serbia", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Slovenia", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Bosnia", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Albania", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Bulgaria", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Romania", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Macedonia", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Croatia", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Kyrgyzstan", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)
CivilSoc(country="Kosovo", startdate=19920101, enddate=20051231, output="month.count", save=TRUE)

system("cat *.civsoc.csv > allciv.csv")



#########    GKG Protests   ###########
setwd("~/GKG Data/Months")
protest <- read.csv("allProtest.csv", stringsAsFactors=FALSE)

themeLocator <- function(df, locations, overlay=TRUE, returndata=FALSE){
  require(gdeltr)
  require(ggplot2)
  require(countrycode)
  # df should preferably just be COUNTS, THEMES, DATE for the region (or theme?) you're interested in.
  # location should be a vector of countries.
  locations.fips <- countrycode(locations, "country.name", "fips104")
  theme.counts <- data.frame()
  location.i <- paste0("#", locations.fips[i], "#")
  
  for(i in 1:length(locations)){
    # loop through the themes vector, return # per day of each.
    tmp <- data.frame()
    df$Number <- sapply(df$COUNTS, function(x) length(grep(location.i, unlist(strsplit(x, ";")))))
    str(df)
    df$Country <- locations[i]
    ## trim it down?
    theme.counts <- rbind(theme.counts, df)
  }
  theme.counts <- theme.counts[,c("DATE", "Number", "Country")]
  # just the cols we need, condense by day
  theme.counts <- as.data.frame(summarise(group_by(tbl_df(theme.counts), DATE, Country), Number=sum(Number)))
  theme.counts$DATE <- gDate(theme.counts$DATE)
  maxheight <- max(theme.counts$Number) + 10
  
  if(returndata==TRUE){
    return(theme.counts)
    stop()
  }
  
  if(overlay==TRUE){
    # all on the same graph
    return(ggplot(data=theme.counts, aes(x=DATE, y=Number, color=Country)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.3, se=FALSE, size=1) + ylim(0, maxheight) + ylab("Count")  + theme_bw())
  }
  
  if(overlay==FALSE){
    # on different graphs
    return(ggplot(data=theme.counts, aes(x=DATE, y=Number, Country)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.3, se=FALSE, size=1) + facet_wrap(~ type, ncol=1) + theme_bw())
  }
}

eur <- c("greece", "spain", "france", "italy")
countrycode(eur, "country.name", "fips104")

themeLocator(protest, c("greece", "spain", "france", "italy"))

protest <- read.csv("allProtest.csv", stringsAsFactors=FALSE)
greece <- themeTrend(protest, c("LGBT", "IMMIGRATION", "ECON", "UNEMPLOYMENT", "MUSLIM"), location="GR", returndata=TRUE)
rm(protest)
gc()
mobilization <- read.csv("~/GKG Data/Months/alltheMobilization.csv", stringsAsFactors=FALSE)

ggplot(greece, aes(x=DATE, y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.1, se=FALSE, size=1) + ylab("Count") + theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0,1)) + ggtitle("Greece: Selected Themes By Protest Mentions")
ggplot(greece2, aes(x=DATE, y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.1, se=FALSE, size=1) + ylab("Count") + theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0,1)) + ggtitle("Greece: Selected Themes by Mobilization")

greece <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location="GR", returndata=TRUE)
spain <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location="SP", returndata=TRUE)
france <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location="FR", returndata=TRUE)
italy <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location="IT", returndata=TRUE)
uk <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("united kingdom"), returndata=TRUE)
sweden <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("sweden"), returndata=TRUE)
germany <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("germany"), returndata=TRUE)
denmark <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("denmark"), returndata=TRUE)
austria <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("austria"), returndata=TRUE)
belgium <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("belgium"), returndata=TRUE)
bulgaria <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("bulgaria"), returndata=TRUE)
croatia <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("croatia"), returndata=TRUE)
cyprus <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("cyprus"), returndata=TRUE)
czech <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("czech republic"), returndata=TRUE)
estonia <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("estonia"), returndata=TRUE)
finland <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("finland"), returndata=TRUE)
hungary <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("hungary"), returndata=TRUE)
ireland <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("ireland"), returndata=TRUE)
latvia <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("latvia"), returndata=TRUE)
lithuania <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("lithuania"), returndata=TRUE)
luxembourg <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("luxembourg"), returndata=TRUE)
malta <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("malta"), returndata=TRUE)
netherlands <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("netherlands"), returndata=TRUE)
poland <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("poland"), returndata=TRUE)
portugal <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("portugal"), returndata=TRUE)
romania <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("romania"), returndata=TRUE)
slovakia <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("slovakia"), returndata=TRUE)
slovenia <- themeTrend(mobilization, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), location=fipsify("slovenia"), returndata=TRUE)


greece$Country <- "Greece"
spain$Country <- "Spain"
france$Country <- "France"
italy$Country <- "Italy"
uk$Country <- "UK"
austria$Country <- "Austria"
belgium$Country <- "Belgium"
bulgaria$Country <- "Bulgaria"
croatia$Country <- "Croatia"
cyprus$Country <- "Cyprus"
czech$Country <- "Czech"
denmark$Country <- "Denmark"
estonia$Country <- "Estonia"
finland$Country <- "Finland"
germany$Country <- "Germany"
hungary$Country <- "Hungary"
ireland$Country <- "Ireland"
latvia$Country <- "Latvia"
lithuania$Country <- "Lithuania"
luxembourg$Country <- "Luxembourg"
malta$Country <- "Malta"
netherlands$Country <- "Netherlands"
poland$Country <- "Poland"
portugal$Country <- "Portugal"
romania$Country <- "Romania"
slovakia$Country <- "Slovakia"
slovenia$Country <- "Slovenia"
sweden$Country <- "Sweden"

#("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT")
#ggplot(eurostrikethemes, aes(y=Percent, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + xlab(NULL) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(text=element_text(family="Gill Sans MT", size=14)) 

eurprotests <- rbind(austria,belgium,bulgaria,croatia,cyprus,czech,denmark,estonia,finland,france,germany,greece,hungary,ireland,italy,latvia,lithuania,luxembourg,malta,netherlands,poland,portugal,romania,slovakia,slovenia,spain,sweden,uk)
write.csv(eurprotests, "~/Dropbox/ISA Paper/EU protest data GKG mobilization.csv")
# read in this file for Econ, German, Immigration, Movement_Womens, Violent_Unrest
#europrotests <- read.csv("~/Dropbox/ISA Paper/EU protest data GKG.csv", stringsAsFactor=FALSE)
europrotests <- read.csv("~/Dropbox/ISA Paper/EU protest data GKG mobilization.csv", stringsAsFactor=FALSE)
europrotests$DATE <- as.Date(europrotests$DATE)
europrotests$type <- as.factor(europrotests$type)
#europrotests$type <- factor(europrotests$type, levels(europrotests$type)[c(2, 3, 1, 4, 5)])

ggplot(eurprotests, aes(x=DATE, y=Number, color=type, Country)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.3, se=FALSE, size=1) + facet_wrap( ~ Country, nrow=15) + ylab("Count") + theme_bw()

colorvalues <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3")

italy <- subset(europrotests, Country=="Italy")
ggplot(italy, aes(x=as.Date(DATE), y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.1, se=FALSE, size=1) + ylab("Count") + theme_bw() + ggtitle("Italy: Selected Themes Connected With Mobilization Events") + scale_color_manual(values = colorvalues) + theme(legend.justification=c(0,1), legend.position=c(0,1)) + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + xlab("Date") + theme(text=element_text(family="Gill Sans MT", size=10))
ggsave(filename="Italy-Themes.pdf", width=8, height=4)
embed_fonts("Italy-Themes.pdf")

greece <- subset(europrotests, Country=="Greece")
ggplot(greece, aes(x=as.Date(DATE), y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.1, se=FALSE, size=1) + ylab("Count") + theme_bw() + ggtitle("Greece: Selected Themes Connected With Mobilization Events") + scale_color_manual(values = colorvalues) + theme(legend.justification=c(0,1), legend.position=c(0,1)) + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + xlab("Date") + theme(text=element_text(family="Gill Sans MT", size=10))
ggsave(filename="Greece-Themes.pdf", width=8, height=4)
embed_fonts("Greece-Themes.pdf")

ireland <- subset(europrotests, Country=="Ireland")
ggplot(ireland, aes(x=as.Date(DATE), y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.1, se=FALSE, size=1) + ylab("Count") + theme_bw() + ggtitle("Ireland: Selected Themes Connected With Mobilization Events") + scale_color_manual(values = colorvalues) + theme(legend.justification=c(0,1), legend.position=c(0,1)) + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + xlab("Date") + theme(text=element_text(family="Gill Sans MT", size=10))
ggsave(filename="Ireland-Themes.pdf", width=8, height=4)
embed_fonts("Ireland-Themes.pdf")

france <- subset(europrotests, Country=="France")
ggplot(france, aes(x=DATE, y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.1, se=FALSE, size=1) + ylab("Count") + theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0,1)) + ggtitle("France: Selected Themes Connected With Mobilization Events") + scale_color_manual(values = colorvalues) + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + xlab("Date")+ theme(text=element_text(family="Gill Sans MT", size=10))
ggsave(filename="France-Themes.pdf", width=8, height=4)
embed_fonts("France-Themes.pdf")

uk <- subset(europrotests, Country=="UK")
ggplot(uk, aes(x=DATE, y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.1, se=FALSE, size=1) + ylab("Count") + theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0,1)) + ggtitle("UK: Selected Themes Connected With Mobilization Events") + scale_color_manual(values = colorvalues) + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + xlab("Date")+ theme(text=element_text(family="Gill Sans MT", size=10))
ggsave(filename="UK-Themes.pdf", width=8, height=4)
embed_fonts("UK-Themes.pdf")

portugal <- subset(europrotests, Country=="Portugal")
ggplot(portugal, aes(x=DATE, y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.1, se=FALSE, size=1) + ylab("Count") + theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0,1)) + ggtitle("Portugal: Selected Themes Connected With Mobilization Events") + scale_color_manual(values = colorvalues) + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + xlab("Date")+ theme(text=element_text(family="Gill Sans MT", size=10))
ggsave(filename="Portugal-Themes.pdf", width=8, height=4)
embed_fonts("Portugal-Themes.pdf")

spain <- subset(europrotests, Country=="Spain")
ggplot(spain, aes(x=DATE, y=Number, color=type)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.1, se=FALSE, size=1) + ylab("Count") + theme_bw() + theme(legend.justification=c(0,1), legend.position=c(0,1)) + ggtitle("Spain: Selected Themes Connected With Mobilization Events") + scale_color_manual(values = colorvalues)+ scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + xlab("Date") + theme(text=element_text(family="Gill Sans MT", size=10))
ggsave(filename="Spain-Themes.pdf", width=8, height=4)
embed_fonts("Spain-Themes.pdf")

greece
## Percentage for women's stuff.
wom <- as.data.frame(summarise(group_by(filter(tbl_df(eurprotests), type=="MOVEMENT_WOMENS"), Country), total=sum(Number)), n=-1)
tot <- as.data.frame(summarise(group_by(tbl_df(eurprotests), Country), total=sum(Number)), n=-1)
wom <- merge(wom, tot, by="Country")
wom$percent <- (wom$total.x / wom$total.y) * 100
names(wom) <- c("Country", "Women's Movement", "Total Protests", "Percent")
wom <- wom[order(-wom$Percent),]
wom.tbl <- xtable(wom[1:15,], row.names=FALSE)
print(wom.tbl, floating=FALSE, row.names=FALSE)


# How many about women's movement?
tmp <- subset(eurprotests, type=="MOVEMENT_WOMENS")
sum(tmp$Number)
summarise(group_by(tbl_df(tmp), Country), count=sum(Number))

ggplot(eurprotests, aes(x=DATE, y=Number, color=type, Country)) + geom_line(size=1, alpha=.3) + geom_smooth(method="loess", span=0.3, se=FALSE, size=1) + facet_wrap( ~ Country, nrow=2) + ylab("Count") + theme_bw()


######## Back to Aid data  #######
# "allciv.csv" is all civil society activity for all countries of interest in E. Europe 
# Here's what it looks like:
#   MonthYear  count	total	Country
#	  199201    	20  	273	  Albania
setwd("~/Dropbox/ISA Paper")
oti <- read.csv("OTI Funding.csv", stringsAsFactors=FALSE)
oti[is.na(oti)] <- 0
oti <- melt(oti)
kosovo.oti <- subset(oti, Country=="Kosovo")
names(kosovo.oti) <- c("Country","year","DG_Aid")
kosovo.oti$year <- gsub("FY", "", kosovo.oti$year)
kosovo.oti$DG_Aid <- kosovo.oti$DG_Aid / 1000

civ <- read.csv("allciv.csv", stringsAsFactors=FALSE)
#usaid <- read.csv("data_usaid_dg.csv", stringsAsFactors=FALSE) 
library(foreign)
usaid <- as.data.frame(read.spss("USAID_DG_Programs.sav"))
##   Codebook for USAID data is here: http://www2.pitt.edu/~politics/democracy/downloads/Codebook_Phase_2.pdf
civ$ISO <- countrycode(civ$Country, "country.name", "iso3c")
civ$year <- as.numeric(strtrim(civ$MonthYear, 4))
# AID100 is Total USAID investment for all Democracy and Governance programs, millions of constant 2000 US dollars.
# See http://www.pitt.edu/~politics/democracy/downloads/Codebook_Phase_2.pdf pg. 3
usaid <- usaid[,c("cname", "year", "AID100")]
usaid$ISO <- countrycode(usaid$cname, "country.name", "iso3c", warn=TRUE)
usaid[grep("Czechoslovakia", usaid$cname), "ISO"] <- "CSK"
usaid[grep("Socialist Rep", usaid$cname), "ISO"] <- "YUG"
usaid[grep("Czechoslovakia", usaid$cname), "ISO"] <- "CSK"
# How do we want to deal with 1992-2006 Serbia?

civ.aid <- merge(civ, usaid, by=c("ISO", "year"), all.x=TRUE)
civ.aid <- merge(civ.aid, kosovo.oti, by=c("Country", "year"), all.x=TRUE)
civ.aid[is.na(civ.aid$DG_Aid), "DG_Aid"] <- 0
civ.aid$AID100 <- civ.aid$AID100 + civ.aid$DG_Aid
civ.aid <- subset(civ.aid, year < 2005)
civ.aid <- civ.aid[,c("MonthYear", "count", "total", "Country", "AID100")]
# Count is civil society events, total is all events
names(civ.aid) <- c("MonthYear", "Count", "Total", "Country", "DG_Aid")
civ.aid$Count <- as.numeric(civ.aid$Count)
civ.aid$Total <- as.numeric(civ.aid$Total)
civ.aid$Date <- gDate(paste0(civ.aid$MonthYear, "01"))
# It's worrisome that I have to tell it NA=0 twice...
civ.aid[is.na(civ.aid$DG_Aid), "DG_Aid"] <- 0


## We'll build our own stupid cut points.
civ.aid$Amount <- 0
civ.aid[civ.aid$DG_Aid==0, "Amount"] <- "None"
civ.aid[civ.aid$DG_Aid > 0 & civ.aid$DG_Aid <= 10, "Amount"] <- "Up to 10"
civ.aid[civ.aid$DG_Aid > 10 & civ.aid$DG_Aid <= 50, "Amount"] <- "10 to 50"
civ.aid[civ.aid$DG_Aid > 50, "Amount"] <- "50+"
civ.aid$Amount <- as.factor(civ.aid$Amount) 
# reorder the factor for plotting:
civ.aid$Amount <- factor(civ.aid$Amount, levels(civ.aid$Amount)[c(3,4,1,2)])
colorvalues <- c("#cccccc","#a1dab4","#41b6c4","#225ea8")

ggplot(civ.aid, aes(x=Date, y=Count, color=Amount, group=1)) + geom_line(size=1.5) + facet_wrap( ~ Country, nrow=5) + theme_bw() + scale_color_manual(values = colorvalues, name="USAID Democracy and\nGovernance Funding\n($m/year)") + theme(strip.background = element_rect(fill = 'white'), legend.position="top") + ylab("Civil Society Events Per Month") + xlab(NULL) + theme(text=element_text(family="Gill Sans MT", size=10))
ggsave("Civsoc-and-funding.pdf", width=8, height=7)
embed_fonts("Civsoc-and-funding.pdf")

#  text=element_text("Gill Sans", size=16)  ## But need Ghostscript to embed fonts in PDF
#ggplot(civ.aid, aes(x=Date, y=Count, alpha=log(DG_Aid+1))) + geom_line(size=1.5) + facet_wrap( ~ Country, nrow=8) + theme_bw()


setwd("~/GKG Data/Months")

GenderSubset <- function(filename){
  tmp <- read.csv(filename, stringsAsFactors=FALSE, sep="\t", header=TRUE)
  ukr <- tmp[grep("GENDER_VIOLENCE", tmp$LOCATIONS),]
  new.file <- paste0("Gender.", filename)
  write.csv(ukr, file=new.file)
  gc()
}

GenderSubset("GKG.April.csv")
GenderSubset("GKG.May.csv")
GenderSubset("GKG.June.csv")
GenderSubset("GKG.July.csv")
GenderSubset("GKG.August.csv")
GenderSubset("GKG.September.csv")
GenderSubset("GKG.October.csv")
GenderSubset("GKG.November.csv")
GenderSubset("GKG.December.csv")
system("cat Gender.* > allGender.csv")

system("cat Gender.* > allGender2.csv")
gender <- read.csv("allGender2.csv", stringsAsFactors=FALSE)

LocationCounter <- function(df){
  require(countrycode)
 locations <- df$LOCATIONS
 locations <- as.character(unlist(strsplit(locations, ";")))
 locations <- strsplit(as.character(locations), "#")
 locations <- sapply(locations, "[", 3)
 locations <- as.data.frame(table(locations))
 locations$locations <- countrycode(locations$locations, "fips104", "country.name")
 locations <- locations[order(-locations$Freq),]
 return(locations)
}

output <- LocationCounter(women)
output <- output[complete.cases(output),]
output <- xtable(output[1:30,], row.names=FALSE)
print(output, floating=TRUE, row.names=FALSE)

gender <- read.csv("allGender.csv", stringsAsFactors=FALSE)
output <- LocationCounter(gender)
output$Region <- countrycode(output$locations, "country.name", "region")
output <- output[grep("Europe", output$Region),]
output <- output[complete.cases(output),]
output <- xtable(output)
print(output, floating=TRUE, include.rownames=FALSE)
ggplot(output, aes(Freq)) + theme_bw() + geom_density()
  #geom_bar(binwidth=5000)


##########   GKG Normalizer   #############

setwd("~/GKG Data/Months")

BaselineSubset <- function(filename){
  require(countrycode)
  df <- read.csv(filename, stringsAsFactors=FALSE, sep="\t", header=TRUE)
  locations <- df$LOCATIONS
  locations <- as.character(unlist(strsplit(locations, ";")))
  locations <- strsplit(as.character(locations), "#")
  locations <- sapply(locations, "[", 3)
  locations <- as.data.frame(table(locations))
  locations$locations <- countrycode(locations$locations, "fips104", "country.name")
  locations <- locations[order(-locations$Freq),]
  new.file <- paste0("Gender.", filename)
  write.csv(locations, file=new.file)
  gc()
}

BaselineSubset("GKG.April.csv")
BaselineSubset("GKG.May.csv")
BaselineSubset("GKG.June.csv")
BaselineSubset("GKG.July.csv")
BaselineSubset("GKG.August.csv")
BaselineSubset("GKG.September.csv")
BaselineSubset("GKG.October.csv")
BaselineSubset("GKG.November.csv")

system("cat Baseline.* > allBaseline.csv")

baseline <- read.csv("allBaseline.csv", stringsAsFactors=FALSE)


setwd("~/GKG Data/Months")
ViolenceSubset <- function(filename){
  tmp <- read.csv(filename, stringsAsFactors=FALSE, sep="\t", header=TRUE)
  ukr <- tmp[grep("VIOLENT_UNREST", tmp$THEMES),]
  new.file <- paste0("Violence.", filename)
  write.csv(ukr, file=new.file)
}
ViolenceSubset("GKG.April.csv")
ViolenceSubset("GKG.May.csv")
ViolenceSubset("GKG.June.csv")
ViolenceSubset("GKG.July.csv")
ViolenceSubset("GKG.August.csv")
ViolenceSubset("GKG.September.csv")
ViolenceSubset("GKG.October.csv")
ViolenceSubset("GKG.November.csv")
system("cat Violence.* > alltheViolence.csv")

# What issues are associated with violent unrest in Europe?
violence <- read.csv("alltheViolence.csv", stringsAsFactors=FALSE)
# Whole world, what themes are tightly connected?
eurnames <- c("austria","belgium","bulgaria","croatia","cyprus","czech republic","denmark","estonia","finland","france","germany","greece","hungary","ireland","italy","latvia","lithuania","luxembourg","malta","netherlands","poland","portugal","romania","slovakia","slovenia","spain","sweden","united kingdom")
eurfips <- countrycode(eurnames, "country.name", "fips104")
eurfips <- paste0("#", eurfips, "#")
system.time(euroviolence <- violence[sapply(eurfips, grepl, violence$LOCATIONS, ignore.case=FALSE),])

ThemeCounter <- function(df){
  require(countrycode)
  themes <- as.character(df$THEMES)
  themes <- as.character(unlist(strsplit(themes, ";")))
  themes <- as.data.frame(table(themes))
  themes <- themes[order(-themes$Freq),]
  return(themes)
}
View(ThemeCounter(europrotests))

### How many of the protests in Europe are related to economic issues?
protests <- read.csv("allProtest.csv", stringsAsFactors=FALSE)
system.time(europrotests <- protests[sapply(eurfips, grepl, protests$LOCATIONS, ignore.case=FALSE),])

tmp <- ThemeCounter(europrotests)

tmp <- LocationCounter(europrotests[grep("GERMAN", europrotests$THEMES),])
sum(tmp$Freq) / nrow(europrotests)



eurprotests <- read.csv("~/Dropbox/ISA Paper/EU protest data GKG.csv", stringsAsFactors=FALSE)
importance <- as.data.frame(summarise(group_by(tbl_df(eurprotests), Country, type), Number=sum(Number)))
importance$type <- as.factor(importance$type)

colorvalues <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3")
ggplot(importance, aes(y=Number, Country, x=type, fill=type)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'))


CivQuadder <- function(country, startdate, enddate){
  df <- GovToCiv(country=country, startdate=startdate, enddate=enddate, output="month.count")
  df$Date <- gDate(paste0(df$MonthYear, "01"))
  df$QuadClass <- as.factor(df$QuadClass)
  levels(df$QuadClass)[levels(df$QuadClass)=="1"] <- "Verbal Coop."
  levels(df$QuadClass)[levels(df$QuadClass)=="2"] <- "Material Coop."
  levels(df$QuadClass)[levels(df$QuadClass)=="3"] <- "Verbal Confl."
  levels(df$QuadClass)[levels(df$QuadClass)=="4"] <- "Material Confl."
  title <- paste0(toupper(country), ": Government to CVL/OPP by Quad Class")
  ggplot(df, aes(x=Date, y=count, color=QuadClass)) + geom_line(size=1.5) + theme_bw() + ggtitle(title) + scale_colour_manual(values=c("#91bfdb", "#7fbf7b", "#fc8d59","#d73027")) 
}

##########   Gov To Civ  #########
GovToCiv <- function(country, startdate, enddate, output){
  ## put the dates in yyyymmdd format. (numeric)
  ## "output" will take c("raw", "month.percent", "month.count", "tone")
  ## puts out quadclass counts
  if(enddate > 20130331){
    stop("This range is not inside the historical GDELT table")
  }
  
  fips <- countrycode(country, "country.name", "fips104")
  iso <- countrycode(country, "country.name", "iso3c")
  civsoc <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""), (Actor1Type1Code=="GOV" | Actor1Type1Code=="COP" | Actor1Type1Code=="MIL"), (Actor2Type1Code=="CVL" | Actor2Type1Code=="OPP" | Actor2Type1Code=="MED" | Actor2Type1Code=="LAB" | Actor2Type1Code=="NGO"))
  
  if(output=="raw"){
    return(civsoc)
    ## pipe this into a new object, then you can filter that object before as.data.framing it.
  }
  
  if(output=="month.count"){
    civsoc <- select(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, QuadClass)
    civsoc <- summarise(civsoc, count=n())
    civsoc <- as.data.frame(civsoc)
    return(civsoc)
  }
  if(output=="month.percent"){
    civsoc <- select(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, QuadClass)
    civsoc <- summarise(civsoc, count=n())
    count <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""))
    count <- group_by(count, MonthYear)
    count <- summarise(count, total=n())
    count <- as.data.frame(count)
    civsoc <- merge(civsoc, count, by="MonthYear")
  }
}

##########  Civ To Gov  ###########
CivToGov <- function(country, startdate, enddate, output){
  ## put the dates in yyyymmdd format. (numeric)
  ## "output" will take c("raw", "month.percent", "month.count", "tone")
  ## puts out quadclass counts
  if(enddate > 20130331){
    stop("This range is not inside the historical GDELT table")
  }
  
  fips <- countrycode(country, "country.name", "fips104")
  iso <- countrycode(country, "country.name", "iso3c")
  civsoc <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""), (Actor1Type1Code=="CVL" | Actor1Type1Code=="OPP" | Actor1Type1Code=="MED" | Actor1Type1Code=="LAB" | Actor1Type1Code=="NGO" | Actor1Type1Code=="HRI"), (Actor2Type1Code=="GOV" | Actor2Type1Code=="MIL" | Actor2Type1Code=="COP"))
  
  if(output=="raw"){
    return(civsoc)
    ## pipe this into a new object, then you can filter that object before as.data.framing it.
  }
  
  if(output=="month.count"){
    civsoc <- select(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, QuadClass)
    civsoc <- summarise(civsoc, count=n())
    civsoc <- as.data.frame(civsoc)
    return(civsoc)
  }
  if(output=="month.percent"){
    civsoc <- select(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, MonthYear, QuadClass)
    civsoc <- group_by(civsoc, QuadClass)
    civsoc <- summarise(civsoc, count=n())
    count <- filter(hist.db, SQLDATE >= startdate, SQLDATE <= enddate, ActionGeo_CountryCode==fips, (Actor1CountryCode==iso | Actor1CountryCode==""), (Actor2CountryCode==iso | Actor2CountryCode==""))
    count <- group_by(count, MonthYear)
    count <- summarise(count, total=n())
    count <- as.data.frame(count)
    civsoc <- merge(civsoc, count, by="MonthYear")
  }
}


georgia.gov <- GovToCiv("georgia", 19971231, 20060101, output="month.count")
georgia.gov$type <- "Source: Government, Target: Civil Soc/Opposition"
georgia.civ <- CivToGov("georgia", 19971231, 20060101, output="month.count")
georgia.civ$type <- "Source: Civil Soc/Opposition  Target: Government"
georgia <- rbind(georgia.gov, georgia.civ)
georgia$Date <- gDate(paste0(georgia$MonthYear, "01"))
georgia$QuadClass <- as.factor(georgia$QuadClass)
georgia$type <- as.factor(georgia$type)
levels(georgia$QuadClass)[levels(georgia$QuadClass)=="1"] <- "Verbal Coop."
levels(georgia$QuadClass)[levels(georgia$QuadClass)=="2"] <- "Material Coop."
levels(georgia$QuadClass)[levels(georgia$QuadClass)=="3"] <- "Verbal Confl."
levels(georgia$QuadClass)[levels(georgia$QuadClass)=="4"] <- "Material Confl."
ggplot(georgia, aes(x=Date, y=count, color=QuadClass)) + geom_line(size=1.5) + theme_bw() + facet_wrap( ~ type, nrow=2) + ggtitle("Georgia") + theme(strip.background = element_rect(fill = 'white'), legend.position="top") + theme(text=element_text(family="Gill Sans MT", size=10))
setwd("~/Dropbox/ISA Paper/")
ggsave(filename="Mobilization-per-capita.pdf", width=8, height=8)
embed_fonts("Mobilization-per-capita.pdf")



CivQuadder <- function(country, startdate, enddate){
  df.gov <- GovToCiv(country=country, startdate=startdate, enddate=enddate, output="month.count")
  df.gov$type <- "Source: Government,  Target: Civil Society and Opposition"
  df.civ <- CivToGov(country=country, startdate=startdate, enddate=enddate, output="month.count")
  df.civ$type <- "Source: Civil Society and Opposition,  Target: Government"
  df <- rbind(df.gov, df.civ)
  df$Date <- gDate(paste0(df$MonthYear, "01"))
  df$QuadClass <- as.factor(df$QuadClass)
  levels(df$QuadClass)[levels(df$QuadClass)=="1"] <- "Verbal Coop."
  levels(df$QuadClass)[levels(df$QuadClass)=="2"] <- "Material Coop."
  levels(df$QuadClass)[levels(df$QuadClass)=="3"] <- "Verbal Confl."
  levels(df$QuadClass)[levels(df$QuadClass)=="4"] <- "Material Confl."
  title <- paste0(country, ": Government and Civil/Political Society Relations")
  ggplot(df, aes(x=Date, y=count, color=QuadClass)) + geom_line(size=1.5) + theme_bw() + facet_wrap( ~ type, nrow=2) + ggtitle(title)  + scale_colour_manual(values=c("#91bfdb", "#7fbf7b", "#fc8d59","#d73027")) + theme(strip.background=element_rect(fill="white"), legend.position="top", legend.margin = unit(-0.6,"cm")) + theme(text=element_text(family="Gill Sans MT", size=14)) + scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y")) + xlab(NULL)
}

setwd("~/Dropbox/ISA Paper")
serb.civquad <- CivQuadder("Serbia", 19970101, 20030101)
ggsave(file="Serbia-DoubleQuad.pdf", width=7, height=4)
embed_fonts("Serbia-DoubleQuad.pdf")

croat.civquad <- CivQuadder("Croatia", 19970101, 20030101)
croat.civquad + scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))
ggsave(file="Croatia-DoubleQuad.pdf", width=7, height=4)
embed_fonts("Croatia-DoubleQuad.pdf")

ukr.civquad <- CivQuadder("Ukraine", 20000101, 20060101)
ggsave(file="Ukraine-DoubleQuad.pdf", width=7, height=4)
embed_fonts("Ukraine-DoubleQuad.pdf")

geor.civquad <- CivQuadder("Georgia", 20000101, 20060101)
ggsave(file="Georgia-DoubleQuad.pdf", width=7, height=4)
embed_fonts("Georgia-DoubleQuad.pdf")

azer.civquad <- CivQuadder("Azerbaijan", 20010101, 20090101)
ggsave(file="Azerbaijan-DoubleQuad.pdf", width=7, height=4)
embed_fonts("Azerbaijan-DoubleQuad.pdf")

rom.civquad <- CivQuadder("Romania", 19930101, 20000101)
ggsave(file="Romania-DoubleQuad.pdf", width=7, height=4)
embed_fonts("Romania-DoubleQuad.pdf")

bul.civquad <- CivQuadder("Bulgaria", 19940101, 20040101)
ggsave(file="Bulgaria-DoubleQuad.pdf", width=7, height=4)
embed_fonts("Bulgaria-DoubleQuad.pdf")

kyr.civquad <- CivQuadder("Kyrgyzstan", 20010101, 20090101)
ggsave(file="Kyrgyzstan-DoubleQuad.pdf", width=7, height=4)
embed_fonts("Kyrgyzstan-DoubleQuad.pdf")

#Romania, Slovakia, Bulgaria (Bulgaria’s years shown in the paper are too late), or Kyrgyzstan.   I’m not sure we need failed breakthroughs, but if we want to add them: Azerbaizan (2005),
setwd("~/Dropbox/ISA Paper")
bulg.civquad <- CivQuadder("Bulgaria", 19940101, 20040101)
ggsave(file="Bulgaria-DoubleQuad.pdf", plot=bulg.civquad, width=11, height=6.5)
rom.civquad <- CivQuadder("Romania", 19930101, 20000101)
ggsave(file="Romania-DoubleQuad.pdf", plot=rom.civquad, width=11, height=6.5)
kyrg.civquad <- CivQuadder("Kyrgyzstan", 20010101, 20090101)
ggsave(file="Kyrgyzstan-DoubleQuad.pdf", plot=kyrg.civquad, width=11, height=6.5)
azer.civquad <- CivQuadder("Azerbaijan", 20010101, 20090101)
ggsave(file="Azerbaijan-DoubleQuad.pdf", plot=azer.civquad, width=11, height=6.5)

greece <- read.csv("~/GKG Data/Months/allGreece.csv", stringsAsFactors=FALSE)
merk.gr <- toneTrend(greece, objects="merkel", type="PERSONS", tld="gr", returndata=TRUE) 
merk.gr$Country <- "Greece"
merk.sp <- toneTrend(protests, objects="merkel", type="PERSONS", tld="es", returndata=TRUE) 
merk.sp$Country <- "Spain"
merk.uk <- toneTrend(protests, objects="merkel", type="PERSONS", tld="uk", returndata=TRUE) 
merk.uk$Country <- "UK"
merk.be <- toneTrend(protests, objects="merkel", type="PERSONS", tld="be", returndata=TRUE) 
merk.be$Country <- "Belgium"
merk <- rbind(merk.gr, merk.sp, merk.uk)
merk$Country <- as.factor(merk$Country)
ggplot(data=merk.gr, aes(x=DATE, y=Tone, color=Country)) + geom_point(size=2, alpha=0.7) + geom_smooth(method="loess", span=0.3, se=FALSE, size=1) + ylab("Tone") + theme(legend.position="top") + scale_x_date(breaks = date_breaks("months"), labels = date_format("%b")) + xlab("Date") + theme_bw() 

#####  Location counts per theme function  ######

# basically theme trend minus time and looping through countries.
## What we want: df with countries as rows, themes as columns, and counts as cells. How many times were locations in each country mentioned in conjunction with each theme in the protests df?

# grep to create a df for each sub-theme, split location, collapse to table.
# make it for all countries, then just pull out the EU countries.

LocationThemes <- function(df, themes, countries){
  # "df" will be the overarching big theme, like protests, for the whole world
  require(countrycode)
  theme.counts <- data.frame()
  for(i in 1:length(themes)){
    type.i <- themes[i]
    locations <- df[grep(type.i, df$THEMES), "LOCATIONS"]
    locations <- as.character(unlist(strsplit(locations, ";")))
    locations <- strsplit(as.character(locations), "#")
    locations <- sapply(locations, "[", 3)
    locations <- as.data.frame(table(locations))
    #tmp$Number <- sapply(tmp$LOCATIONS, function(x) length(grep(country.i, unlist(strsplit(x, ";")))))
    locations$type <- tolower(type.i)
    theme.counts <- rbind(theme.counts, locations)
  }
  names(theme.counts) <- c("Country", "Count", "Theme") 
  countrylist <- countrycode(countries, "country.name", "fips104")
  theme.counts <- theme.counts[theme.counts$Country %in% countrylist,]
  #print("Second loop done")
  #print(head(theme.counts))
  theme.counts$Country <- countrycode(theme.counts$Country, "fips104", "country.name")
  countrytotals <- as.character(unlist(strsplit(df$LOCATIONS, ";")))
  countrytotals <- strsplit(as.character(countrytotals), "#")
  countrytotals <- sapply(countrytotals, "[", 3)
  countrytotals <- as.data.frame(table(countrytotals))
  names(countrytotals) <- c("Country", "Total")
  countrytotals$Country <- countrycode(countrytotals$Country, "fips104", "country.name")
  theme.counts <- merge(theme.counts, countrytotals, by="Country")
  theme.counts$Percent <- theme.counts$Count / theme.counts$Total
  #print(str(theme.counts))
  return(theme.counts)
  }

mobilization <- read.csv("~/GKG Data/Months/alltheMobilization.csv", stringsAsFactors=FALSE)
protests <- read.csv("~/GKG Data/Months/alltheProtests.csv", stringsAsFactors=FALSE)

setwd("~/Dropbox/ISA Paper")

europrotestthemes <- LocationThemes(protests, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), countries=eurnames)
colorvalues <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3")
eurocounts <- ggplot(europrotestthemes, aes(y=Count, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(text=element_text(family="Gill Sans MT", size=14)) 
ggsave("Protest-Themes-Country.pdf", plot=eurocounts, width=8, height=7)

eurothemegrid <- ggplot(europrotestthemes, aes(y=Percent, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + xlab(NULL) + theme(text=element_text(family="Gill Sans MT", size=14)) 
ggsave("Protest-Theme-Percent2.pdf", plot=eurothemegrid, width=8, height=8)
embed_fonts("Protest-Theme-Percent2.pdf")

strikes <- read.csv("~/GKG Data/Months/alltheStrikes.csv", stringsAsFactors=FALSE)
eurostrikethemes <- LocationThemes(strikes, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), countries=eurnames)
colorvalues <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3")
ggplot(eurostrikethemes, aes(y=Percent, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + xlab(NULL) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(text=element_text(family="Gill Sans MT", size=14)) 
ggsave(filename="Strike-Theme-Percent.pdf", width=8, height=8)
embed_fonts("Strike-Theme-Percent.pdf")

movement <- read.csv("alltheMovement.csv", stringsAsFactors=FALSE)
euromovement <- LocationThemes(movement, c("IMMIGRATION", "LGBT", "ECON", "MUSLIM", "UNEMPLOYMENT"), countries=eurnames)
colorvalues <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3")
ggplot(euromovement, aes(y=Percent, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + xlab(NULL) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(text=element_text(family="Gill Sans MT", size=14))
ggsave(filename="Movement-Theme-Percent.pdf", width=8, height=8)
embed_fonts("Movement-Theme-Percent.pdf")



#system.time(europrotestthemes <- LocationThemes(protests, c("VIOLENT_UNREST"), countries=eurnames))

#####  Are we getting the right Actor1Type1Codes?  ######
system.time(actor <- as.data.frame(summarise(group_by(select(daily.db, Actor1Type1Code), Actor1Type1Code), count=n())))
## See http://gdelt.utdallas.edu/data/lookups/CAMEO.type.txt 
## We should probably add in LAB (labor) 14,000 in 2000, (compared to 71,000 for CVL) could add in JUD (judiciary) (61,000), LEG (legislature) (86,000), NGO (11,600), 

cameotypes <- read.delim("CAMEO.type.txt")
act.croat <- as.data.frame(summarise(group_by(select(filter(hist.db, ActionGeo_CountryCode=="HR", (Actor1CountryCode=="HRV" | Actor1CountryCode==""), (Actor2CountryCode=="HRV" | Actor2CountryCode==""), SQLDATE > 19931231, SQLDATE < 20040101), Actor1Type1Code), Actor1Type1Code), count=n()))
act.croat <- act.croat[order(-act.croat$count),]
act.croat <- merge(act.croat, cameotypes, by.x="Actor1Type1Code", by.y="CODE")
act.croat <- xtable(act.croat)
print(act.croat, floating=TRUE, include.rownames=FALSE)

actor <- actor[order(-actor$count),]
actor <- xtable(actor)
print(actor, floating=TRUE, include.rownames=FALSE)


################   Subnational Stuff  ###############
system.time(croatia.locs <- as.data.frame(filter(hist.db, SQLDATE >= 19980101, SQLDATE <= 20030101, ActionGeo_CountryCode=="HR", (Actor1CountryCode=="HRV" | Actor1CountryCode==""), (Actor2CountryCode=="HRV" | Actor2CountryCode==""), (Actor1Type1Code=="NGO" |Actor1Type1Code=="CVL" | Actor1Type1Code=="OPP" | Actor1Type1Code=="LAB" | Actor1Type1Code=="MED" | EventRootCode=="14" | EventCode=="0243" | EventCode=="0244" | EventCode=="104" | EventCode=="1041" | EventCode=="1042" | EventCode=="1043" | EventCode=="1044" | EventCode=="105" | EventCode=="1051" | EventCode=="113" | EventCode=="024" | EventCode=="0241" | EventCode=="0242" | EventCode=="0243" | EventCode=="0244" | EventCode=="0251" | EventCode=="0253" | EventCode=="1121" | EventCode=="1122" | EventCode=="1242" | EventCode=="133")), n=-1))


serbia.locs <- as.data.frame(summarise(group_by(select(croatia.locs, ActionGeo_FullName, ActionGeo_Lat, ActionGeo_Long), ActionGeo_Lat, ActionGeo_Long), count=n()), n=-1)

setwd("~/GKG Data/Months")
MovementSubset <- function(filename){
  tmp <- read.csv(filename, stringsAsFactors=FALSE, sep="\t", header=TRUE)
  ukr <- tmp[grep("MOVEMENT", tmp$THEMES),]
  new.file <- paste0("Movement.", filename)
  write.csv(ukr, file=new.file)
}
MovementSubset("GKG.April.csv")
MovementSubset("GKG.May.csv")
MovementSubset("GKG.June.csv")
MovementSubset("GKG.July.csv")
MovementSubset("GKG.August.csv")
MovementSubset("GKG.September.csv")
MovementSubset("GKG.October.csv")
MovementSubset("GKG.November.csv")
MovementSubset("GKG.December.csv")
system("cat Movement* > alltheMovement.csv")

## New chart: violent unrest, protest, strike, movement counts.
setwd("~/GKG Data/Months")
MobilizationSubset <- function(filename){
  tmp <- read.csv(filename, stringsAsFactors=FALSE, sep="\t", header=TRUE)
  ukr <- tmp[grep("MOVEMENT|PROTEST|VIOLENT_UNREST|STRIKE", tmp$THEMES),]
  new.file <- paste0("Mobilization.", filename)
  write.csv(ukr, file=new.file)
}
MobilizationSubset("GKG.April.csv")
MobilizationSubset("GKG.May.csv")
MobilizationSubset("GKG.June.csv")
MobilizationSubset("GKG.July.csv")
MobilizationSubset("GKG.August.csv")
MobilizationSubset("GKG.September.csv")
MobilizationSubset("GKG.October.csv")
MobilizationSubset("GKG.November.csv")
MobilizationSubset("GKG.December.csv")
system("cat Mobilization* > alltheMobilization.csv")

mobilization <- read.csv("alltheMobilization.csv", stringsAsFactors=FALSE)
mobilization <- mobilization[,c("THEMES", "LOCATIONS")]

system.time(mobilization.grid <- LocationThemes(mobilization, c("PROTEST", "MOVEMENT", "STRIKE", "VIOLENT_UNREST"), countries=eurnames))
#colorvalues <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3")
colorvalues <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3")
mobilization.grid.nouk <- subset(mobilization.grid, Country != "United Kingdom" & Country != "Germany" & Country != "France" & Country != "Italy", Country != "Malta" & Country != "Luxembourg")
)
mobilization.grid.bigs <- subset(mobilization.grid, Country == "United Kingdom" | Country == "Germany" | Country == "France" | Country == "Italy")
mobilizationcounts.littles <- ggplot(mobilization.grid.nouk, aes(y=Count, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=6) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + xlab(NULL) + theme(text=element_text(family="Gill Sans MT", size=10))
mobilizationcounts.bigs <- ggplot(mobilization.grid.bigs, aes(y=Count, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=1) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + xlab(NULL) + theme(text=element_text(family="Gill Sans MT", size=10))
ggsave("Mobilization-Counts-littles.pdf", plot=mobilizationcounts.littles, width=8, height=7)
ggsave("Mobilization-Counts-bigs.pdf", plot=mobilizationcounts.bigs, width=8, height=2)
embed_fonts("Mobilization-Counts-littles.pdf")
embed_fonts("Mobilization-Counts-bigs.pdf")

# Now with per-capita counts
setwd("~/GKG Data/Months")
mobilization <- read.csv("alltheMobilization.csv", stringsAsFactors=FALSE)
mobilization <- mobilization[,c("THEMES", "LOCATIONS")]
mob.grid.data <- LocationThemes(mobilization, c("LGBT", "MOVEMENT_WOMENS"), countries=eurnames)
mob.grid.data$ISO <- countrycode(mob.grid.data$Country, "country.name", "iso3c")

population <- read.csv("~/Dropbox/ISA Paper/World Bank Population Data.csv", stringsAsFactors=FALSE, header=TRUE)
population <- population[,c(1, 2, 5:57)]
population <- melt(population, id.vars=c("Country.Name", "Country.Code"))
names(population) <- c("Country", "Country.Code", "Year", "Population")
population$Year <- as.numeric(gsub("X", "", as.character(population$Year)))
population$ISO <- countrycode(population$Country, "country.name", "iso3c")
population <- subset(population, Year==2012) # most recent population

setwd("~/Dropbox/ISA Paper")
mob.pop <- merge(mob.grid.data, population, by=c("ISO"), all.x=TRUE)
mob.pop$percap <- with(mob.pop, Count / Population) * 1000
mob.pop$Country.x <- as.factor(mob.pop$Country.x)
mob.pop <- subset(mob.pop, Country.x != "Malta" & Country.x != "Luxembourg")
ggplot(mob.pop, aes(y=percap, Country.x, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country.x, nrow=5) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + xlab(NULL) + ylab("Events Per Thousand People") + theme(text=element_text(family="Gill Sans MT", size=10))
setwd("~/Dropbox/ISA Paper/")
ggsave(filename="LGBT-womens-per-capita.pdf", width=8, height=8)
embed_fonts("LGBT-womens-per-capita.pdf")


women <- read.csv("~/GKG Data/Months/allWomen.csv", stringsAsFactors=FALSE)
setwd("~/Dropbox/ISA Paper")
womenlocthemes <- LocationThemes(women, c("LGBT", "GENDER_VIOLENCE", "ECON", "UNEMPLOYMENT"), countries=eurnames)
colorvalues <- c("#d7191c","#fdae61","#bebada","#abdda4","#2b83ba")
womencounts <- ggplot(womenlocthemes, aes(y=Count, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(text=element_text(family="Gill Sans MT", size=14)) 
ggsave("Women-Themes-Count.pdf", plot=womencounts, width=8, height=7)
embed_fonts("Women-Themes-Count.pdf")

womenpercent <- ggplot(womenlocthemes, aes(y=Percent, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(text=element_text(family="Gill Sans MT", size=14)) 
ggsave("Women-Themes-Percent.pdf", plot=womenpercent, width=8, height=7)
embed_fonts("Women-Themes-Percent.pdf")

womenloctactics <- LocationThemes(women, c("PROTEST", "STRIKE", "VIOLENT_UNREST"), countries=eurnames)
womentactics <- ggplot(womenloctactics, aes(y=Count, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(text=element_text(family="Gill Sans MT", size=14)) 
ggsave("Women-Tactics-Count-Country.pdf", plot=womentactics, width=8, height=7)
embed_fonts("Women-Tactics-Count-Country.pdf")

womentacticsperc <- ggplot(womenloctactics, aes(y=Percent, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(text=element_text(family="Gill Sans MT", size=14)) 
ggsave("Women-Tactics-Percent.pdf", plot=womentacticsperc, width=8, height=7)
embed_fonts("Women-Tactics-Percent.pdf")

mobilization <- read.csv("~/GKG Data/Months/alltheMobilization.csv", stringsAsFactors=FALSE)
mobilization <- mobilization[,c("THEMES", "LOCATIONS")]
mobcount <- LocationThemes(mobilization, c("ECON", "IMMIGRATION", "LGBT", "MUSLIM", "UNEMPLOYMENT"), countries=eurnames)
colorvalues <- c("#8dd3c7","#ffffb3","#bebada","#fb8072","#80b1d3")
ggplot(mobcount, aes(y=Count, Country, x=Theme, fill=Theme)) + geom_bar(stat="identity") + facet_wrap( ~ Country, nrow=8) + theme_bw() + scale_fill_manual(values = colorvalues) + xlab(NULL) + theme(strip.background = element_rect(fill = 'white'), legend.position="top", axis.ticks = element_blank(), axis.text.x = element_blank()) + theme(text=element_text(family="Gill Sans MT", size=14)) 
ggsave(filename="Mobilization-Themes-Country.pdf", width=8, height=8)
embed_fonts("Mobilization-Themes-Country.pdf")

setwd("~/GKG Data/Months")
mobilization <- read.csv("alltheMobilization.csv", stringsAsFactors=FALSE)
mobilization <- mobilization[,c("THEMES", "LOCATIONS")]
mob.grid.data <- LocationThemes(mobilization, c("LGBT", "MOVEMENT_WOMENS"), countries=eurnames)
mob.grid.data$ISO <- countrycode(mob.grid.data$Country, "country.name", "iso3c")
