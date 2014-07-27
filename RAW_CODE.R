## Synopsis

## Environment

### Prepare the Environment

library(knitr)
opts_chunk$set(echo = TRUE, results = 'hold')

library(data.table)
library(xtable)
library(ggplot2) # we shall use ggplot2 for plotting figures
library(gridExtra) # use of grid_arrange function instead par.

### Reference of sofware environment for reproductibility:
sessionInfo()

## Data Processing

### Data Loading
check_file_exist <- function(file_path) 
{
        if (!file.exists(file_path))
                stop("The ", file_path, " not found!") else TRUE 
}

load_data <- function(data_dir , fileURL, fileSource) 
{
        # Dataset check and load 
        
        file_path <- paste(data_dir, "/", fileSource , sep="")
        
        if (!file.exists(file_path)) {
                message(paste("Please Wait! Download...", fileURL, "..."));
                download.file(fileURL, destfile=source_path);
        } 

        data <- read.csv(bzfile(file_path),
                         header=TRUE,  na.strings=c("NA",""))
        data        
        
}

data_dir <- "C:/Users/Marcelo/NOAA_Analysis/Data";
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2" 
fileSource <-"repdata-data-StormData.csv.bz2"

raw_data <- load_data(data_dir , fileURL, fileSource) 

str(raw_data)

xt <- xtable(summary(raw_data))
print(xt, type = "html")

uniqevtype <- unique(raw_data$EVTYPE)
head(sort(uniqevtype), length(uniqevtype))

head(sort(uniqevtype), n = 25)

tidy <- raw_data

# rename the columns names to lowercase for ease of coding. 
colnames(tidy) <- tolower(names(tidy))

tidy$bgn_date <- as.Date(tidy$bgn_date, format="%m/%e/%Y %H:%M:%S")

tidy$evtype <- gsub("(\\sAND(\\s|$))|(\\s&\\s)", " ", tidy$evtype) 
tidy$evtype <- gsub("^\\s+|\\s+$", "", tidy$evtype) 
tidy$evtype <- gsub("\\s{2,}", " ", tidy$evtype) 
tidy$evtype <- tolower(tidy[, "evtype"])

tidy$evtype <- gsub("\\s{0,1}-\\s", "/", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("\\sand[[:alpha:]]{1,}", "/", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("\\sch([^[[:alpha:]]]{0,}|$)", " chill", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("\\schi([^[[:alpha:]]]{0,}|$)", " chill", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("\\schil($|\\s)", " chill", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("\\ssno($|\\s)", " snow", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("avalance", "avalanche", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("clou$", "cloud", tidy$evtype, ignore.case = TRUE) 
tidy$evtype <- gsub("drie", "dry", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("cstl", "coastal", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("erosin", "erosion", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("fld", "flood", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("floodin", "flood", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("flooding", "flood", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("freeze", "freezing", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("high wind (g40)", "high wind", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("ligntning", "lightning", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("micoburst", "microburst", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("non(-|\\s)[[:alpha:]]{1,}\\s(.*)", "\\2", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("precipatation", "precipitation", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("rip currents", "rip current", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("strm", "stream", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("tornadoes", "tornados", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("torndao", "tornados", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("tstm", "thunderstorm", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("tunderstorm", "thunderstorm", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("typhoo", "tornados", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("vog", "fog", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("waterspouts", "waterspout", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("wauseon", "", tidy$evtype, ignore.case = TRUE) 
tidy$evtype <- gsub("wayterspout", "waterspout", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("wi$|wi[^ndl]|win[^d]", "wind", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("winder", "winter", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("wintery", "winter", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("wintry", "winter", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- gsub("wnd", "wind", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("(hurricane|typhoon).*", "hurricane", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("(th?u?n?d?ee?r?|tstm).*", "thunderstorm wind", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("(tornado|torndao).*", "tornado", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("avalance.*", "avalanche", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("blizzard.*", "blizzard", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("coastal.*(flood|surge).*", "coastal flood", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("cold.*", "cold/wind chill", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("dust.*devel.*", "dust devil", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("dust.*storm.*", "dust storm", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("excessive.*heat.*", "excessive heat", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("extreme.*(code|wind).*", "extreme code/wind chill", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("flash.*flood.*|.*flood.*flash.*", "flash flood", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("funnel.*(cloud)*.*", "funnel cloud", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("hail.*", "hail", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("heat.*", "heat", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("heavy.*(rain|shower).*", "heavy rain", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("heavy.*snow.*", "heavy snow", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("high.*surf.*", "high surf", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("high.*wind.*", "high wind", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("ice.*storm.*", "ice storm", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("lake.*effect.*snow.*", "lake-effect snow", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("lig(h|n)tn?ing.*", "lightning", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("rip.*current.*", "rip current", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("seiche.*", "seiche", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("storm.*", "storm surge/tide", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("tropical.*storm.*", "tropical storm", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("volcanic.*", "volcanic ash", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("wild.*fire.*", "wildfire", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("summary.*", "summary", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("(.*flood.*|.*floooding.*)", "flood", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("(sleet.*|.*sleet.*|.*snow.*rain.*|.*rain.*snow.*)", "sleet", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("(.*free.*|.*frost.*)", "frost/freezing", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("(icy roads|.*snow.*|.*ice.*)", "ice/snow", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub(".*thunderstorm.*", "thunderstorm wind", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub(".*blizzard.*", "blizzard", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("(hurricane|gustnado)", "hurricane", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub("(.*seas.*|high tides|blow-out tides|blow-out tide)", "heavy seas/tides ", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub(".*erosion.*", "costal erosion", tidy$evtype, ignore.case = TRUE) 
tidy$evtype <- sub(".*whirlwind*", "whirlwind", tidy$evtype, ignore.case = TRUE)
tidy$evtype <- sub(".*landspout*", "landspout", tidy$evtype, ignore.case = TRUE)

# create a new column for group events
tidy$evgroup <- NA

# 1) select convection group events and assign it to evgroup column.
tidy[grepl("lightning", tidy$evtype) | grepl("tornado", tidy$evtype) | grepl("thunderstorm", tidy$evtype) | grepl("whirlwind", tidy$evtype) | grepl("landspout", tidy$evtype) | grepl("hail", tidy$evtype), c("evgroup")] <- "convection"

# 2) select ext. temp group events and assign it to evgroup column.
tidy[grepl("cold", tidy$evtype) | grepl("heat", tidy$evtype), c("evgroup")] <- "ext. temp"

# 3) select flood group events and assign it to evgroup column.
tidy[grepl("flash flood", tidy$evtype) | grepl("flood", tidy$evtype) |grepl("river flood", tidy$evtype), c("evgroup")] <- "flood"

# 4) select marine group events and assign it to evgroup column.
tidy[grepl("coastal storm", tidy$evtype) | grepl("tsunami", tidy$evtype) | grepl("rip current", tidy$evtype), c("evgroup")] <- "marine"

# 5) select tropical cyclones group event and assign it to evgroup column.
tidy[grepl("tropical storm", tidy$evtype) | grepl("hurricane", tidy$evtype), c("evgroup")] <- "trop. cyclones"

# 6) select winter group events and assign it to evgroup column.
tidy[grepl("winter storm", tidy$evtype) | grepl("ice", tidy$evtype) | grepl("sleet", tidy$evtype) | grepl("frost/freezing", tidy$evtype) | grepl("ice/snow", tidy$evtype) | grepl("blizzard", tidy$evtype) | grepl("avalanche", tidy$evtype), c("evgroup")] <- "winter"

# 7) select other group events and assign it to evgroup column.
tidy[is.na(tidy$evgroup), "evgroup"] <- "other"


tidy <- tidy[tidy$bgn_date > as.Date("01/01/1996 0:00:00", "%m/%e/%Y %H:%M:%S"), ]

# According to he 2006 annual summaries report. the total cost of Flash Flood and River Flood is 2,136.6 milion and 1,726.3 milion, the R code below shows one of the flood occured in 2006 cost 115 billion, where must be not correct, but $115 million cost of damage will be more realistic, hence I will replace the "b" with the "m".

# Show the events which input the property damage cost wrong in billion instead in million.
tidy[grepl("115", tidy$propdmg) & grepl("32.5", tidy$cropdmg), c(2,8,25,26,27,28)]

# replace 'b' with 'm'
tidy[grepl("115", tidy$propdmg) & grepl("32.5", tidy$cropdmg) & 
             grepl("B", tidy$propdmgexp), c("propdmgexp")] <- "M"

# According to the following url: http://thmp.info/metadata/other-storms/Other%20Storm%20Events%201955-2003.html

# it defines "Prop. damage Exp.: expressed in dollar unit type", so I will assume it is exponent, I will replace "k" with "1e3", "m" with "1e6", "b" with 1e9, and "" with "1e0".

get_exponent <- function(PROPdmgEXP) {
        if (is.na(PROPdmgEXP))  exponent <- 1 
        else {
                exp <- toupper(PROPdmgEXP);
                if (exp == "K") { exponent <- 1000 }
                else if (exp == "M") { exponent <- 1000000 }
                else if (exp == "B") { exponent <- 1000000000 }
                else { exponent <- 1 }
        }
        exponent
}

unique(tidy$propdmgexp)
unique(tidy$cropdmgexp)

# so the total damage cost is the sum of the property damage cost, resulting from propdmg times propdmgexp, and crop damage cost, resulting from cropdmg times cropdmgexp.

tidy$damage <- with(tidy, propdmg * sapply(propdmgexp, get_exponent)) +
        with(tidy, cropdmg * sapply(cropdmgexp, get_exponent))

# Remove unuseful events from the data set to reduce the date size for easier handel, the reason for this is if an event had no fatalities and no injured and no crop or property damage, than for the purpose of this analysis it is useless.

# subset observations which have fatalities or injured or damages
tidy <- subset(tidy, injuries > 0 | fatalities > 0 | damage > 0)

# compute the total casualties and save it in a new variable called
tidy$casualties <- tidy$fatalities + tidy$injuries

# subset the columns which are useful for this analysis.
tidy <- tidy[tidy$evtype!="summary", c("bgn_date", "evgroup", "evtype", "fatalities", "injuries", "casualties", "damage")]

## Results

### create a summary of event groups table to see which event group have most obsersations.
table(tidy$evgroup)


# From the table above, we can see the convection event, which includes lightning, tornado, thunderstorm wind, hail, occurs most frequncy compare to other event group, 151.801 times from 1996 to 2011 across the United States.

### Check worse Weather Event Group Types by Casualties and damage Cost

### Aggregating data by weather category events
aggr_grp <- aggregate(cbind(fatalities, injuries, casualties, damage) ~ evgroup, data = tidy, FUN=sum, na.rm=TRUE)

worse_casualties <- aggr_grp[with(aggr_grp, order(-casualties)),]
worse_casualties$evgroup <-factor(worse_casualties$evgroup, levels=worse_casualties[order(worse_casualties$casualties), "evgroup"])

worse_damage <- aggr_grp[with(aggr_grp, order(-damage)),]
worse_damage$evgroup <-factor(worse_damage$evgroup, levels=worse_damage[order(worse_damage$damage), "evgroup"])

### Figure 1: Weather Category Events:
pd1 <-  ggplot(worse_damage, aes(x=evgroup, y=damage)) +
        ggtitle("Having Greatest Economic Impact") +
        xlab("") + ylab("damage Cost") +
        theme(text = element_text(size=8), 
              axis.text.y = element_text(size= 6)) +
        geom_bar(stat="identity") +  coord_flip()

pc1 <-  ggplot(worse_casualties, aes(x=evgroup, y=casualties)) +
        xlab("Weather Category Events") +  ylab("Casualties (fatalities + injuries)") +
        theme(text = element_text(size=8), 
              axis.text.y = element_text(size= 6)) +
        ggtitle("Causing Most Casualties") +
        geom_bar(stat="identity") + coord_flip()

par(mfrow=c(1,3), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
grid.arrange(pd1, pc1, ncol=2)

### Aggregating data by event types
aggr_type <- aggregate(cbind(fatalities, injuries, casualties, damage) ~ evtype, data = tidy, FUN=sum, na.rm=TRUE)

### Getting the top ten Weather Event Types of Casualties and damage Cost
top_casualties <- aggr_type[with(aggr_type, order(-casualties)),][1:10,]
top_casualties$evtype <-factor(top_casualties$evtype, levels=top_casualties[order(top_casualties$casualties), "evtype"])

top_damage <- aggr_type[with(aggr_type, order(-damage)),][1:10,]
top_damage$evtype <-factor(top_damage$evtype, levels=top_damage[order(top_damage$damage), "evtype"])

top_fatalities <- aggr_type[with(aggr_type, order(-fatalities)),][1:10,]
top_fatalities$evtype <-factor(top_fatalities$evtype, levels=top_fatalities[order(top_fatalities$fatalities), "evtype"])

### Figure 2: Top 10 Weather Event Types:
pd2 <- ggplot(top_damage, aes(x=evtype, y=damage)) +
        ggtitle("Having Greatest Economic Impact") +
        xlab("Weather Event Type") +  ylab("damage Cost") +
        theme(text = element_text(size=8), 
              axis.text.y = element_text(size= 6)) +
        geom_bar(stat="identity") +  coord_flip()

pc2 <- ggplot(top_casualties, aes(x=evtype, y=casualties)) +
        xlab("") + ylab("Casualties (fatalities + injuries)") +
        theme(text = element_text(size=8), 
              axis.text.y = element_text(size= 6)) +
        ggtitle("Causing Most Casualties") +
        geom_bar(stat="identity") + coord_flip()

pf2 <- ggplot(top_fatalities, aes(x=evtype, y=fatalities)) +
        ggtitle("Causing Most Fatalities") +
        xlab("") + ylab("Fatalities") +
        theme(text = element_text(size=5), 
              axis.text.y = element_text(size= 6)) +
        geom_bar(stat="identity") +  coord_flip()

par(mfrow=c(1,3), mar=c(5.1,4.1,4.1,2.1), oma=c(0,0,0,0))
grid.arrange(pd2, pc2, pf2, ncol=3) 

