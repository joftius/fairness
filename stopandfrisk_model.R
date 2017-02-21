# Load and preproces the stop and frisk dataset
library(forcats)
library(rgdal)

source("load_stopandfrisk.R")

#stopandfrisk <- read.csv("2014_stop_and_frisk.csv",
#                         strip.white = TRUE,
#                         header = TRUE,
#                         stringsAsFactors = FALSE)

#keepcols <- which(apply(stopandfrisk, 2, function(col) mean(is.na(col))) < .4)
#nacols <- c("beat", "post") # forceuse
#stopandfrisk[,nacols] <- NULL

# sensitive variables
#sens <- c("sex", "race")

# arrested variables
arst <- c("arstmade")

# search/frisked
sf <- c("frisked", "searched")

# weapons/contraband (here we assume if NA then it is 0)
# and, we're going to collapse guns
##arms <- c("pistol", "riflshot", "asltweap", "machgun", "knifcuti", "othrweap")
##stopandfrisk$weapon <- apply(stopandfrisk[,arms], 1, function(x) (any(x == 1)))
#stopandfrisk$guncount <- rowSums(stopandfrisk[,gun], na.rm = FALSE, dims = 1)

#wc <- c("contrabn", "knifcuti", "othrweap")
##stopandfrisk[,c("weapon")][is.na(stopandfrisk[,c("weapon")])] <- 0


# force (we will also assume if NA then 0)
#fo <- c("pf_hands", "pf_wall", "pf_grnd", "pf_drwep", "pf_ptwep", "pf_baton", "pf_hcuff", "pf_pepsp", "pf_other"
#       )
#stopandfrisk[,fo][is.na(stopandfrisk[,fo])] <- 0

###dataNA <- stopandfrisk[,c(sens,app,arst,sf,"weapon","year","xcoord","ycoord","timestop","city","datestop")]#wc,"gun")]#,fo)]
# Clean! Based on: https://github.com/stablemarkets/StopAndFrisk/blob/master/code/2.0%20-%20clean%20data.R

## clean Date of stop
stopandfrisk$date<-as.character(stopandfrisk$datestop)
# dates in 2006 have different format
# non-2006 dates are of form m-dd-yyyy for single digit months
# for two digit months, mm-dd-yyyy. So add 0 to start of singe digit months
# standardize everything to mm-dd-yyyy
stopandfrisk$date<-ifelse(nchar(stopandfrisk$date)<=7 & stopandfrisk$year!=2006,paste('0',stopandfrisk$date,sep=''),stopandfrisk$date) 
stopandfrisk$date<-as.Date(stopandfrisk$date,'%m%d%Y')
stopandfrisk<-stopandfrisk[stopandfrisk$date!='1900-12-31' & !is.na(stopandfrisk$date),]
stopandfrisk$yearmon<-substr(stopandfrisk$date,1,7)
data <- stopandfrisk[complete.cases(stopandfrisk),]

raw_coords <- data.frame(x=data$xcoord, y=data$ycoord)
coordinates(raw_coords) <- c('x', 'y')

# ESRI:102718 corresponds to 
# US state Plane Coordinate Systems New York 3104, 
# Long Island Zone (1983 Survey feet) 
proj4string(raw_coords)<-CRS("+init=ESRI:102718")

# EPSG:4326 corresponds to our usually longitude/lattitude coordinate system
new_coords<-spTransform(raw_coords,CRS("+init=EPSG:4326"))

data$longitude <- new_coords$x
data$latitude  <- new_coords$y





# we'll z-score age & height & weight
age_m <- mean(data$age)
age_s <- sd(data$age)
height_m <- mean(data$height)
height_s <- sd(data$height)
weight_m <- mean(data$weight)
weight_s <- sd(data$weight)

data$age <- (data$age - mean(data$age))/sd(data$age)
data$height <- (data$height - mean(data$height))/sd(data$height)
data$weight <- (data$weight - mean(data$weight))/sd(data$weight)


# appearance variables
app <- c("age", "height", "weight", 
         #"hairblack", "hairbrown", "hairblond", "hairred", "hairwhite", "hairbald", "hairsandy", "hairsap", "hairdyed",
         #"eyeblack",  "eyebrown", "eyeblue",    "eyegreen","eyehazel",  "eyegray",  "eyemaroon", "eyediff",
         "buildheavy", "buildmusc", "buildmedium", "buildthin")


#### change "haircolr", "eyecolor", "build" to binary
###data$hairblack  <- as.numeric(data$haircolr == 1)
###data$hairbrown  <- as.numeric(data$haircolr == 2)
###data$hairblond  <- as.numeric(data$haircolr == 3)
###data$hairred    <- as.numeric(data$haircolr == 4)
#### there is no 6 in the datset, and 5 has no coding in the code book
#### so we'll assume they messed up and that 5 is white
###data$hairwhite  <- as.numeric(data$haircolr == 5)
###data$hairbald   <- as.numeric(data$haircolr == 7)
###data$hairsandy  <- as.numeric(data$haircolr == 8)
###data$hairsap    <- as.numeric(data$haircolr == 9)
###data$hairdyed   <- as.numeric(data$haircolr == 10)
#### there is no 11 in the dataset
###
###data$eyeblack    <- as.numeric(data$eyecolor == 1)
###data$eyebrown    <- as.numeric(data$eyecolor == 2)
###data$eyeblue     <- as.numeric(data$eyecolor == 3)
###data$eyegreen    <- as.numeric(data$eyecolor == 4)
###data$eyehazel    <- as.numeric(data$eyecolor == 5)
###data$eyegray     <- as.numeric(data$eyecolor == 6)
###data$eyemaroon   <- as.numeric(data$eyecolor == 7)
###data$eyediff     <- as.numeric(data$eyecolor == 8)

data$buildheavy  <- as.numeric(data$build == 1)
data$buildmusc   <- as.numeric(data$build == 2)
data$buildmedium <- as.numeric(data$build == 3)
data$buildthin   <- as.numeric(data$build == 4)



#data$iswhite <- as.numeric(data$race == 4)
###data$iswhite[data$iswhite == 0] = "non-white"
###data$iswhite[data$iswhite == 1] = "white"
#data$notwhite <- as.numeric(data$race != 4)

# change race to binary vectors
data$black     <- as.numeric(data$race == "Black")
data$blackhisp <- as.numeric(data$race == "BlaskHisp")
data$hisp      <- as.numeric(data$race == "Hispanic")
data$white     <- as.numeric(data$race == "White")
data$aspi      <- as.numeric(data$race == "AsianPacI")
data$naam      <- as.numeric(data$race == "NativeAm")


# just limit to males
data <- data[data$sex == "Male",]
#data$female <- as.numeric(data$sex == "Female")
#data$male   <- as.numeric(data$sex == "Male")

sens <- c("black", "blackhisp", "hisp", "white", "aspi", "naam") #, "female", "male")
n <- nrow(data)
d <- ncol(data)


library(rstan)
# stan model data
stop_stan_dat2 <- list(N = n, Ds = length(sens), se = data.matrix(data[,sens]), 
                             Da = length(app),  ap = data.matrix(data[,app]),
                             ar = data[,arst],
                             sf1= data[,c("searched")], sf2 = data[,c("frisked")],
                             we = data[,c("weapon")], fo = data[,c("force")], summ = data[,c("sumissue")])
                             #Do = length(fo),   fo = data[,fo]
                     #)
                
fit2 <- stan(file = 'stopandfrisk.stan', data = stop_stan_dat2, iter = 2000, chains = 1, verbose = TRUE)


# Extract information
# because criminality is negative and the weights associating criminality
# to arrest, search, frisk, weapon, force, and summons are all negative
# I'm going to negate everything associated with criminality so higher criminality 
# means more crimial
la <- extract(fit2, permuted = TRUE)
c <- -colMeans(la$c)
p <- -colMeans(la$p)
# Predict Y

# Here's how being each race helps/hurts perception
colMeans(la$w_s_p)
colMeans(la$w_ap_p)

# check these to see if perception/criminality variable is neg or pos
mean(la$w_c_ar)
mean(la$w_c_sf1)
mean(la$w_c_sf2)
mean(la$w_c_we)
mean(la$w_c_fo)
mean(la$w_c_summ)
mean(la$w_p_ar)
mean(la$w_p_sf1)
mean(la$w_p_sf2)
mean(la$w_p_we)
mean(la$w_p_fo)
mean(la$w_p_summ)
# in this case they are both pos



save(la,file='stopandfrisk_stan_fits_force.Rdata')
######################################################################################################
# Store data for prediction module

output <- data.frame(black = data$black,
                     blackhisp = data$blackhisp,
                     hisp = data$hisp,
                     white = data$white,
                     aspi = data$aspi,
                     naam = data$naam,
                     #male = data$male,
                     #female = data$female,
                     sex = data$sex,
                     #race = data$race,
                     age = data$age, 
                     height = data$height, 
                     weight = data$weight,
                     #hairblack  = data$hairblack,
                     #hairbrown  = data$hairbrown,
                     #hairblond  = data$hairblond,
                     #hairred    = data$hairred  ,
                     #hairwhite  = data$hairwhite,
                     #hairbald   = data$hairbald ,
                     #hairsandy  = data$hairsandy,
                     #hairsap    = data$hairsap  ,
                     #hairdyed   = data$hairdyed ,
                     #eyeblack   = data$eyeblack ,
                     #eyebrown   = data$eyebrown ,
                     #eyeblue    = data$eyeblue  ,
                     #eyegreen   = data$eyegreen ,
                     #eyehazel   = data$eyehazel ,
                     #eyegray    = data$eyegray  ,
                     #eyemaroon  = data$eyemaroon,
                     #eyediff    = data$eyediff  ,
                     buildheavy = data$buildheavy ,
                     buildmusc  = data$buildmusc  ,
                     buildmedium= data$buildmedium,
                     buildthin  = data$buildthin  ,
                     haircolor = data$haircolr,
                     eyecolor = data$eyecolor, 
                     build = data$build,
                     date = data$date,
                     yearmon = data$yearmon,
                     arrested = data$arstmade,
                     searched = data$searched,
                     frisked = data$frisked,
                     weapon = data$weapon,
                     longitude = data$longitude,
                     latitude = data$latitude,
                     force = data$force,
                     summons = data$sumissue,
                     criminality = c,
                     perception = p)


write.csv(output, file = "stopandfrisk_stan_results_full.csv", row.names = TRUE)

data$criminality <- c
data$perception  <- p

#########################################
library(ggplot2)
library(ggmap)




ny_plot<-ggmap(get_map("New York, NY",zoom=11,source='google',maptype='terrain'))
c_data = data.frame(longitude=data$longitude,
                    latitude =data$latitude,
                    criminality = data$criminality)
p_data = data.frame(longitude=data$longitude,
                    latitude =data$latitude,
                    perception = data$perception)




qmplot(longitude, latitude, data=c_data, maptype = "toner-background", color=criminality, size=I(0.000001), zoom = 11, darken = .7, legend = "topleft") + 
  scale_color_gradient2("Criminality", low = "white", high = "red", midpoint=0.5) #(max(data$criminality)-min(data$criminality))/2 + min(data$criminality))

# there's one dude who's really not a criminal and his/her perception is 11 while the next person is ~3.6, let's rescale appropriately
qmplot(longitude, latitude, data=p_data, maptype = "toner-background", color=perception,  size=I(0.000001), zoom = 11, darken = .7, legend = "topleft") + 
  scale_color_gradient2("Perception", low = "white", high = "blue", midpoint=(max(data$perception)-min(data$perception))/2 + min(data$perception))

whichrace <- rep(1, length(data$race)); 
whichrace[data$white == 1] <- 0
whichrace[data$aspi == 1] <- 0
whichrace[data$naam == 1] <- 0

data$race_categ <- whichrace
data$race_categ[data$race_categ == 0] = "White/AsianPac/NativeAm"
data$race_categ[data$race_categ == 1] = "Black/Hispanic"
data$race_categ <- as.factor(data$race_categ)

# HERERE

#data$racef <- as.factor(data$race)
#data$iswhite[data$iswhite == 0] = "non-white"
#data$iswhite[data$iswhite == 1] = "white"
#data$white <- as.factor(data$iswhite)

qmplot(longitude, latitude, data=data, maptype = "toner-background", color=race_categ, size=I(0.000001), zoom = 11, darken = .7, legend = "topleft") + 
  scale_color_manual(values=c("#33CC00", "#6666FF"))
  
ggplot(data, aes(race_categ)) + geom_bar(fill=c("#33CC00", "#6666FF"))


ggplot(data, aes(criminality)) + stat_density(position="identity",geom="line")
  #geom_density()
ggplot(data, aes(perception)) + stat_density(position="identity",geom="line") # +
  #xlim(c(-4,max(data$perception)))

arsum <- rep(0, length(data$arstmade)); 
arsum[data$arstmade == 1] <- 1
arsum[data$sumissue == 1] <- 1

data$arr_sum <- arsum
data$arr_sum[data$arr_sum == 1] = "Yes"
data$arr_sum[data$arr_sum == 0] = "No"
data$arr_sum <- as.factor(data$arr_sum)

qmplot(longitude, latitude, data=data, maptype = "toner-background", color=arr_sum, size=I(0.000001), zoom = 11, darken = .7, legend = "topleft") + 
  scale_color_manual(values=c("#FFFFFF", "#FF0000"))
  #scale_color_manual(values=c("#FFFF00", "#FF0000"))
ggplot(data, aes(arr_sum)) + geom_bar(fill=c("#FFFFFF", "#FF0000"))
#ggplot(data, aes(as.factor(arstmade))) + geom_bar(fill=c("#FFFFFF", "#FF0000"))