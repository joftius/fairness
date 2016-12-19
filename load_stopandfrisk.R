# Load and preproces the stop and frisk dataset

stopandfrisk <- read.csv("2014_stop_and_frisk.csv", strip.white = TRUE, header = TRUE)
stopandfrisk <- stopandfrisk[,which(apply(stopandfrisk, 2, function(col) mean(is.na(col))) < .4)]
cols <- which(apply(stopandfrisk, 2, function(col) mean(is.na(col))) > .05)
stopandfrisk[,cols][is.na(stopandfrisk[,cols])] <- 0
stopandfrisk[,c("year", "linecm", "dob"," datestop", "repcmd", "revcmd", "ser_num",
                "addrpct", "typeofid", "explnstop", "offverb", "officrid", "offshld",
                "recstat")] <- NULL
faccols <- which(apply(stopandfrisk, 2, function(col) length(unique(col))) < 10)
for (col in faccols) stopandfrisk[,col] <- factor(stopandfrisk[,col])
stringcols <- which(sapply(stopandfrisk[1,], function(v) is.factor(v) & nlevels(v) > 10))
stopandfrisk <- stopandfrisk[,-stringcols]
stopandfrisk <- stopandfrisk[complete.cases(stopandfrisk),]
stopandfrisk$pct <- factor(stopandfrisk$pct)
stopandfrisk$sector <- factor(stopandfrisk$sector)
stopandfrisk$race <- fct_recode(factor(stopandfrisk$race),
                        Black = "1", BlaskHisp = "2", Hispanic = "3",
                        White = "4", AsianPacI = "5", NativeAm = "6")
stopandfrisk$sex <- fct_recode(factor(stopandfrisk$sex), Female = "0", Male = "1")
stopandfrisk$city <- fct_recode(factor(stopandfrisk$city),
                        Manhattan = "1", Brooklyn = "2", Bronx = "3",
                        Queens = "4", "StatenIsland" = 5)
stopandfrisk$build <- fct_recode(factor(stopandfrisk$build), heavy = "1", muscular = "2",
                         medium = "3", thin = "4")
stopandfrisk$haircolr <- fct_recode(factor(stopandfrisk$haircolr), black = "1", brown = "2",
                                 blonde = "3", other = "4", other = "5", bald = "7",
                                 other = "8", other ="9", other = "10")

# Collapse some categories
othrweapons <- c("riflshot", "asltweap", "machgun", "othrweap")
stopandfrisk$othrweapon <- apply(stopandfrisk[,othrweapons], 1, function(x) (any(x == "1")))
stopandfrisk[,othrweapons] <- NULL
othrforce <- c("pf_baton", "pf_pepsp", "pf_other")
stopandfrisk$pf_othr <- apply(stopandfrisk[,othrforce], 1, function(x) (any(x == "1")))
stopandfrisk[,othrforce] <- NULL

pf_cols <- grep("pf_", names(stopandfrisk), fixed = TRUE, value = TRUE)
cs_cols <- grep("cs_", names(stopandfrisk), fixed = TRUE, value = TRUE)
rf_cols <- grep("rf_", names(stopandfrisk), fixed = TRUE, value = TRUE)
sb_cols <- grep("sb_", names(stopandfrisk), fixed = TRUE, value = TRUE)
ac_cols <- grep("ac_", names(stopandfrisk), fixed = TRUE, value = TRUE)
reasoncols <- grep("_", names(stopandfrisk), fixed=TRUE, value=TRUE)
#apply(stopandfrisk[,reasoncols], 2, function(x) sum(x == "1"))
#apply(stopandfrisk[,cs_cols], 2, function(x) sum(x == "1"))
#apply(stopandfrisk[,rf_cols], 2, function(x) sum(x == "1"))

# Truncate long tailed predictors / omit weird outliers
# question: > or >= ?
stopandfrisk$perobs5 <- stopandfrisk$perobs >= 5
stopandfrisk$perobs[stopandfrisk$perobs5] <- 5 
stopandfrisk$perstop10 <- stopandfrisk$perstop >= 10
stopandfrisk$perstop[stopandfrisk$perstop10] <- 10
stopandfrisk <- stopandfrisk[stopandfrisk$age > 11,]
stopandfrisk <- stopandfrisk[stopandfrisk$age <= 65,]
stopandfrisk$weight[stopandfrisk$weight > 300] <- 300
stopandfrisk <- stopandfrisk[stopandfrisk$weight >= 80,]
stopandfrisk <- stopandfrisk[stopandfrisk$height >= 55,]

