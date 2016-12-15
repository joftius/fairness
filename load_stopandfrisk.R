# Load and preproces the stop and frisk stopandfriskset

stopandfrisk <- read.csv("2014_stop_and_frisk.csv", strip.white = TRUE, header = TRUE)
stopandfrisk <- stopandfrisk[,which(apply(stopandfrisk, 2, function(col) mean(is.na(col))) < .1)]
stopandfrisk$year <- NULL
stopandfrisk$linecm <- NULL
stopandfrisk$dob <- NULL
stopandfrisk$datestop <- NULL
stopandfrisk$repcmd <- NULL
stopandfrisk$revcmd <- NULL
faccols <- which(apply(stopandfrisk, 2, function(col) length(unique(col))) < 10)
for (col in faccols) stopandfrisk[,col] <- factor(stopandfrisk[,col])
stringcols <- which(sapply(stopandfrisk[1,], function(v) is.factor(v) & nlevels(v) > 10))
stopandfrisk <- stopandfrisk[,-stringcols]
stopandfrisk <- stopandfrisk[complete.cases(stopandfrisk),]
stopandfrisk$pct <- factor(stopandfrisk$pct)
stopandfrisk$race <- fct_recode(factor(stopandfrisk$race),
                        Black = "1", BlaskHisp = "2", Hispanic = "3",
                        White = "4", AsianPacI = "5", NativeAm = "6")
stopandfrisk$sex <- fct_recode(factor(stopandfrisk$sex), Female = "0", Male = "1")
stopandfrisk$city <- fct_recode(factor(stopandfrisk$city),
                        Manhattan = "1", Brooklyn = "2", Bronx = "3",
                        Queens = "4", "StatenIsland" = 5)
stopandfrisk$build <- fct_recode(factor(stopandfrisk$build), heavy = "1", muscular = "2",
                         medium = "3", thin = "4")
