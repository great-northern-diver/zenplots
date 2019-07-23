## By Marius Hofert and Wayne Oldford

## Generating data object 'happy' based on data from
## https://www.kaggle.com/unsdsn/world-happiness


### 1 Read data ################################################################

## 2015
happy2015.. <- read.csv("2018-05-04_data_2015.csv")
(names2015. <- names(happy2015..))
## 2016
happy2016.. <- read.csv("2018-05-04_data_2016.csv")
(names2016. <- names(happy2016..))
## 2017
happy2017.. <- read.csv("2018-05-04_data_2017.csv")
(names2017. <- names(happy2017..))


### 2 Pick out variables we work with (data columns) and clean their names #####

## Names of variables we want to keep
(longNames. <- union(names2015., union(names2016., names2017.)))
(longNames <- setdiff(longNames., c("Standard.Error", "Lower.Confidence.Interval",
                                    "Upper.Confidence.Interval", "Whisker.high",
                                    "Whisker.low")))
## => Omit 'Standard.Error', etc. (keep 'Region' although not available for 2017)
## Corresponding short names
shortNames <- c("Country", "Region", "Rank", "Happiness", "GDP", "Family",
                "Health", "Freedom", "Corruption", "Generosity", "Dystopia")
(permNames <- c("Region", "Country", "Happiness", "Rank",
                tail(shortNames, n = -4)))

## Grab out respective data columns, order columns (variables)
## 2015
happy2015. <- happy2015..[, names2015. %in% longNames]
(oldNames2015 <- names(happy2015.))
(names(happy2015.) <- shortNames[longNames %in% oldNames2015])
str(happy2015. <- happy2015.[, permNames[permNames %in% names(happy2015.)]])
## 2016
happy2016. <- happy2016..[, names2016. %in% longNames]
(oldNames2016 <- names(happy2016.))
(names(happy2016.) <- shortNames[longNames %in% oldNames2016])
str(happy2016. <- happy2016.[, permNames[permNames %in% names(happy2016.)]])
## 2017
happy2017. <- happy2017..[, names2017. %in% longNames]
(oldNames2017 <- names(happy2017.)) # => missing 'Region'
(names(happy2017.) <- shortNames[longNames %in% oldNames2017])
str(happy2017. <- happy2017.[, permNames[permNames %in% names(happy2017.)]])
## => (existing) columns now ordered


### 3 Expand rows to match for all three years (fill with NA) ##################

## Expand the data.frames to have the same, ordered countries (with possibly
## missing data).
## 2015
(allCountry <- as.factor(union(happy2015.$Country, # => factor
                               union(happy2016.$Country, happy2017.$Country))))
allCountryNames <- as.character(allCountry)
stopifnot(!is.na(allCountry))
happy2015 <- happy2015.[match(allCountry, happy2015.$Country), ]
stopifnot(all(happy2015$Country == allCountryNames, na.rm = TRUE)) # sanity check; => so even have the same order (if not missing)
## 2016
happy2016 <- happy2016.[match(allCountry, happy2016.$Country), ]
stopifnot(all(happy2016$Country == allCountryNames, na.rm = TRUE))
## 2017
happy2017 <- happy2017.[match(allCountry, happy2017.$Country), ]
stopifnot(all(happy2017$Country == allCountryNames, na.rm = TRUE))

## Determine regions corresponding to countries
stopifnot(all(happy2015$Region == happy2016$Region, na.rm = TRUE)) # same countries (if not missing) share same regions
allRegion <- happy2015$Region # => factor
isNAallRegion <- is.na(allRegion)
allRegion[isNAallRegion] <- happy2016$Region[isNAallRegion] # overlay => all available regions
isNAregions <- is.na(allRegion)
allCountry[isNAregions] # => "Taiwan Province of China" "Hong Kong S.A.R., China"
allRegion[isNAregions] <- happy2015$Region[which(happy2015$Country == "China")]
stopifnot(!is.na(allRegion))
## => 'allCountry' and 'allRegion' correspond and have no missing data

## Give everyone the same countries and regions (rest possibly missing)
## Note: Order already checked in above, so happy2015$Country <- allCountry
##       is good enough (augmenting factors for *just* filling NAs is
##       more complicated)
## 2015
happy2015$Country <- allCountry
happy2015$Region  <- allRegion
## 2016
happy2016$Country <- allCountry
happy2016$Region  <- allRegion
## 2017
happy2017$Country <- allCountry
happy2017 <- cbind(Region = allRegion, happy2017) # add Region to happy2017 (same place as for other data sets)

## Sanity checks
stopifnot(dim(happy2015) == dim(happy2016), dim(happy2016) == dim(happy2017),
          names(happy2015) == names(happy2016), names(happy2016) == names(happy2017))


### 4 Sort according to Region and Country #####################################

## 2015
ord2015 <- order(happy2015$Region, happy2015$Country) # note: *not* order(happy2015[, c("Region", "Country")])
happy2015 <- happy2015[ord2015, ]
## 2016
ord2016 <- order(happy2016$Region, happy2016$Country)
happy2016 <- happy2016[ord2016, ]
## 2017
ord2017 <- order(happy2017$Region, happy2017$Country)
happy2017 <- happy2017[ord2017, ]

## Fix row names
rownames(happy2015) <- 1:nrow(happy2015)
rownames(happy2016) <- 1:nrow(happy2016)
rownames(happy2017) <- 1:nrow(happy2017)


### 5 Add factor Time, merge, write object #####################################

## Add factor 'Time' and merge
time <- as.factor(rep(2015:2017, each = nrow(happy2015)))
happiness <- cbind(Time = time, rbind(happy2015, happy2016, happy2017))

## Last check
happiness
head(happiness)
str(happiness)

## Write .rda
save(happiness, file = "happiness.rda", compress = "xz")
