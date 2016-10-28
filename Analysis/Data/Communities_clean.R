#############################################################
## Chris Boomhower, Andrew Abbott, Brian Kruse, Johnny Quick
## MSDS 6372-401
## Project 2: Principal Component Analysis
## 10/27/2016
##
## Communities_clean.R
##############################

require(reshape2)
require(ggplot2)

#setwd("C:/Users/Owner/OneDrive for Business/Semester_2/Experimental Stats II (MSDS 6372)/Project 2/")

comm <- read.table("communities.data", sep = ",", stringsAsFactors = FALSE)
comm1 <- comm

commNames <- read.csv("communities_columnNames.csv", header = FALSE, stringsAsFactors = FALSE)$V1
names(comm1) <- commNames
str(comm1)

# Remove categorical attributes and police data which is 84% missing
comm1 <- subset(comm1, select = -c(
    state, county, community, communityname, fold,
    LemasSwornFT, LemasSwFTPerPop, LemasSwFTFieldOps,
    LemasSwFTFieldPerPop, LemasTotalReq, LemasTotReqPerPop,
    PolicReqPerOffic, PolicPerPop, RacialMatchCommPol,
    PctPolicWhite, PctPolicBlack, PctPolicHisp, PctPolicAsian,
    PctPolicMinor, OfficAssgnDrugUnits, NumKindsDrugsSeiz,
    PolicAveOTWorked, PolicCars, PolicOperBudg, LemasPctPolicOnPatr,
    LemasGangUnitDeploy, LemasPctOfficDrugUn, PolicBudgPerPop))

# OtherPerCap column treated as chr class for some reason... investigate further
comm1$OtherPerCap <- as.numeric(comm1$OtherPerCap)
which(is.na(comm1$OtherPerCap))

# Remove observation 131 since it is the only one with missing OtherPerCap data and remove observations where population == 0
#comm1 <- comm1[-131,]
comm1 <- comm1[-c(131,which(comm1$population == 0)),]

# Remove attributes which appear to be redundant data
comm2 <- subset(comm1, select = -c(
    numbUrban, NumUnderPov, NumIlleg, HousVacant, NumImmig,
    OwnOccLowQuart, OwnOccMedVal, OwnOccHiQuart, RentLowQ, RentMedian, RentHighQ))

# Observe attribute distributions
comm2.melt <- melt(comm2[sapply(comm2, is.numeric)])
ggplot(data = comm2.melt, mapping = aes(x = value)) +
    geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

# Transform skewed attributes (THESE NEED ADJUSTMENT STILL SINCE PLOTS NO LONGER LOOK GOOD AFTER ADDING 1 TO VALUE FOR LOGS)
comm2$population          <- log(comm2$population)
comm2$racepctblack        <- comm2$racepctblack^(1/3)
comm2$racePctWhite        <- abs(comm2$racePctWhite-1)^(1/3)
comm2$racePctAsian        <- comm2$racePctAsian^(1/3)
comm2$racePctHisp         <- comm2$racePctHisp^(1/3)
comm2$medIncome           <- sqrt(comm2$medIncome)
comm2$pctWFarmSelf        <- sqrt(comm2$pctWFarmSelf)
comm2$pctWPubAsst         <- sqrt(comm2$pctWPubAsst)
comm2$medFamInc           <- sqrt(comm2$medFamInc)
comm2$perCapInc           <- sqrt(comm2$perCapInc)
comm2$PctLess9thGrade     <- sqrt(comm2$PctLess9thGrade)
comm2$PctBSorMore         <- sqrt(comm2$PctBSorMore)
comm2$PctYoungKids2Par    <- sqrt(abs(comm2$PctYoungKids2Par-1))
comm2$PctIlleg            <- sqrt(comm2$PctIlleg)
comm2$PctImmigRecent      <- sqrt(comm2$PctImmigRecent)
comm2$PctRecentImmig      <- comm2$PctRecentImmig^(1/3)
comm2$PctRecImmig5        <- comm2$PctRecImmig5^(1/3)
comm2$PctRecImmig8        <- comm2$PctRecImmig8^(1/3)
comm2$PctRecImmig10       <- comm2$PctRecImmig10^(1/3)
comm2$PctSpeakEnglOnly    <- abs(comm2$PctSpeakEnglOnly - 1)^(1/3)
comm2$PctNotSpeakEnglWell <- comm2$PctNotSpeakEnglWell^(1/3)
comm2$PctLargHouseFam     <- comm2$PctLargHouseFam^(1/3)
comm2$PctLargHouseOccup   <- sqrt(comm2$PctLargHouseOccup)
comm2$PctPersDenseHous    <- comm2$PctPersDenseHous^(1/3)
comm2$PctHousOccup        <- sqrt(abs(comm2$PctHousOccup-1))
comm2$PctVacantBoarded    <- comm2$PctVacantBoarded^(1/3)
comm2$PctHousNoPhone      <- sqrt(comm2$PctHousNoPhone)
comm2$PctWOFullPlumb      <- sqrt(comm2$PctWOFullPlumb)
comm2$MedRent             <- sqrt(comm2$MedRent)
comm2$NumInShelters       <- comm2$NumInShelters^(1/3)
comm2$NumStreet           <- comm2$NumStreet^(1/3)
comm2$PctForeignBorn      <- comm2$PctForeignBorn^(1/3)
comm2$PctSameCity85       <- sqrt(abs(comm2$PctSameCity85-1))
comm2$PctSameState85      <- sqrt(abs(comm2$PctSameState85-1))
comm2$LandArea            <- comm2$LandArea^(1/3)
comm2$PopDens             <- comm2$PopDens^(1/3)
comm2$PctUsePubTrans      <- comm2$PctUsePubTrans^(1/3)
comm2$ViolentCrimesPerPop <- comm2$ViolentCrimesPerPop^(1/3)

# Observe new attribute distributions after transformation
comm2.melt <- melt(comm2[sapply(comm2, is.numeric)])
ggplot(data = comm2.melt, mapping = aes(x = value)) +
    geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

# Observe correlations between variables
write.csv(cor(comm2[sapply(comm2, is.numeric)]), file = "communities_Correlations.csv")
