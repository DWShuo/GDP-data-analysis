library("readxl")
library("stringr")
library("ggplot2")
library("blscrapeR")
library("rpart")
library("rpart.plot")
library("MASS")
library("caret")
#=========== CPI =============================================
CPI <- read_excel("CPI.xlsx",sheet = "BLS Data Series")
CPI <- data.frame(CPI)
CPI <- CPI[-(1:10),] #romve top rows
names(CPI) <- CPI[1,]#set column names
CPI <- CPI[-1,]#remove column names rows
CPI <- CPI[c("Year","Annual")]
rownames(CPI) <- CPI[,1]#reset row labels
CPI <- CPI["Annual"]

curCPI = as.numeric(CPI[nrow(CPI),])
CPImulti = apply(CPI, 1, function(x) ( (curCPI-as.numeric(x[1])) / as.numeric(x[1]) + 1))

#parse in data for analysis
bTaxA <- read_excel("Section6.xlsx",sheet ="T61700A-A")
bTaxB <- read_excel("Section6.xlsx",sheet ="T61700B-A")
bTaxC <- read_excel("Section6.xlsx",sheet ="T61700C-A")
bTaxD <- read_excel("Section6.xlsx",sheet ="T61700D-A")

TaxA <- read_excel("Section6.xlsx",sheet ="T61900A-A")
TaxB <- read_excel("Section6.xlsx",sheet ="T61900B-A")
TaxC <- read_excel("Section6.xlsx",sheet ="T61900C-A")
TaxD <- read_excel("Section6.xlsx",sheet ="T61900D-A")

divA <- read_excel("Section6.xlsx",sheet ="T62000A-A")
divB <- read_excel("Section6.xlsx",sheet ="T62000B-A")
divC <- read_excel("Section6.xlsx",sheet ="T62000C-A")
divD <- read_excel("Section6.xlsx",sheet ="T62000D-A")

undA <- read_excel("Section6.xlsx",sheet ="T62100A-A")
undB <- read_excel("Section6.xlsx",sheet ="T62100B-A")
undC <- read_excel("Section6.xlsx",sheet ="T62100C-A")
undD <- read_excel("Section6.xlsx",sheet ="T62100D-A")

wageA <- read_excel("Section6.xlsx",sheet ="T60600A-A")
wageB <- read_excel("Section6.xlsx",sheet ="T60600B-A")
wageC <- read_excel("Section6.xlsx",sheet ="T60600C-A")
wageD <- read_excel("Section6.xlsx",sheet ="T60600D-A")

#=========== wageA =============================================
wageA <- data.frame(wageA)#put in dataframe
wageA <- wageA[-(1:6),] #romve top rows
wageA <- head(wageA,-25) #remove botttom rows
wageA <- wageA[,-c(1,3)]#remove useless rows
wageA[1,1] <- "Industry" #replace title
wageA[,1] <- str_remove(wageA[,1],"\\\\[:digit:]\\\\")

names(wageA) <- wageA[1,]#set column names
wageA <- wageA[-1,]#remove column names rows
rownames(wageA) <- make.names(wageA[,1],unique=TRUE)#reset row labels
wageA <- wageA[,-1]#remove row names rows
wageA[] <- lapply(wageA, as.numeric)

rownames(wageA)[rownames(wageA) == 'Agricultural.services..forestry..and.fisheries'] <- 'Forestry..fishing..and.related.activities'

wageA["Metal.mining",] <- wageA["Metal.mining",]+wageA["Anthracite.mining",]+wageA["Bituminous.and.other.soft.coal.mining",]+wageA["Nonmetallic.mining.and.quarrying",]
rownames(wageA)[rownames(wageA) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
rownames(wageA)[rownames(wageA) == 'Crude.petroleum.and.natural.gas'] <- 'Oil.and.gas.extraction'
wageA = wageA[!row.names(wageA)%in%c("Anthracite.mining","Bituminous.and.other.soft.coal.mining","Nonmetallic.mining.and.quarrying"),]

rownames(wageA)[rownames(wageA) == 'Contract.construction'] <- 'Construction'

wageA["Lumber.and.basic.timber.products",] <- wageA["Lumber.and.basic.timber.products",]+wageA["Furniture.and.finished.lumber.products",]
rownames(wageA)[rownames(wageA) == "Lumber.and.basic.timber.products"] <- 'Wood.products'
wageA = wageA[!row.names(wageA)%in%c("Furniture.and.finished.lumber.products"),]

rownames(wageA)[rownames(wageA) == "Iron.and.steel.and.their.products..including.ordnance"] <- "Primary.metal.industries"
rownames(wageA)[rownames(wageA) == "Nonferrous.metals.and.their.products"] <- "Fabricated.metal.products"
rownames(wageA)[rownames(wageA) == "Machinery..except.electrical"] <- "Machinery"
rownames(wageA)[rownames(wageA) == "Miscellaneous.manufacturing..including.instruments.and.miscellaneous.plastic.products"] <- "Miscellaneous.manufacturing"

wageA["Food.and.kindred.products",] <- wageA["Food.and.kindred.products",]+wageA["Tobacco.manufactures",]
rownames(wageA)[rownames(wageA) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
wageA = wageA[!row.names(wageA)%in%c("Tobacco.manufactures"),]

wageA["Apparel.and.other.textile.products",] <- wageA["Apparel.and.other.textile.products",]+wageA["Leather.and.leather.products",]
rownames(wageA)[rownames(wageA) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
wageA = wageA[!row.names(wageA)%in%c("Leather.and.leather.products"),]

rownames(wageA)[rownames(wageA) == "Rubber.products"] <- 'Plastics.and.rubber.products'
wageA = wageA[!row.names(wageA)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]

rownames(wageA)[rownames(wageA) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(wageA)[rownames(wageA) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

wageA = wageA[!row.names(wageA)%in%c("Telephone.and.telegraph","Radio.and.television.broadcasting","Utilities..electric.and.gas","Local.utilities.and.public.services..n.e.c.","Wholesale.trade","Retail.trade.and.automobile.services"),]
rownames(wageA)[rownames(wageA) == "Communication"] <- "Broadcasting.and.telecommunications"

wageA = wageA[!row.names(wageA)%in%c("Finance..insurance..and.real.estate"),]
wageA["Banking",] <- wageA["Banking",]+wageA["Credit.agencies..other.than.banks..and.holding.and.other.investment.companies",]
rownames(wageA)[rownames(wageA) == "Banking"] <- 'Banking.and.credit.agencies'
wageA = wageA[!row.names(wageA)%in%c("Credit.agencies..other.than.banks..and.holding.and.other.investment.companies"),]
rownames(wageA)[rownames(wageA) == "Security.and.commodity.brokers..and.services"] <- 'Security.and.commodity.brokers'

wageA["Insurance.carriers",] <- wageA["Insurance.carriers",]+wageA["Insurance.agents..brokers..and.service",]
rownames(wageA)[rownames(wageA) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
wageA = wageA[!row.names(wageA)%in%c("Insurance.agents..brokers..and.service"),]

wageA = wageA[!row.names(wageA)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
wageA = wageA[!row.names(wageA)%in%c("Membership.organizations","Miscellaneous.professional.services","Private.households"),]
rownames(wageA)[rownames(wageA) == "Educational.services..n.e.c."] <- 'Educational.services'

wageA = wageA[!row.names(wageA)%in%c("General.government","Government.enterprises","General.government.1","Government.enterprises.1","Other.services"),]

wageA = wageA[-ncol(wageA)]

#=========== wageB =============================================
wageB <- data.frame(wageB)#put in dataframe
wageB <- wageB[-(1:6),] #romve top rows
wageB <- head(wageB,-21) #remove botttom rows
wageB <- wageB[,-c(1,3)]#remove useless rows
wageB[1,1] <- "Industry" #replace title
wageB[,1] <- str_remove(wageB[,1],"\\\\[:digit:]\\\\")

names(wageB) <- wageB[1,]#set column names
wageB <- wageB[-1,]#remove column names rows
rownames(wageB) <- make.names(wageB[,1],unique=TRUE)#reset row labels
wageB <- wageB[,-1]#remove row names rows
wageB[] <- lapply(wageB, as.numeric)

rownames(wageB)[rownames(wageB) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

wageB["Metal.mining",] <- wageB["Metal.mining",]+wageB["Coal.mining",]+wageB["Nonmetallic.minerals..except.fuels",]
rownames(wageB)[rownames(wageB) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
wageB = wageB[!row.names(wageB)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]

wageB["Lumber.and.wood.products",] <- wageB["Lumber.and.wood.products",]+wageB["Furniture.and.fixtures",]
rownames(wageB)[rownames(wageB) == "Lumber.and.wood.products"] <- 'Wood.products'
wageB = wageB[!row.names(wageB)%in%c("Furniture.and.fixtures"),]

rownames(wageB)[rownames(wageB) == "Machinery..except.electrical"] <- "Machinery"
wageB = wageB[!row.names(wageB)%in%c("Instruments.and.related.products"),]
rownames(wageB)[rownames(wageB) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

wageB["Food.and.kindred.products",] <- wageB["Food.and.kindred.products",]+wageB["Tobacco.manufactures",]
rownames(wageB)[rownames(wageB) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
wageB = wageB[!row.names(wageB)%in%c("Tobacco.manufactures"),]

wageB["Apparel.and.other.textile.products",] <- wageB["Apparel.and.other.textile.products",]+wageB["Leather.and.leather.products",]
rownames(wageB)[rownames(wageB) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
wageB = wageB[!row.names(wageB)%in%c("Leather.and.leather.products"),]

rownames(wageB)[rownames(wageB) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'

wageB = wageB[!row.names(wageB)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(wageB)[rownames(wageB) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(wageB)[rownames(wageB) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

wageB = wageB[!row.names(wageB)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(wageB)[rownames(wageB) == "Communication"] <- "Broadcasting.and.telecommunications"

wageB = wageB[!row.names(wageB)%in%c("Finance..insurance..and.real.estate"),]
wageB["Banking",] <- wageB["Banking",]+wageB["Credit.agencies.other.than.banks",]
rownames(wageB)[rownames(wageB) == "Banking"] <- 'Banking.and.credit.agencies'
wageB = wageB[!row.names(wageB)%in%c("Credit.agencies.other.than.banks","Federal.Reserve.banks","Commercial.and.mutual.banks"),]

wageB["Insurance.carriers",] <- wageB["Insurance.carriers",]+wageB["Insurance.agents..brokers..and.service",]
rownames(wageB)[rownames(wageB) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
wageB = wageB[!row.names(wageB)%in%c("Insurance.agents..brokers..and.service"),]

wageB = wageB[!row.names(wageB)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
wageB = wageB[!row.names(wageB)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
wageB = wageB[!row.names(wageB)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Miscellaneous.professional.services","Private.households"),]
wageB = wageB[!row.names(wageB)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1","Other.services"),]

#=========== wageC =============================================
wageC <- data.frame(wageC)#put in dataframe
wageC <- wageC[-(1:6),] #romve top rows
wageC <- head(wageC,-23) #remove botttom rows
wageC <- wageC[,-c(1,3,4)]#remove useless rows
wageC[1,1] <- "Industry" #replace title
wageC[,1] <- str_remove(wageC[,1],"\\\\[:digit:]\\\\")
 
names(wageC) <- wageC[1,]#set column names
wageC <- wageC[-1,]#remove column names rows
rownames(wageC) <- make.names(wageC[,1],unique=TRUE)#reset row labels
wageC <- wageC[,-1]#remove row names rows
wageC[] <- lapply(wageC, as.numeric)
 
rownames(wageC)[rownames(wageC) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

wageC["Metal.mining",] <- wageC["Metal.mining",]+wageC["Coal.mining",]+wageC["Nonmetallic.minerals..except.fuels",]
rownames(wageC)[rownames(wageC) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
wageC = wageC[!row.names(wageC)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]
 
wageC["Lumber.and.wood.products",] <- wageC["Lumber.and.wood.products",]+wageC["Furniture.and.fixtures",]
rownames(wageC)[rownames(wageC) == "Lumber.and.wood.products"] <- 'Wood.products'
wageC = wageC[!row.names(wageC)%in%c("Furniture.and.fixtures"),]

rownames(wageC)[rownames(wageC) == "Industrial.machinery.and.equipment"] <- "Machinery"
rownames(wageC)[rownames(wageC) == "Electronic.and.other.electric.equipment"] <- "Electric.and.electronic.equipment"
wageC = wageC[!row.names(wageC)%in%c("Furniture.and.fixtures"),]
rownames(wageC)[rownames(wageC) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

wageC["Food.and.kindred.products",] <- wageC["Food.and.kindred.products",]+wageC["Tobacco.products",]
rownames(wageC)[rownames(wageC) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
wageC = wageC[!row.names(wageC)%in%c("Instruments.and.related.products","Tobacco.products"),]
 
wageC["Apparel.and.other.textile.products",] <- wageC["Apparel.and.other.textile.products",]+wageC["Leather.and.leather.products",]
rownames(wageC)[rownames(wageC) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
wageC = wageC[!row.names(wageC)%in%c("Leather.and.leather.products"),]

rownames(wageC)[rownames(wageC) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'
 
wageC = wageC[!row.names(wageC)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(wageC)[rownames(wageC) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(wageC)[rownames(wageC) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

wageC = wageC[!row.names(wageC)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(wageC)[rownames(wageC) == "Communications"] <- "Broadcasting.and.telecommunications"

wageC = wageC[!row.names(wageC)%in%c("Finance..insurance..and.real.estate"),]
wageC["Depository.institutions",] <- wageC["Depository.institutions",]+wageC["Nondepository.institutions",]
rownames(wageC)[rownames(wageC) == "Depository.institutions"] <- 'Banking.and.credit.agencies'
wageC = wageC[!row.names(wageC)%in%c("Nondepository.institutions","Federal.Reserve.banks","Commercial.and.mutual.depository.institutions"),]

wageC["Insurance.carriers",] <- wageC["Insurance.carriers",]+wageC["Insurance.agents..brokers..and.service",]
rownames(wageC)[rownames(wageC) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
wageC = wageC[!row.names(wageC)%in%c("Insurance.agents..brokers..and.service"),]
 
wageC = wageC[!row.names(wageC)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
wageC = wageC[!row.names(wageC)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
wageC = wageC[!row.names(wageC)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Other.services","Private.households"),]
wageC = wageC[!row.names(wageC)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]

#=========== wageD =============================================
wageD <- data.frame(wageD)#put in dataframe
wageD <- wageD[-(1:6),] #romve top rows
wageD <- head(wageD,-22) #remove botttom rows
wageD <- wageD[,-c(1,3)]#remove useless rows
wageD[1,1] <- "Industry" #replace title
wageD[,1] <- str_remove(wageD[,1],"\\\\[:digit:]\\\\")
 
names(wageD) <- wageD[1,]#set column names
wageD <- wageD[-1,]#remove column names rows
rownames(wageD) <- make.names(wageD[,1],unique=TRUE)#reset row labels
wageD <- wageD[,-1]#remove row names rows
wageD[] <- lapply(wageD, as.numeric)

wageD = wageD[-c(1:3)]

wageD = wageD[!row.names(wageD)%in%c("Support.activities.for.mining"),]

rownames(wageD)[rownames(wageD) == "Nonmetallic.mineral.products"] <- "Stone..clay..and.glass.products"

wageD = wageD[!row.names(wageD)%in%c("Wholesale.trade","Durable.goods.1","Nondurable.goods.1","Retail.trade","Motor.vehicle.and.parts.dealers","Food.and.beverage.stores","General.merchandise.stores","Other.retail"),]

wageD["Computer.and.electronic.products",] <- wageD["Computer.and.electronic.products",]+wageD["Electrical.equipment..appliances..and.components",]
rownames(wageD)[rownames(wageD) == "Computer.and.electronic.products"] <- 'Electric.and.electronic.equipment'
wageD = wageD[!row.names(wageD)%in%c("Electrical.equipment..appliances..and.components"),]

rownames(wageD)[rownames(wageD) == "Motor.vehicles..bodies.and.trailers..and.parts"] <- 'Motor.vehicles.and.equipment'
rownames(wageD)[rownames(wageD) == "Textile.mills.and.textile.product.mills"] <- 'Textile.mill.products'
rownames(wageD)[rownames(wageD) == "Paper.products"] <- 'Paper.and.allied.products'
rownames(wageD)[rownames(wageD) == "Printing.and.related.support.activities"] <- 'Printing.and.publishing'
rownames(wageD)[rownames(wageD) == "Chemical.products"] <- 'Chemicals.and.allied.products'
rownames(wageD)[rownames(wageD) == "Rail.transportation"] <- 'Railroad.transportation'
rownames(wageD)[rownames(wageD) == "Transit.and.ground.passenger.transportation"] <- 'Local.and.interurban.passenger.transit'
rownames(wageD)[rownames(wageD) == "Air.transportation"] <- 'Transportation.by.air'
rownames(wageD)[rownames(wageD) == "Utilities"] <- 'Electric..gas..and.sanitary.services'
rownames(wageD)[rownames(wageD) == "Federal.Reserve.banks..credit.intermediation..and.related.activities"] <- 'Banking.credit.agencies'
rownames(wageD)[rownames(wageD) == "Securities..commodity.contracts..and.investments"] <- 'Security.and.commodity.brokers'
rownames(wageD)[rownames(wageD) == "Amusements..gambling..and.recreation.industries"] <- 'Amusement.and.recreation.services'

wageD = wageD[!row.names(wageD)%in%c("Furniture.and.related.products","Transportation.and.warehousing"),]
wageD = wageD[!row.names(wageD)%in%c("Other.transportation.and.support.activities","Warehousing.and.storage","Information","Publishing.industries..includes.software.","Motion.picture.and.sound.recording.industries","Information.and.data.processing.services"),]
wageD = wageD[!row.names(wageD)%in%c("Funds..trusts..and.other.financial.vehicles","Real.estate.and.rental.and.leasing","Rental.and.leasing.services.and.lessors.of.intangible.assets"),]
wageD = wageD[!row.names(wageD)%in%c("Finance.and.insurance","Health.care.and.social.assistance"),]
wageD = wageD[!row.names(wageD)%in%c("Professional..scientific..and.technical.services","Computer.systems.design.and.related.services","Miscellaneous.professional..scientific..and.technical.services","Management.of.bTaxAnies.and.enterprises","Administrative.and.waste.management.services","Administrative.and.support.services","Waste.management.and.remediation.services","Social.assistance","Arts..entertainment..and.recreation","Performing.arts..spectator.sports..museums..and.related.activities","Accommodation.and.food.services","Accommodation","Food.services.and.drinking.places","Other.services..except.government"),]

wageD["Ambulatory.health.care.services",] <- wageD["Ambulatory.health.care.services",]+wageD["Hospitals.and.nursing.and.residential.care.facilities",]
rownames(wageD)[rownames(wageD) == "Ambulatory.health.care.services"] <- 'Health.services'
wageD = wageD[!row.names(wageD)%in%c("Hospitals.and.nursing.and.residential.care.facilities","Management.of.companies.and.enterprises"),]

wageD = wageD[c(1:6,8,7,9:nrow(wageD)),]
wageD = wageD[c(1:27,29,28,30:nrow(wageD)),]
wageD = wageD[c(1:30,32,31,33:nrow(wageD)),]
wageD = wageD[c(1:31,35,34,33,32,36:nrow(wageD)),]
wageD <- rbind(wageD[1:8,], wageD[10:37,], wageD[9,], wageD[38:nrow(wageD),])

wageD["Federal.Reserve.banks",] <- wageD["Federal.Reserve.banks",]+wageD["Credit.intermediation.and.related.activities",]
rownames(wageD)[rownames(wageD) == "Federal.Reserve.banks"] <- 'Banking.credit.agencies'
wageD = wageD[!row.names(wageD)%in%c("Credit.intermediation.and.related.activities"),]
wageD = wageD[c(1:41,45,44,42,43,46:nrow(wageD)),]

wageD = wageD[!row.names(wageD)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1","Private.industries"),]
wageD <- head(wageD,-2) #remove botttom rows

#=========== undA =============================================
undA <- data.frame(undA)#put in dataframe
undA <- undA[-(1:6),] #romve top rows
undA <- head(undA,-6) #remove botttom rows
undA <- undA[,-c(1,3)]#remove useless rows
undA[1,1] <- "Industry" #replace title
undA[,1] <- str_remove(undA[,1],"\\\\[:digit:]\\\\")

names(undA) <- undA[1,]#set column names
undA <- undA[-1,]#remove column names rows
rownames(undA) <- make.names(undA[,1],unique=TRUE)#reset row labels
undA <- undA[,-1]#remove row names rows
undA[] <- lapply(undA, as.numeric)

rownames(undA)[rownames(undA) == 'Agricultural.services..forestry..and.fisheries'] <- 'Forestry..fishing..and.related.activities'

undA["Metal.mining",] <- undA["Metal.mining",]+undA["Anthracite.mining",]+undA["Bituminous.and.other.soft.coal.mining",]+undA["Nonmetallic.mining.and.quarrying",]
rownames(undA)[rownames(undA) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
rownames(undA)[rownames(undA) == 'Crude.petroleum.and.natural.gas'] <- 'Oil.and.gas.extraction'
undA = undA[!row.names(undA)%in%c("Anthracite.mining","Bituminous.and.other.soft.coal.mining","Nonmetallic.mining.and.quarrying"),]

rownames(undA)[rownames(undA) == 'Contract.construction'] <- 'Construction'

undA["Lumber.and.basic.timber.products",] <- undA["Lumber.and.basic.timber.products",]+undA["Furniture.and.finished.lumber.products",]
rownames(undA)[rownames(undA) == "Lumber.and.basic.timber.products"] <- 'Wood.products'
undA = undA[!row.names(undA)%in%c("Furniture.and.finished.lumber.products"),]

rownames(undA)[rownames(undA) == "Iron.and.steel.and.their.products..including.ordnance"] <- "Primary.metal.industries"
rownames(undA)[rownames(undA) == "Nonferrous.metals.and.their.products"] <- "Fabricated.metal.products"
rownames(undA)[rownames(undA) == "Machinery..except.electrical"] <- "Machinery"
rownames(undA)[rownames(undA) == "Miscellaneous.manufacturing..including.instruments.and.miscellaneous.plastic.products"] <- "Miscellaneous.manufacturing"

undA["Food.and.kindred.products",] <- undA["Food.and.kindred.products",]+undA["Tobacco.manufactures",]
rownames(undA)[rownames(undA) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
undA = undA[!row.names(undA)%in%c("Tobacco.manufactures"),]

undA["Apparel.and.other.textile.products",] <- undA["Apparel.and.other.textile.products",]+undA["Leather.and.leather.products",]
rownames(undA)[rownames(undA) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
undA = undA[!row.names(undA)%in%c("Leather.and.leather.products"),]

rownames(undA)[rownames(undA) == "Rubber.products"] <- 'Plastics.and.rubber.products'
undA = undA[!row.names(undA)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]

rownames(undA)[rownames(undA) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(undA)[rownames(undA) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

undA = undA[!row.names(undA)%in%c("Telephone.and.telegraph","Radio.and.television.broadcasting","Utilities..electric.and.gas","Local.utilities.and.public.services..n.e.c.","Wholesale.trade","Retail.trade.and.automobile.services"),]
rownames(undA)[rownames(undA) == "Communication"] <- "Broadcasting.and.telecommunications"

undA = undA[!row.names(undA)%in%c("Finance..insurance..and.real.estate"),]
undA["Banking",] <- undA["Banking",]+undA["Credit.agencies..other.than.banks..and.holding.and.other.investment.companies",]
rownames(undA)[rownames(undA) == "Banking"] <- 'Banking.and.credit.agencies'
undA = undA[!row.names(undA)%in%c("Credit.agencies..other.than.banks..and.holding.and.other.investment.companies"),]
rownames(undA)[rownames(undA) == "Security.and.commodity.brokers..and.services"] <- 'Security.and.commodity.brokers'

undA["Insurance.carriers",] <- undA["Insurance.carriers",]+undA["Insurance.agents..brokers..and.service",]
rownames(undA)[rownames(undA) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
undA = undA[!row.names(undA)%in%c("Insurance.agents..brokers..and.service"),]

undA = undA[!row.names(undA)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
undA = undA[!row.names(undA)%in%c("Membership.organizations","Miscellaneous.professional.services","Private.households"),]
rownames(undA)[rownames(undA) == "Educational.services..n.e.c."] <- 'Educational.services'

undA = undA[!row.names(undA)%in%c("General.government","Government.enterprises","General.government.1","Government.enterprises.1","Other.services"),]

#undA = undA[-ncol(undA)]

#=========== undB =============================================
undB <- data.frame(undB)#put in dataframe
undB <- undB[-(1:6),] #romve top rows
undB <- head(undB,-9) #remove botttom rows
undB <- undB[,-c(1,3)]#remove useless rows
undB[1,1] <- "Industry" #replace title
undB[,1] <- str_remove(undB[,1],"\\\\[:digit:]\\\\")

names(undB) <- undB[1,]#set column names
undB <- undB[-1,]#remove column names rows
rownames(undB) <- make.names(undB[,1],unique=TRUE)#reset row labels
undB <- undB[,-1]#remove row names rows
undB[] <- lapply(undB, as.numeric)

rownames(undB)[rownames(undB) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

undB["Metal.mining",] <- undB["Metal.mining",]+undB["Coal.mining",]+undB["Nonmetallic.minerals..except.fuels",]
rownames(undB)[rownames(undB) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
undB = undB[!row.names(undB)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]

undB["Lumber.and.wood.products",] <- undB["Lumber.and.wood.products",]+undB["Furniture.and.fixtures",]
rownames(undB)[rownames(undB) == "Lumber.and.wood.products"] <- 'Wood.products'
undB = undB[!row.names(undB)%in%c("Furniture.and.fixtures"),]

rownames(undB)[rownames(undB) == "Machinery..except.electrical"] <- "Machinery"
undB = undB[!row.names(undB)%in%c("Instruments.and.related.products"),]
rownames(undB)[rownames(undB) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

undB["Food.and.kindred.products",] <- undB["Food.and.kindred.products",]+undB["Tobacco.manufactures",]
rownames(undB)[rownames(undB) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
undB = undB[!row.names(undB)%in%c("Tobacco.manufactures"),]

undB["Apparel.and.other.textile.products",] <- undB["Apparel.and.other.textile.products",]+undB["Leather.and.leather.products",]
rownames(undB)[rownames(undB) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
undB = undB[!row.names(undB)%in%c("Leather.and.leather.products"),]

rownames(undB)[rownames(undB) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'

undB = undB[!row.names(undB)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(undB)[rownames(undB) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(undB)[rownames(undB) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

undB = undB[!row.names(undB)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(undB)[rownames(undB) == "Communication"] <- "Broadcasting.and.telecommunications"

undB = undB[!row.names(undB)%in%c("Finance..insurance..and.real.estate"),]
undB["Banking",] <- undB["Banking",]+undB["Credit.agencies.other.than.banks",]
rownames(undB)[rownames(undB) == "Banking"] <- 'Banking.and.credit.agencies'
undB = undB[!row.names(undB)%in%c("Credit.agencies.other.than.banks","Federal.Reserve.banks","Commercial.and.mutual.banks"),]

undB["Insurance.carriers",] <- undB["Insurance.carriers",]+undB["Insurance.agents..brokers..and.service",]
rownames(undB)[rownames(undB) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
undB = undB[!row.names(undB)%in%c("Insurance.agents..brokers..and.service"),]

undB = undB[!row.names(undB)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
undB = undB[!row.names(undB)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
undB = undB[!row.names(undB)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Miscellaneous.professional.services","Private.households"),]
undB = undB[!row.names(undB)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1","Other.services"),]

#=========== undC =============================================
undC <- data.frame(undC)#put in dataframe
undC <- undC[-(1:6),] #romve top rows
undC <- head(undC,-9) #remove botttom rows
undC <- undC[,-c(1,3,4)]#remove useless rows
undC[1,1] <- "Industry" #replace title
undC[,1] <- str_remove(undC[,1],"\\\\[:digit:]\\\\")
 
names(undC) <- undC[1,]#set column names
undC <- undC[-1,]#remove column names rows
rownames(undC) <- make.names(undC[,1],unique=TRUE)#reset row labels
undC <- undC[,-1]#remove row names rows
undC[] <- lapply(undC, as.numeric)
 
rownames(undC)[rownames(undC) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

undC["Metal.mining",] <- undC["Metal.mining",]+undC["Coal.mining",]+undC["Nonmetallic.minerals..except.fuels",]
rownames(undC)[rownames(undC) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
undC = undC[!row.names(undC)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]
 
undC["Lumber.and.wood.products",] <- undC["Lumber.and.wood.products",]+undC["Furniture.and.fixtures",]
rownames(undC)[rownames(undC) == "Lumber.and.wood.products"] <- 'Wood.products'
undC = undC[!row.names(undC)%in%c("Furniture.and.fixtures"),]

rownames(undC)[rownames(undC) == "Industrial.machinery.and.equipment"] <- "Machinery"
rownames(undC)[rownames(undC) == "Electronic.and.other.electric.equipment"] <- "Electric.and.electronic.equipment"
undC = undC[!row.names(undC)%in%c("Furniture.and.fixtures"),]
rownames(undC)[rownames(undC) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

undC["Food.and.kindred.products",] <- undC["Food.and.kindred.products",]+undC["Tobacco.products",]
rownames(undC)[rownames(undC) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
undC = undC[!row.names(undC)%in%c("Instruments.and.related.products","Tobacco.products"),]
 
undC["Apparel.and.other.textile.products",] <- undC["Apparel.and.other.textile.products",]+undC["Leather.and.leather.products",]
rownames(undC)[rownames(undC) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
undC = undC[!row.names(undC)%in%c("Leather.and.leather.products"),]

rownames(undC)[rownames(undC) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'
 
undC = undC[!row.names(undC)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(undC)[rownames(undC) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(undC)[rownames(undC) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

undC = undC[!row.names(undC)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(undC)[rownames(undC) == "Communications"] <- "Broadcasting.and.telecommunications"

undC = undC[!row.names(undC)%in%c("Finance..insurance..and.real.estate"),]
undC["Depository.institutions",] <- undC["Depository.institutions",]+undC["Nondepository.institutions",]
rownames(undC)[rownames(undC) == "Depository.institutions"] <- 'Banking.and.credit.agencies'
undC = undC[!row.names(undC)%in%c("Nondepository.institutions","Federal.Reserve.banks","Commercial.and.mutual.depository.institutions"),]

undC["Insurance.carriers",] <- undC["Insurance.carriers",]+undC["Insurance.agents..brokers..and.service",]
rownames(undC)[rownames(undC) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
undC = undC[!row.names(undC)%in%c("Insurance.agents..brokers..and.service"),]
 
undC = undC[!row.names(undC)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
undC = undC[!row.names(undC)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
undC = undC[!row.names(undC)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Other.services","Private.households"),]
undC = undC[!row.names(undC)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]

#=========== divD =============================================
undD <- data.frame(undD)#put in dataframe
undD <- undD[-(1:6),] #romve top rows
undD <- head(undD,-13) #remove botttom rows
undD <- undD[,-c(1,3)]#remove useless rows
undD[1,1] <- "Industry" #replace title
undD[,1] <- str_remove(undD[,1],"\\\\[:digit:]\\\\")
 
names(undD) <- undD[1,]#set column names
undD <- undD[-1,]#remove column names rows
rownames(undD) <- make.names(undD[,1],unique=TRUE)#reset row labels
undD <- undD[,-1]#remove row names rows
undD[] <- lapply(undD, as.numeric)

undD = undD[-c(1:3)]

undD = undD[!row.names(undD)%in%c("Support.activities.for.mining"),]

rownames(undD)[rownames(undD) == "Nonmetallic.mineral.products"] <- "Stone..clay..and.glass.products"

undD = undD[!row.names(undD)%in%c("Wholesale.trade","Durable.goods.1","Nondurable.goods.1","Retail.trade","Motor.vehicle.and.parts.dealers","Food.and.beverage.stores","General.merchandise.stores","Other.retail"),]

undD["Computer.and.electronic.products",] <- undD["Computer.and.electronic.products",]+undD["Electrical.equipment..appliances..and.components",]
rownames(undD)[rownames(undD) == "Computer.and.electronic.products"] <- 'Electric.and.electronic.equipment'
undD = undD[!row.names(undD)%in%c("Electrical.equipment..appliances..and.components"),]

rownames(undD)[rownames(undD) == "Motor.vehicles..bodies.and.trailers..and.parts"] <- 'Motor.vehicles.and.equipment'
rownames(undD)[rownames(undD) == "Textile.mills.and.textile.product.mills"] <- 'Textile.mill.products'
rownames(undD)[rownames(undD) == "Paper.products"] <- 'Paper.and.allied.products'
rownames(undD)[rownames(undD) == "Printing.and.related.support.activities"] <- 'Printing.and.publishing'
rownames(undD)[rownames(undD) == "Chemical.products"] <- 'Chemicals.and.allied.products'
rownames(undD)[rownames(undD) == "Rail.transportation"] <- 'Railroad.transportation'
rownames(undD)[rownames(undD) == "Transit.and.ground.passenger.transportation"] <- 'Local.and.interurban.passenger.transit'
rownames(undD)[rownames(undD) == "Air.transportation"] <- 'Transportation.by.air'
rownames(undD)[rownames(undD) == "Utilities"] <- 'Electric..gas..and.sanitary.services'
rownames(undD)[rownames(undD) == "Federal.Reserve.banks..credit.intermediation..and.related.activities"] <- 'Banking.credit.agencies'
rownames(undD)[rownames(undD) == "Securities..commodity.contracts..and.investments"] <- 'Security.and.commodity.brokers'
rownames(undD)[rownames(undD) == "Amusements..gambling..and.recreation.industries"] <- 'Amusement.and.recreation.services'

undD = undD[!row.names(undD)%in%c("Furniture.and.related.products","Transportation.and.warehousing"),]
undD = undD[!row.names(undD)%in%c("Other.transportation.and.support.activities","Warehousing.and.storage","Information","Publishing.industries..includes.software.","Motion.picture.and.sound.recording.industries","Information.and.data.processing.services"),]
undD = undD[!row.names(undD)%in%c("Funds..trusts..and.other.financial.vehicles","Real.estate.and.rental.and.leasing","Rental.and.leasing.services.and.lessors.of.intangible.assets"),]
undD = undD[!row.names(undD)%in%c("Finance.and.insurance","Health.care.and.social.assistance"),]
undD = undD[!row.names(undD)%in%c("Professional..scientific..and.technical.services","Computer.systems.design.and.related.services","Miscellaneous.professional..scientific..and.technical.services","Management.of.bTaxAnies.and.enterprises","Administrative.and.waste.management.services","Administrative.and.support.services","Waste.management.and.remediation.services","Social.assistance","Arts..entertainment..and.recreation","Performing.arts..spectator.sports..museums..and.related.activities","Accommodation.and.food.services","Accommodation","Food.services.and.drinking.places","Other.services..except.government"),]

undD["Ambulatory.health.care.services",] <- undD["Ambulatory.health.care.services",]+undD["Hospitals.and.nursing.and.residential.care.facilities",]
rownames(undD)[rownames(undD) == "Ambulatory.health.care.services"] <- 'Health.services'
undD = undD[!row.names(undD)%in%c("Hospitals.and.nursing.and.residential.care.facilities","Management.of.companies.and.enterprises"),]

undD = undD[c(1:6,8,7,9:nrow(undD)),]
undD = undD[c(1:27,29,28,30:nrow(undD)),]
undD = undD[c(1:30,32,31,33:nrow(undD)),]
undD = undD[c(1:31,35,34,33,32,36:nrow(undD)),]
undD <- rbind(undD[1:8,], undD[10:37,], undD[9,], undD[38:nrow(undD),])

undD["Federal.Reserve.banks",] <- undD["Federal.Reserve.banks",]+undD["Credit.intermediation.and.related.activities",]
rownames(undD)[rownames(undD) == "Federal.Reserve.banks"] <- 'Banking.credit.agencies'
undD = undD[!row.names(undD)%in%c("Credit.intermediation.and.related.activities"),]
undD = undD[c(1:41,45,44,42,43,46:nrow(undD)),]

undD = undD[!row.names(undD)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]
undD <- head(undD,-2) #remove botttom rows

#=========== divA =============================================
divA <- data.frame(divA)#put in dataframe
divA <- divA[-(1:6),] #romve top rows
divA <- head(divA,-6) #remove botttom rows
divA <- divA[,-c(1,3)]#remove useless rows
divA[1,1] <- "Industry" #replace title
divA[,1] <- str_remove(divA[,1],"\\\\[:digit:]\\\\")

names(divA) <- divA[1,]#set column names
divA <- divA[-1,]#remove column names rows
rownames(divA) <- make.names(divA[,1],unique=TRUE)#reset row labels
divA <- divA[,-1]#remove row names rows
divA[] <- lapply(divA, as.numeric)

rownames(divA)[rownames(divA) == 'Agricultural.services..forestry..and.fisheries'] <- 'Forestry..fishing..and.related.activities'

divA["Metal.mining",] <- divA["Metal.mining",]+divA["Anthracite.mining",]+divA["Bituminous.and.other.soft.coal.mining",]+divA["Nonmetallic.mining.and.quarrying",]
rownames(divA)[rownames(divA) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
rownames(divA)[rownames(divA) == 'Crude.petroleum.and.natural.gas'] <- 'Oil.and.gas.extraction'
divA = divA[!row.names(divA)%in%c("Anthracite.mining","Bituminous.and.other.soft.coal.mining","Nonmetallic.mining.and.quarrying"),]

rownames(divA)[rownames(divA) == 'Contract.construction'] <- 'Construction'

divA["Lumber.and.basic.timber.products",] <- divA["Lumber.and.basic.timber.products",]+divA["Furniture.and.finished.lumber.products",]
rownames(divA)[rownames(divA) == "Lumber.and.basic.timber.products"] <- 'Wood.products'
divA = divA[!row.names(divA)%in%c("Furniture.and.finished.lumber.products"),]

rownames(divA)[rownames(divA) == "Iron.and.steel.and.their.products..including.ordnance"] <- "Primary.metal.industries"
rownames(divA)[rownames(divA) == "Nonferrous.metals.and.their.products"] <- "Fabricated.metal.products"
rownames(divA)[rownames(divA) == "Machinery..except.electrical"] <- "Machinery"
rownames(divA)[rownames(divA) == "Miscellaneous.manufacturing..including.instruments.and.miscellaneous.plastic.products"] <- "Miscellaneous.manufacturing"

divA["Food.and.kindred.products",] <- divA["Food.and.kindred.products",]+divA["Tobacco.manufactures",]
rownames(divA)[rownames(divA) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
divA = divA[!row.names(divA)%in%c("Tobacco.manufactures"),]

divA["Apparel.and.other.textile.products",] <- divA["Apparel.and.other.textile.products",]+divA["Leather.and.leather.products",]
rownames(divA)[rownames(divA) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
divA = divA[!row.names(divA)%in%c("Leather.and.leather.products"),]

rownames(divA)[rownames(divA) == "Rubber.products"] <- 'Plastics.and.rubber.products'
divA = divA[!row.names(divA)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]

rownames(divA)[rownames(divA) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(divA)[rownames(divA) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

divA = divA[!row.names(divA)%in%c("Telephone.and.telegraph","Radio.and.television.broadcasting","Utilities..electric.and.gas","Local.utilities.and.public.services..n.e.c.","Wholesale.trade","Retail.trade.and.automobile.services"),]
rownames(divA)[rownames(divA) == "Communication"] <- "Broadcasting.and.telecommunications"

divA = divA[!row.names(divA)%in%c("Finance..insurance..and.real.estate"),]
divA["Banking",] <- divA["Banking",]+divA["Credit.agencies..other.than.banks..and.holding.and.other.investment.companies",]
rownames(divA)[rownames(divA) == "Banking"] <- 'Banking.and.credit.agencies'
divA = divA[!row.names(divA)%in%c("Credit.agencies..other.than.banks..and.holding.and.other.investment.companies"),]
rownames(divA)[rownames(divA) == "Security.and.commodity.brokers..and.services"] <- 'Security.and.commodity.brokers'

divA["Insurance.carriers",] <- divA["Insurance.carriers",]+divA["Insurance.agents..brokers..and.service",]
rownames(divA)[rownames(divA) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
divA = divA[!row.names(divA)%in%c("Insurance.agents..brokers..and.service"),]

divA = divA[!row.names(divA)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
divA = divA[!row.names(divA)%in%c("Membership.organizations","Miscellaneous.professional.services","Private.households"),]
rownames(divA)[rownames(divA) == "Educational.services..n.e.c."] <- 'Educational.services'

divA = divA[!row.names(divA)%in%c("General.government","Government.enterprises","General.government.1","Government.enterprises.1","Other.services"),]

#divA = divA[-ncol(divA)]

#=========== divB =============================================
divB <- data.frame(divB)#put in dataframe
divB <- divB[-(1:6),] #romve top rows
divB <- head(divB,-9) #remove botttom rows
divB <- divB[,-c(1,3)]#remove useless rows
divB[1,1] <- "Industry" #replace title
divB[,1] <- str_remove(divB[,1],"\\\\[:digit:]\\\\")

names(divB) <- divB[1,]#set column names
divB <- divB[-1,]#remove column names rows
rownames(divB) <- make.names(divB[,1],unique=TRUE)#reset row labels
divB <- divB[,-1]#remove row names rows
divB[] <- lapply(divB, as.numeric)

rownames(divB)[rownames(divB) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

divB["Metal.mining",] <- divB["Metal.mining",]+divB["Coal.mining",]+divB["Nonmetallic.minerals..except.fuels",]
rownames(divB)[rownames(divB) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
divB = divB[!row.names(divB)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]

divB["Lumber.and.wood.products",] <- divB["Lumber.and.wood.products",]+divB["Furniture.and.fixtures",]
rownames(divB)[rownames(divB) == "Lumber.and.wood.products"] <- 'Wood.products'
divB = divB[!row.names(divB)%in%c("Furniture.and.fixtures"),]

rownames(divB)[rownames(divB) == "Machinery..except.electrical"] <- "Machinery"
divB = divB[!row.names(divB)%in%c("Instruments.and.related.products"),]
rownames(divB)[rownames(divB) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

divB["Food.and.kindred.products",] <- divB["Food.and.kindred.products",]+divB["Tobacco.manufactures",]
rownames(divB)[rownames(divB) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
divB = divB[!row.names(divB)%in%c("Tobacco.manufactures"),]

divB["Apparel.and.other.textile.products",] <- divB["Apparel.and.other.textile.products",]+divB["Leather.and.leather.products",]
rownames(divB)[rownames(divB) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
divB = divB[!row.names(divB)%in%c("Leather.and.leather.products"),]

rownames(divB)[rownames(divB) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'

divB = divB[!row.names(divB)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(divB)[rownames(divB) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(divB)[rownames(divB) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

divB = divB[!row.names(divB)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(divB)[rownames(divB) == "Communication"] <- "Broadcasting.and.telecommunications"

divB = divB[!row.names(divB)%in%c("Finance..insurance..and.real.estate"),]
divB["Banking",] <- divB["Banking",]+divB["Credit.agencies.other.than.banks",]
rownames(divB)[rownames(divB) == "Banking"] <- 'Banking.and.credit.agencies'
divB = divB[!row.names(divB)%in%c("Credit.agencies.other.than.banks","Federal.Reserve.banks","Commercial.and.mutual.banks"),]

divB["Insurance.carriers",] <- divB["Insurance.carriers",]+divB["Insurance.agents..brokers..and.service",]
rownames(divB)[rownames(divB) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
divB = divB[!row.names(divB)%in%c("Insurance.agents..brokers..and.service"),]

divB = divB[!row.names(divB)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
divB = divB[!row.names(divB)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
divB = divB[!row.names(divB)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Miscellaneous.professional.services","Private.households"),]
divB = divB[!row.names(divB)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1","Other.services"),]

#=========== divC =============================================
divC <- data.frame(divC)#put in dataframe
divC <- divC[-(1:6),] #romve top rows
divC <- head(divC,-9) #remove botttom rows
divC <- divC[,-c(1,3,4)]#remove useless rows
divC[1,1] <- "Industry" #replace title
divC[,1] <- str_remove(divC[,1],"\\\\[:digit:]\\\\")
 
names(divC) <- divC[1,]#set column names
divC <- divC[-1,]#remove column names rows
rownames(divC) <- make.names(divC[,1],unique=TRUE)#reset row labels
divC <- divC[,-1]#remove row names rows
divC[] <- lapply(divC, as.numeric)
 
rownames(divC)[rownames(divC) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

divC["Metal.mining",] <- divC["Metal.mining",]+divC["Coal.mining",]+divC["Nonmetallic.minerals..except.fuels",]
rownames(divC)[rownames(divC) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
divC = divC[!row.names(divC)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]
 
divC["Lumber.and.wood.products",] <- divC["Lumber.and.wood.products",]+divC["Furniture.and.fixtures",]
rownames(divC)[rownames(divC) == "Lumber.and.wood.products"] <- 'Wood.products'
divC = divC[!row.names(divC)%in%c("Furniture.and.fixtures"),]

rownames(divC)[rownames(divC) == "Industrial.machinery.and.equipment"] <- "Machinery"
rownames(divC)[rownames(divC) == "Electronic.and.other.electric.equipment"] <- "Electric.and.electronic.equipment"
divC = divC[!row.names(divC)%in%c("Furniture.and.fixtures"),]
rownames(divC)[rownames(divC) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

divC["Food.and.kindred.products",] <- divC["Food.and.kindred.products",]+divC["Tobacco.products",]
rownames(divC)[rownames(divC) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
divC = divC[!row.names(divC)%in%c("Instruments.and.related.products","Tobacco.products"),]
 
divC["Apparel.and.other.textile.products",] <- divC["Apparel.and.other.textile.products",]+divC["Leather.and.leather.products",]
rownames(divC)[rownames(divC) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
divC = divC[!row.names(divC)%in%c("Leather.and.leather.products"),]

rownames(divC)[rownames(divC) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'
 
divC = divC[!row.names(divC)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(divC)[rownames(divC) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(divC)[rownames(divC) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

divC = divC[!row.names(divC)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(divC)[rownames(divC) == "Communications"] <- "Broadcasting.and.telecommunications"

divC = divC[!row.names(divC)%in%c("Finance..insurance..and.real.estate"),]
divC["Depository.institutions",] <- divC["Depository.institutions",]+divC["Nondepository.institutions",]
rownames(divC)[rownames(divC) == "Depository.institutions"] <- 'Banking.and.credit.agencies'
divC = divC[!row.names(divC)%in%c("Nondepository.institutions","Federal.Reserve.banks","Commercial.and.mutual.depository.institutions"),]

divC["Insurance.carriers",] <- divC["Insurance.carriers",]+divC["Insurance.agents..brokers..and.service",]
rownames(divC)[rownames(divC) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
divC = divC[!row.names(divC)%in%c("Insurance.agents..brokers..and.service"),]
 
divC = divC[!row.names(divC)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
divC = divC[!row.names(divC)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
divC = divC[!row.names(divC)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Other.services","Private.households"),]
divC = divC[!row.names(divC)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]

#=========== divD =============================================
divD <- data.frame(divD)#put in dataframe
divD <- divD[-(1:6),] #romve top rows
divD <- head(divD,-13) #remove botttom rows
divD <- divD[,-c(1,3)]#remove useless rows
divD[1,1] <- "Industry" #replace title
divD[,1] <- str_remove(divD[,1],"\\\\[:digit:]\\\\")
 
names(divD) <- divD[1,]#set column names
divD <- divD[-1,]#remove column names rows
rownames(divD) <- make.names(divD[,1],unique=TRUE)#reset row labels
divD <- divD[,-1]#remove row names rows
divD[] <- lapply(divD, as.numeric)

divD = divD[-c(1:3)]

divD = divD[!row.names(divD)%in%c("Support.activities.for.mining"),]

rownames(divD)[rownames(divD) == "Nonmetallic.mineral.products"] <- "Stone..clay..and.glass.products"

divD = divD[!row.names(divD)%in%c("Wholesale.trade","Durable.goods.1","Nondurable.goods.1","Retail.trade","Motor.vehicle.and.parts.dealers","Food.and.beverage.stores","General.merchandise.stores","Other.retail"),]

divD["Computer.and.electronic.products",] <- divD["Computer.and.electronic.products",]+divD["Electrical.equipment..appliances..and.components",]
rownames(divD)[rownames(divD) == "Computer.and.electronic.products"] <- 'Electric.and.electronic.equipment'
divD = divD[!row.names(divD)%in%c("Electrical.equipment..appliances..and.components"),]

rownames(divD)[rownames(divD) == "Motor.vehicles..bodies.and.trailers..and.parts"] <- 'Motor.vehicles.and.equipment'
rownames(divD)[rownames(divD) == "Textile.mills.and.textile.product.mills"] <- 'Textile.mill.products'
rownames(divD)[rownames(divD) == "Paper.products"] <- 'Paper.and.allied.products'
rownames(divD)[rownames(divD) == "Printing.and.related.support.activities"] <- 'Printing.and.publishing'
rownames(divD)[rownames(divD) == "Chemical.products"] <- 'Chemicals.and.allied.products'
rownames(divD)[rownames(divD) == "Rail.transportation"] <- 'Railroad.transportation'
rownames(divD)[rownames(divD) == "Transit.and.ground.passenger.transportation"] <- 'Local.and.interurban.passenger.transit'
rownames(divD)[rownames(divD) == "Air.transportation"] <- 'Transportation.by.air'
rownames(divD)[rownames(divD) == "Utilities"] <- 'Electric..gas..and.sanitary.services'
rownames(divD)[rownames(divD) == "Federal.Reserve.banks..credit.intermediation..and.related.activities"] <- 'Banking.credit.agencies'
rownames(divD)[rownames(divD) == "Securities..commodity.contracts..and.investments"] <- 'Security.and.commodity.brokers'
rownames(divD)[rownames(divD) == "Amusements..gambling..and.recreation.industries"] <- 'Amusement.and.recreation.services'

divD = divD[!row.names(divD)%in%c("Furniture.and.related.products","Transportation.and.warehousing"),]
divD = divD[!row.names(divD)%in%c("Other.transportation.and.support.activities","Warehousing.and.storage","Information","Publishing.industries..includes.software.","Motion.picture.and.sound.recording.industries","Information.and.data.processing.services"),]
divD = divD[!row.names(divD)%in%c("Funds..trusts..and.other.financial.vehicles","Real.estate.and.rental.and.leasing","Rental.and.leasing.services.and.lessors.of.intangible.assets"),]
divD = divD[!row.names(divD)%in%c("Finance.and.insurance","Health.care.and.social.assistance"),]
divD = divD[!row.names(divD)%in%c("Professional..scientific..and.technical.services","Computer.systems.design.and.related.services","Miscellaneous.professional..scientific..and.technical.services","Management.of.bTaxAnies.and.enterprises","Administrative.and.waste.management.services","Administrative.and.support.services","Waste.management.and.remediation.services","Social.assistance","Arts..entertainment..and.recreation","Performing.arts..spectator.sports..museums..and.related.activities","Accommodation.and.food.services","Accommodation","Food.services.and.drinking.places","Other.services..except.government"),]

divD["Ambulatory.health.care.services",] <- divD["Ambulatory.health.care.services",]+divD["Hospitals.and.nursing.and.residential.care.facilities",]
rownames(divD)[rownames(divD) == "Ambulatory.health.care.services"] <- 'Health.services'
divD = divD[!row.names(divD)%in%c("Hospitals.and.nursing.and.residential.care.facilities","Management.of.companies.and.enterprises"),]

divD = divD[c(1:6,8,7,9:nrow(divD)),]
divD = divD[c(1:27,29,28,30:nrow(divD)),]
divD = divD[c(1:30,32,31,33:nrow(divD)),]
divD = divD[c(1:31,35,34,33,32,36:nrow(divD)),]
divD <- rbind(divD[1:8,], divD[10:37,], divD[9,], divD[38:nrow(divD),])

divD["Federal.Reserve.banks",] <- divD["Federal.Reserve.banks",]+divD["Credit.intermediation.and.related.activities",]
rownames(divD)[rownames(divD) == "Federal.Reserve.banks"] <- 'Banking.credit.agencies'
divD = divD[!row.names(divD)%in%c("Credit.intermediation.and.related.activities"),]
divD = divD[c(1:41,45,44,42,43,46:nrow(divD)),]

divD = divD[!row.names(divD)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]
divD <- head(divD,-2) #remove botttom rows

#=========== bTaxA =============================================
bTaxA <- data.frame(bTaxA)#put in dataframe
bTaxA <- bTaxA[-(1:6),] #romve top rows
bTaxA <- head(bTaxA,-8) #remove botttom rows
bTaxA <- bTaxA[,-c(1,3)]#remove useless rows
bTaxA[1,1] <- "Industry" #replace title
bTaxA[,1] <- str_remove(bTaxA[,1],"\\\\[:digit:]\\\\")

names(bTaxA) <- bTaxA[1,]#set column names
bTaxA <- bTaxA[-1,]#remove column names rows
rownames(bTaxA) <- make.names(bTaxA[,1],unique=TRUE)#reset row labels
bTaxA <- bTaxA[,-1]#remove row names rows
bTaxA[] <- lapply(bTaxA, as.numeric)

rownames(bTaxA)[rownames(bTaxA) == 'Agricultural.services..forestry..and.fisheries'] <- 'Forestry..fishing..and.related.activities'

bTaxA["Metal.mining",] <- bTaxA["Metal.mining",]+bTaxA["Anthracite.mining",]+bTaxA["Bituminous.and.other.soft.coal.mining",]+bTaxA["Nonmetallic.mining.and.quarrying",]
rownames(bTaxA)[rownames(bTaxA) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
rownames(bTaxA)[rownames(bTaxA) == 'Crude.petroleum.and.natural.gas'] <- 'Oil.and.gas.extraction'
bTaxA = bTaxA[!row.names(bTaxA)%in%c("Anthracite.mining","Bituminous.and.other.soft.coal.mining","Nonmetallic.mining.and.quarrying"),]

rownames(bTaxA)[rownames(bTaxA) == 'Contract.construction'] <- 'Construction'

bTaxA["Lumber.and.basic.timber.products",] <- bTaxA["Lumber.and.basic.timber.products",]+bTaxA["Furniture.and.finished.lumber.products",]
rownames(bTaxA)[rownames(bTaxA) == "Lumber.and.basic.timber.products"] <- 'Wood.products'
bTaxA = bTaxA[!row.names(bTaxA)%in%c("Furniture.and.finished.lumber.products"),]

rownames(bTaxA)[rownames(bTaxA) == "Iron.and.steel.and.their.products..including.ordnance"] <- "Primary.metal.industries"
rownames(bTaxA)[rownames(bTaxA) == "Nonferrous.metals.and.their.products"] <- "Fabricated.metal.products"
rownames(bTaxA)[rownames(bTaxA) == "Machinery..except.electrical"] <- "Machinery"
rownames(bTaxA)[rownames(bTaxA) == "Miscellaneous.manufacturing..including.instruments.and.miscellaneous.plastic.products"] <- "Miscellaneous.manufacturing"

bTaxA["Food.and.kindred.products",] <- bTaxA["Food.and.kindred.products",]+bTaxA["Tobacco.manufactures",]
rownames(bTaxA)[rownames(bTaxA) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
bTaxA = bTaxA[!row.names(bTaxA)%in%c("Tobacco.manufactures"),]

bTaxA["Apparel.and.other.textile.products",] <- bTaxA["Apparel.and.other.textile.products",]+bTaxA["Leather.and.leather.products",]
rownames(bTaxA)[rownames(bTaxA) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
bTaxA = bTaxA[!row.names(bTaxA)%in%c("Leather.and.leather.products"),]

rownames(bTaxA)[rownames(bTaxA) == "Rubber.products"] <- 'Plastics.and.rubber.products'
bTaxA = bTaxA[!row.names(bTaxA)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]

rownames(bTaxA)[rownames(bTaxA) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(bTaxA)[rownames(bTaxA) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

bTaxA = bTaxA[!row.names(bTaxA)%in%c("Telephone.and.telegraph","Radio.and.television.broadcasting","Utilities..electric.and.gas","Local.utilities.and.public.services..n.e.c.","Wholesale.trade","Retail.trade.and.automobile.services"),]
rownames(bTaxA)[rownames(bTaxA) == "Communication"] <- "Broadcasting.and.telecommunications"

bTaxA = bTaxA[!row.names(bTaxA)%in%c("Finance..insurance..and.real.estate"),]
bTaxA["Banking",] <- bTaxA["Banking",]+bTaxA["Credit.agencies..other.than.banks..and.holding.and.other.investment.companies",]
rownames(bTaxA)[rownames(bTaxA) == "Banking"] <- 'Banking.and.credit.agencies'
bTaxA = bTaxA[!row.names(bTaxA)%in%c("Credit.agencies..other.than.banks..and.holding.and.other.investment.companies"),]
rownames(bTaxA)[rownames(bTaxA) == "Security.and.commodity.brokers..and.services"] <- 'Security.and.commodity.brokers'

bTaxA["Insurance.carriers",] <- bTaxA["Insurance.carriers",]+bTaxA["Insurance.agents..brokers..and.service",]
rownames(bTaxA)[rownames(bTaxA) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
bTaxA = bTaxA[!row.names(bTaxA)%in%c("Insurance.agents..brokers..and.service"),]

bTaxA = bTaxA[!row.names(bTaxA)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
bTaxA = bTaxA[!row.names(bTaxA)%in%c("Membership.organizations","Miscellaneous.professional.services","Private.households"),]
rownames(bTaxA)[rownames(bTaxA) == "Educational.services..n.e.c."] <- 'Educational.services'

bTaxA = bTaxA[!row.names(bTaxA)%in%c("General.government","Government.enterprises","General.government.1","Government.enterprises.1","Other.services"),]

bTaxA = bTaxA[-ncol(bTaxA)]

#=========== bTaxB =============================================
bTaxB <- data.frame(bTaxB)#put in dataframe
bTaxB <- bTaxB[-(1:6),] #romve top rows
bTaxB <- head(bTaxB,-8) #remove botttom rows
bTaxB <- bTaxB[,-c(1,3)]#remove useless rows
bTaxB[1,1] <- "Industry" #replace title
bTaxB[,1] <- str_remove(bTaxB[,1],"\\\\[:digit:]\\\\")

names(bTaxB) <- bTaxB[1,]#set column names
bTaxB <- bTaxB[-1,]#remove column names rows
rownames(bTaxB) <- make.names(bTaxB[,1],unique=TRUE)#reset row labels
bTaxB <- bTaxB[,-1]#remove row names rows
bTaxB[] <- lapply(bTaxB, as.numeric)

rownames(bTaxB)[rownames(bTaxB) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

bTaxB["Metal.mining",] <- bTaxB["Metal.mining",]+bTaxB["Coal.mining",]+bTaxB["Nonmetallic.minerals..except.fuels",]
rownames(bTaxB)[rownames(bTaxB) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
bTaxB = bTaxB[!row.names(bTaxB)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]

bTaxB["Lumber.and.wood.products",] <- bTaxB["Lumber.and.wood.products",]+bTaxB["Furniture.and.fixtures",]
rownames(bTaxB)[rownames(bTaxB) == "Lumber.and.wood.products"] <- 'Wood.products'
bTaxB = bTaxB[!row.names(bTaxB)%in%c("Furniture.and.fixtures"),]

rownames(bTaxB)[rownames(bTaxB) == "Machinery..except.electrical"] <- "Machinery"
bTaxB = bTaxB[!row.names(bTaxB)%in%c("Instruments.and.related.products"),]
rownames(bTaxB)[rownames(bTaxB) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

bTaxB["Food.and.kindred.products",] <- bTaxB["Food.and.kindred.products",]+bTaxB["Tobacco.manufactures",]
rownames(bTaxB)[rownames(bTaxB) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
bTaxB = bTaxB[!row.names(bTaxB)%in%c("Tobacco.manufactures"),]

bTaxB["Apparel.and.other.textile.products",] <- bTaxB["Apparel.and.other.textile.products",]+bTaxB["Leather.and.leather.products",]
rownames(bTaxB)[rownames(bTaxB) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
bTaxB = bTaxB[!row.names(bTaxB)%in%c("Leather.and.leather.products"),]

rownames(bTaxB)[rownames(bTaxB) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'

bTaxB = bTaxB[!row.names(bTaxB)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(bTaxB)[rownames(bTaxB) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(bTaxB)[rownames(bTaxB) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

bTaxB = bTaxB[!row.names(bTaxB)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(bTaxB)[rownames(bTaxB) == "Communication"] <- "Broadcasting.and.telecommunications"

bTaxB = bTaxB[!row.names(bTaxB)%in%c("Finance..insurance..and.real.estate"),]
bTaxB["Banking",] <- bTaxB["Banking",]+bTaxB["Credit.agencies.other.than.banks",]
rownames(bTaxB)[rownames(bTaxB) == "Banking"] <- 'Banking.and.credit.agencies'
bTaxB = bTaxB[!row.names(bTaxB)%in%c("Credit.agencies.other.than.banks","Federal.Reserve.banks","Commercial.and.mutual.banks"),]

bTaxB["Insurance.carriers",] <- bTaxB["Insurance.carriers",]+bTaxB["Insurance.agents..brokers..and.service",]
rownames(bTaxB)[rownames(bTaxB) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
bTaxB = bTaxB[!row.names(bTaxB)%in%c("Insurance.agents..brokers..and.service"),]

bTaxB = bTaxB[!row.names(bTaxB)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
bTaxB = bTaxB[!row.names(bTaxB)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
bTaxB = bTaxB[!row.names(bTaxB)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Miscellaneous.professional.services","Private.households"),]
bTaxB = bTaxB[!row.names(bTaxB)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1","Other.services"),]

#=========== bTaxC =============================================
bTaxC <- data.frame(bTaxC)#put in dataframe
bTaxC <- bTaxC[-(1:6),] #romve top rows
bTaxC <- head(bTaxC,-8) #remove botttom rows
bTaxC <- bTaxC[,-c(1,3,4)]#remove useless rows
bTaxC[1,1] <- "Industry" #replace title
bTaxC[,1] <- str_remove(bTaxC[,1],"\\\\[:digit:]\\\\")
 
names(bTaxC) <- bTaxC[1,]#set column names
bTaxC <- bTaxC[-1,]#remove column names rows
rownames(bTaxC) <- make.names(bTaxC[,1],unique=TRUE)#reset row labels
bTaxC <- bTaxC[,-1]#remove row names rows
bTaxC[] <- lapply(bTaxC, as.numeric)
 
rownames(bTaxC)[rownames(bTaxC) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

bTaxC["Metal.mining",] <- bTaxC["Metal.mining",]+bTaxC["Coal.mining",]+bTaxC["Nonmetallic.minerals..except.fuels",]
rownames(bTaxC)[rownames(bTaxC) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]
 
bTaxC["Lumber.and.wood.products",] <- bTaxC["Lumber.and.wood.products",]+bTaxC["Furniture.and.fixtures",]
rownames(bTaxC)[rownames(bTaxC) == "Lumber.and.wood.products"] <- 'Wood.products'
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Furniture.and.fixtures"),]

rownames(bTaxC)[rownames(bTaxC) == "Industrial.machinery.and.equipment"] <- "Machinery"
rownames(bTaxC)[rownames(bTaxC) == "Electronic.and.other.electric.equipment"] <- "Electric.and.electronic.equipment"
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Furniture.and.fixtures"),]
rownames(bTaxC)[rownames(bTaxC) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

bTaxC["Food.and.kindred.products",] <- bTaxC["Food.and.kindred.products",]+bTaxC["Tobacco.products",]
rownames(bTaxC)[rownames(bTaxC) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Instruments.and.related.products","Tobacco.products"),]
 
bTaxC["Apparel.and.other.textile.products",] <- bTaxC["Apparel.and.other.textile.products",]+bTaxC["Leather.and.leather.products",]
rownames(bTaxC)[rownames(bTaxC) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Leather.and.leather.products"),]

rownames(bTaxC)[rownames(bTaxC) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'
 
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(bTaxC)[rownames(bTaxC) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(bTaxC)[rownames(bTaxC) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

bTaxC = bTaxC[!row.names(bTaxC)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(bTaxC)[rownames(bTaxC) == "Communications"] <- "Broadcasting.and.telecommunications"

bTaxC = bTaxC[!row.names(bTaxC)%in%c("Finance..insurance..and.real.estate"),]
bTaxC["Depository.institutions",] <- bTaxC["Depository.institutions",]+bTaxC["Nondepository.institutions",]
rownames(bTaxC)[rownames(bTaxC) == "Depository.institutions"] <- 'Banking.and.credit.agencies'
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Nondepository.institutions","Federal.Reserve.banks","Commercial.and.mutual.depository.institutions"),]

bTaxC["Insurance.carriers",] <- bTaxC["Insurance.carriers",]+bTaxC["Insurance.agents..brokers..and.service",]
rownames(bTaxC)[rownames(bTaxC) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Insurance.agents..brokers..and.service"),]
 
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
bTaxC = bTaxC[!row.names(bTaxC)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Other.services","Private.households"),]
bTaxC = bTaxC[!row.names(bTaxC)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]

#=========== bTaxD =============================================
bTaxD <- data.frame(bTaxD)#put in dataframe
bTaxD <- bTaxD[-(1:6),] #romve top rows
bTaxD <- head(bTaxD,-11) #remove botttom rows
bTaxD <- bTaxD[,-c(1,3)]#remove useless rows
bTaxD[1,1] <- "Industry" #replace title
bTaxD[,1] <- str_remove(bTaxD[,1],"\\\\[:digit:]\\\\")
 
names(bTaxD) <- bTaxD[1,]#set column names
bTaxD <- bTaxD[-1,]#remove column names rows
rownames(bTaxD) <- make.names(bTaxD[,1],unique=TRUE)#reset row labels
bTaxD <- bTaxD[,-1]#remove row names rows
bTaxD[] <- lapply(bTaxD, as.numeric)

bTaxD = bTaxD[-c(1:3)]

bTaxD = bTaxD[!row.names(bTaxD)%in%c("Support.activities.for.mining"),]

rownames(bTaxD)[rownames(bTaxD) == "Nonmetallic.mineral.products"] <- "Stone..clay..and.glass.products"

bTaxD = bTaxD[!row.names(bTaxD)%in%c("Wholesale.trade","Durable.goods.1","Nondurable.goods.1","Retail.trade","Motor.vehicle.and.parts.dealers","Food.and.beverage.stores","General.merchandise.stores","Other.retail"),]

bTaxD["Computer.and.electronic.products",] <- bTaxD["Computer.and.electronic.products",]+bTaxD["Electrical.equipment..appliances..and.components",]
rownames(bTaxD)[rownames(bTaxD) == "Computer.and.electronic.products"] <- 'Electric.and.electronic.equipment'
bTaxD = bTaxD[!row.names(bTaxD)%in%c("Electrical.equipment..appliances..and.components"),]

rownames(bTaxD)[rownames(bTaxD) == "Motor.vehicles..bodies.and.trailers..and.parts"] <- 'Motor.vehicles.and.equipment'
rownames(bTaxD)[rownames(bTaxD) == "Textile.mills.and.textile.product.mills"] <- 'Textile.mill.products'
rownames(bTaxD)[rownames(bTaxD) == "Paper.products"] <- 'Paper.and.allied.products'
rownames(bTaxD)[rownames(bTaxD) == "Printing.and.related.support.activities"] <- 'Printing.and.publishing'
rownames(bTaxD)[rownames(bTaxD) == "Chemical.products"] <- 'Chemicals.and.allied.products'
rownames(bTaxD)[rownames(bTaxD) == "Rail.transportation"] <- 'Railroad.transportation'
rownames(bTaxD)[rownames(bTaxD) == "Transit.and.ground.passenger.transportation"] <- 'Local.and.interurban.passenger.transit'
rownames(bTaxD)[rownames(bTaxD) == "Air.transportation"] <- 'Transportation.by.air'
rownames(bTaxD)[rownames(bTaxD) == "Utilities"] <- 'Electric..gas..and.sanitary.services'
rownames(bTaxD)[rownames(bTaxD) == "Federal.Reserve.banks..credit.intermediation..and.related.activities"] <- 'Banking.credit.agencies'
rownames(bTaxD)[rownames(bTaxD) == "Securities..commodity.contracts..and.investments"] <- 'Security.and.commodity.brokers'
rownames(bTaxD)[rownames(bTaxD) == "Amusements..gambling..and.recreation.industries"] <- 'Amusement.and.recreation.services'

bTaxD = bTaxD[!row.names(bTaxD)%in%c("Furniture.and.related.products","Transportation.and.warehousing"),]
bTaxD = bTaxD[!row.names(bTaxD)%in%c("Other.transportation.and.support.activities","Warehousing.and.storage","Information","Publishing.industries..includes.software.","Motion.picture.and.sound.recording.industries","Information.and.data.processing.services"),]
bTaxD = bTaxD[!row.names(bTaxD)%in%c("Funds..trusts..and.other.financial.vehicles","Real.estate.and.rental.and.leasing","Rental.and.leasing.services.and.lessors.of.intangible.assets"),]
bTaxD = bTaxD[!row.names(bTaxD)%in%c("Finance.and.insurance","Health.care.and.social.assistance"),]
bTaxD = bTaxD[!row.names(bTaxD)%in%c("Professional..scientific..and.technical.services","Computer.systems.design.and.related.services","Miscellaneous.professional..scientific..and.technical.services","Management.of.bTaxAnies.and.enterprises","Administrative.and.waste.management.services","Administrative.and.support.services","Waste.management.and.remediation.services","Social.assistance","Arts..entertainment..and.recreation","Performing.arts..spectator.sports..museums..and.related.activities","Accommodation.and.food.services","Accommodation","Food.services.and.drinking.places","Other.services..except.government"),]

bTaxD["Ambulatory.health.care.services",] <- bTaxD["Ambulatory.health.care.services",]+bTaxD["Hospitals.and.nursing.and.residential.care.facilities",]
rownames(bTaxD)[rownames(bTaxD) == "Ambulatory.health.care.services"] <- 'Health.services'
bTaxD = bTaxD[!row.names(bTaxD)%in%c("Hospitals.and.nursing.and.residential.care.facilities","Management.of.companies.and.enterprises"),]

bTaxD = bTaxD[c(1:6,8,7,9:nrow(bTaxD)),]
bTaxD = bTaxD[c(1:27,29,28,30:nrow(bTaxD)),]
bTaxD = bTaxD[c(1:30,32,31,33:nrow(bTaxD)),]
bTaxD = bTaxD[c(1:31,35,34,33,32,36:nrow(bTaxD)),]
bTaxD <- rbind(bTaxD[1:8,], bTaxD[10:37,], bTaxD[9,], bTaxD[38:nrow(bTaxD),])

bTaxD["Federal.Reserve.banks",] <- bTaxD["Federal.Reserve.banks",]+bTaxD["Credit.intermediation.and.related.activities",]
rownames(bTaxD)[rownames(bTaxD) == "Federal.Reserve.banks"] <- 'Banking.credit.agencies'
bTaxD = bTaxD[!row.names(bTaxD)%in%c("Credit.intermediation.and.related.activities"),]
bTaxD = bTaxD[c(1:41,45,44,42,43,46:nrow(bTaxD)),]

bTaxD = bTaxD[!row.names(bTaxD)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]
bTaxD <- head(bTaxD,-2) #remove botttom rows

#=========== TaxA =============================================
TaxA <- data.frame(TaxA)#put in dataframe
TaxA <- TaxA[-(1:6),] #romve top rows
TaxA <- head(TaxA,-6) #remove botttom rows
TaxA <- TaxA[,-c(1,3)]#remove useless rows
TaxA[1,1] <- "Industry" #replace title
TaxA[,1] <- str_remove(TaxA[,1],"\\\\[:digit:]\\\\")

names(TaxA) <- TaxA[1,]#set column names
TaxA <- TaxA[-1,]#remove column names rows
rownames(TaxA) <- make.names(TaxA[,1],unique=TRUE)#reset row labels
TaxA <- TaxA[,-1]#remove row names rows
TaxA[] <- lapply(TaxA, as.numeric)

rownames(TaxA)[rownames(TaxA) == 'Agricultural.services..forestry..and.fisheries'] <- 'Forestry..fishing..and.related.activities'

TaxA["Metal.mining",] <- TaxA["Metal.mining",]+TaxA["Anthracite.mining",]+TaxA["Bituminous.and.other.soft.coal.mining",]+TaxA["Nonmetallic.mining.and.quarrying",]
rownames(TaxA)[rownames(TaxA) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
rownames(TaxA)[rownames(TaxA) == 'Crude.petroleum.and.natural.gas'] <- 'Oil.and.gas.extraction'
TaxA = TaxA[!row.names(TaxA)%in%c("Anthracite.mining","Bituminous.and.other.soft.coal.mining","Nonmetallic.mining.and.quarrying"),]

rownames(TaxA)[rownames(TaxA) == 'Contract.construction'] <- 'Construction'

TaxA["Lumber.and.basic.timber.products",] <- TaxA["Lumber.and.basic.timber.products",]+TaxA["Furniture.and.finished.lumber.products",]
rownames(TaxA)[rownames(TaxA) == "Lumber.and.basic.timber.products"] <- 'Wood.products'
TaxA = TaxA[!row.names(TaxA)%in%c("Furniture.and.finished.lumber.products"),]

rownames(TaxA)[rownames(TaxA) == "Iron.and.steel.and.their.products..including.ordnance"] <- "Primary.metal.industries"
rownames(TaxA)[rownames(TaxA) == "Nonferrous.metals.and.their.products"] <- "Fabricated.metal.products"
rownames(TaxA)[rownames(TaxA) == "Machinery..except.electrical"] <- "Machinery"
rownames(TaxA)[rownames(TaxA) == "Miscellaneous.manufacturing..including.instruments.and.miscellaneous.plastic.products"] <- "Miscellaneous.manufacturing"

TaxA["Food.and.kindred.products",] <- TaxA["Food.and.kindred.products",]+TaxA["Tobacco.manufactures",]
rownames(TaxA)[rownames(TaxA) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
TaxA = TaxA[!row.names(TaxA)%in%c("Tobacco.manufactures"),]

TaxA["Apparel.and.other.textile.products",] <- TaxA["Apparel.and.other.textile.products",]+TaxA["Leather.and.leather.products",]
rownames(TaxA)[rownames(TaxA) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
TaxA = TaxA[!row.names(TaxA)%in%c("Leather.and.leather.products"),]

rownames(TaxA)[rownames(TaxA) == "Rubber.products"] <- 'Plastics.and.rubber.products'
TaxA = TaxA[!row.names(TaxA)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]

rownames(TaxA)[rownames(TaxA) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(TaxA)[rownames(TaxA) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

TaxA = TaxA[!row.names(TaxA)%in%c("Telephone.and.telegraph","Radio.and.television.broadcasting","Utilities..electric.and.gas","Local.utilities.and.public.services..n.e.c.","Wholesale.trade","Retail.trade.and.automobile.services"),]
rownames(TaxA)[rownames(TaxA) == "Communication"] <- "Broadcasting.and.telecommunications"

TaxA = TaxA[!row.names(TaxA)%in%c("Finance..insurance..and.real.estate"),]
TaxA["Banking",] <- TaxA["Banking",]+TaxA["Credit.agencies..other.than.banks..and.holding.and.other.investment.companies",]
rownames(TaxA)[rownames(TaxA) == "Banking"] <- 'Banking.and.credit.agencies'
TaxA = TaxA[!row.names(TaxA)%in%c("Credit.agencies..other.than.banks..and.holding.and.other.investment.companies"),]
rownames(TaxA)[rownames(TaxA) == "Security.and.commodity.brokers..and.services"] <- 'Security.and.commodity.brokers'

TaxA["Insurance.carriers",] <- TaxA["Insurance.carriers",]+TaxA["Insurance.agents..brokers..and.service",]
rownames(TaxA)[rownames(TaxA) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
TaxA = TaxA[!row.names(TaxA)%in%c("Insurance.agents..brokers..and.service"),]

TaxA = TaxA[!row.names(TaxA)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
TaxA = TaxA[!row.names(TaxA)%in%c("Membership.organizations","Miscellaneous.professional.services","Private.households"),]
rownames(TaxA)[rownames(TaxA) == "Educational.services..n.e.c."] <- 'Educational.services'

TaxA = TaxA[!row.names(TaxA)%in%c("General.government","Government.enterprises","General.government.1","Government.enterprises.1","Other.services","Social.services.and.membership.organizations"),]

#TaxA = TaxA[-ncol(TaxA)]

#=========== TaxB =============================================
TaxB <- data.frame(TaxB)#put in dataframe
TaxB <- TaxB[-(1:6),] #romve top rows
TaxB <- head(TaxB,-8) #remove botttom rows
TaxB <- TaxB[,-c(1,3)]#remove useless rows
TaxB[1,1] <- "Industry" #replace title
TaxB[,1] <- str_remove(TaxB[,1],"\\\\[:digit:]\\\\")

names(TaxB) <- TaxB[1,]#set column names
TaxB <- TaxB[-1,]#remove column names rows
rownames(TaxB) <- make.names(TaxB[,1],unique=TRUE)#reset row labels
TaxB <- TaxB[,-1]#remove row names rows
TaxB[] <- lapply(TaxB, as.numeric)

rownames(TaxB)[rownames(TaxB) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

TaxB["Metal.mining",] <- TaxB["Metal.mining",]+TaxB["Coal.mining",]+TaxB["Nonmetallic.minerals..except.fuels",]
rownames(TaxB)[rownames(TaxB) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
TaxB = TaxB[!row.names(TaxB)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]

TaxB["Lumber.and.wood.products",] <- TaxB["Lumber.and.wood.products",]+TaxB["Furniture.and.fixtures",]
rownames(TaxB)[rownames(TaxB) == "Lumber.and.wood.products"] <- 'Wood.products'
TaxB = TaxB[!row.names(TaxB)%in%c("Furniture.and.fixtures"),]

rownames(TaxB)[rownames(TaxB) == "Machinery..except.electrical"] <- "Machinery"
TaxB = TaxB[!row.names(TaxB)%in%c("Instruments.and.related.products"),]
rownames(TaxB)[rownames(TaxB) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

TaxB["Food.and.kindred.products",] <- TaxB["Food.and.kindred.products",]+TaxB["Tobacco.manufactures",]
rownames(TaxB)[rownames(TaxB) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
TaxB = TaxB[!row.names(TaxB)%in%c("Tobacco.manufactures"),]

TaxB["Apparel.and.other.textile.products",] <- TaxB["Apparel.and.other.textile.products",]+TaxB["Leather.and.leather.products",]
rownames(TaxB)[rownames(TaxB) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
TaxB = TaxB[!row.names(TaxB)%in%c("Leather.and.leather.products"),]

rownames(TaxB)[rownames(TaxB) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'

TaxB = TaxB[!row.names(TaxB)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(TaxB)[rownames(TaxB) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(TaxB)[rownames(TaxB) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

TaxB = TaxB[!row.names(TaxB)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(TaxB)[rownames(TaxB) == "Communication"] <- "Broadcasting.and.telecommunications"

TaxB = TaxB[!row.names(TaxB)%in%c("Finance..insurance..and.real.estate"),]
TaxB["Banking",] <- TaxB["Banking",]+TaxB["Credit.agencies.other.than.banks",]
rownames(TaxB)[rownames(TaxB) == "Banking"] <- 'Banking.and.credit.agencies'
TaxB = TaxB[!row.names(TaxB)%in%c("Commercial.and.mutual.banks","Federal.Reserve.banks","Credit.agencies.other.than.banks"),]

TaxB["Insurance.carriers",] <- TaxB["Insurance.carriers",]+TaxB["Insurance.agents..brokers..and.service",]
rownames(TaxB)[rownames(TaxB) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
TaxB = TaxB[!row.names(TaxB)%in%c("Insurance.agents..brokers..and.service"),]

TaxB = TaxB[!row.names(TaxB)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
TaxB = TaxB[!row.names(TaxB)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
TaxB = TaxB[!row.names(TaxB)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Miscellaneous.professional.services","Private.households"),]
TaxB = TaxB[!row.names(TaxB)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1","Other.services"),]

#=========== TaxC =============================================
TaxC <- data.frame(TaxC)#put in dataframe
TaxC <- TaxC[-(1:6),] #romve top rows
TaxC <- head(TaxC,-8) #remove botttom rows
TaxC <- TaxC[,-c(1,3,4)]#remove useless rows
TaxC[1,1] <- "Industry" #replace title
TaxC[,1] <- str_remove(TaxC[,1],"\\\\[:digit:]\\\\")
 
names(TaxC) <- TaxC[1,]#set column names
TaxC <- TaxC[-1,]#remove column names rows
rownames(TaxC) <- make.names(TaxC[,1],unique=TRUE)#reset row labels
TaxC <- TaxC[,-1]#remove row names rows
TaxC[] <- lapply(TaxC, as.numeric)
 
rownames(TaxC)[rownames(TaxC) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

TaxC["Metal.mining",] <- TaxC["Metal.mining",]+TaxC["Coal.mining",]+TaxC["Nonmetallic.minerals..except.fuels",]
rownames(TaxC)[rownames(TaxC) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
TaxC = TaxC[!row.names(TaxC)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]
 
TaxC["Lumber.and.wood.products",] <- TaxC["Lumber.and.wood.products",]+TaxC["Furniture.and.fixtures",]
rownames(TaxC)[rownames(TaxC) == "Lumber.and.wood.products"] <- 'Wood.products'
TaxC = TaxC[!row.names(TaxC)%in%c("Furniture.and.fixtures"),]

rownames(TaxC)[rownames(TaxC) == "Industrial.machinery.and.equipment"] <- "Machinery"
rownames(TaxC)[rownames(TaxC) == "Electronic.and.other.electric.equipment"] <- "Electric.and.electronic.equipment"
TaxC = TaxC[!row.names(TaxC)%in%c("Furniture.and.fixtures"),]
rownames(TaxC)[rownames(TaxC) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

TaxC["Food.and.kindred.products",] <- TaxC["Food.and.kindred.products",]+TaxC["Tobacco.products",]
rownames(TaxC)[rownames(TaxC) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
TaxC = TaxC[!row.names(TaxC)%in%c("Instruments.and.related.products","Tobacco.products"),]
 
TaxC["Apparel.and.other.textile.products",] <- TaxC["Apparel.and.other.textile.products",]+TaxC["Leather.and.leather.products",]
rownames(TaxC)[rownames(TaxC) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
TaxC = TaxC[!row.names(TaxC)%in%c("Leather.and.leather.products"),]

rownames(TaxC)[rownames(TaxC) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'
 
TaxC = TaxC[!row.names(TaxC)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(TaxC)[rownames(TaxC) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(TaxC)[rownames(TaxC) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

TaxC = TaxC[!row.names(TaxC)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(TaxC)[rownames(TaxC) == "Communications"] <- "Broadcasting.and.telecommunications"

TaxC = TaxC[!row.names(TaxC)%in%c("Finance..insurance..and.real.estate"),]
TaxC["Depository.institutions",] <- TaxC["Depository.institutions",]+TaxC["Federal.Reserve.banks",]
rownames(TaxC)[rownames(TaxC) == "Depository.institutions"] <- 'Banking.and.credit.agencies'
TaxC = TaxC[!row.names(TaxC)%in%c("Nondepository.institutions","Federal.Reserve.banks","Commercial.and.mutual.depository.institutions"),]

TaxC["Insurance.carriers",] <- TaxC["Insurance.carriers",]+TaxC["Insurance.agents..brokers..and.service",]
rownames(TaxC)[rownames(TaxC) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
TaxC = TaxC[!row.names(TaxC)%in%c("Insurance.agents..brokers..and.service"),]
 
TaxC = TaxC[!row.names(TaxC)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
TaxC = TaxC[!row.names(TaxC)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
TaxC = TaxC[!row.names(TaxC)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Other.services","Private.households"),]
TaxC = TaxC[!row.names(TaxC)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]

#=========== TaxD =============================================
TaxD <- data.frame(TaxD)#put in dataframe
TaxD <- TaxD[-(1:6),] #romve top rows
TaxD <- head(TaxD,-12) #remove botttom rows
TaxD <- TaxD[,-c(1,3)]#remove useless rows
TaxD[1,1] <- "Industry" #replace title
TaxD[,1] <- str_remove(TaxD[,1],"\\\\[:digit:]\\\\")
 
names(TaxD) <- TaxD[1,]#set column names
TaxD <- TaxD[-1,]#remove column names rows
rownames(TaxD) <- make.names(TaxD[,1],unique=TRUE)#reset row labels
TaxD <- TaxD[,-1]#remove row names rows
TaxD[] <- lapply(TaxD, as.numeric)

TaxD = TaxD[-c(1:3)]

TaxD = TaxD[!row.names(TaxD)%in%c("Support.activities.for.mining"),]

rownames(TaxD)[rownames(TaxD) == "Nonmetallic.mineral.products"] <- "Stone..clay..and.glass.products"

TaxD = TaxD[!row.names(TaxD)%in%c("Wholesale.trade","Durable.goods.1","Nondurable.goods.1","Retail.trade","Motor.vehicle.and.parts.dealers","Food.and.beverage.stores","General.merchandise.stores","Other.retail"),]

TaxD["Computer.and.electronic.products",] <- TaxD["Computer.and.electronic.products",]+TaxD["Electrical.equipment..appliances..and.components",]
rownames(TaxD)[rownames(TaxD) == "Computer.and.electronic.products"] <- 'Electric.and.electronic.equipment'
TaxD = TaxD[!row.names(TaxD)%in%c("Electrical.equipment..appliances..and.components"),]

rownames(TaxD)[rownames(TaxD) == "Motor.vehicles..bodies.and.trailers..and.parts"] <- 'Motor.vehicles.and.equipment'
rownames(TaxD)[rownames(TaxD) == "Textile.mills.and.textile.product.mills"] <- 'Textile.mill.products'
rownames(TaxD)[rownames(TaxD) == "Paper.products"] <- 'Paper.and.allied.products'
rownames(TaxD)[rownames(TaxD) == "Printing.and.related.support.activities"] <- 'Printing.and.publishing'
rownames(TaxD)[rownames(TaxD) == "Chemical.products"] <- 'Chemicals.and.allied.products'
rownames(TaxD)[rownames(TaxD) == "Rail.transportation"] <- 'Railroad.transportation'
rownames(TaxD)[rownames(TaxD) == "Transit.and.ground.passenger.transportation"] <- 'Local.and.interurban.passenger.transit'
rownames(TaxD)[rownames(TaxD) == "Air.transportation"] <- 'Transportation.by.air'
rownames(TaxD)[rownames(TaxD) == "Utilities"] <- 'Electric..gas..and.sanitary.services'
rownames(TaxD)[rownames(TaxD) == "Federal.Reserve.banks..credit.intermediation..and.related.activities"] <- 'Banking.credit.agencies'
rownames(TaxD)[rownames(TaxD) == "Securities..commodity.contracts..and.investments"] <- 'Security.and.commodity.brokers'
rownames(TaxD)[rownames(TaxD) == "Amusements..gambling..and.recreation.industries"] <- 'Amusement.and.recreation.services'

TaxD = TaxD[!row.names(TaxD)%in%c("Furniture.and.related.products","Transportation.and.warehousing"),]
TaxD = TaxD[!row.names(TaxD)%in%c("Other.transportation.and.support.activities","Warehousing.and.storage","Information","Publishing.industries..includes.software.","Motion.picture.and.sound.recording.industries","Information.and.data.processing.services"),]
TaxD = TaxD[!row.names(TaxD)%in%c("Funds..trusts..and.other.financial.vehicles","Real.estate.and.rental.and.leasing","Rental.and.leasing.services.and.lessors.of.intangible.assets"),]
TaxD = TaxD[!row.names(TaxD)%in%c("Finance.and.insurance","Health.care.and.social.assistance"),]
TaxD = TaxD[!row.names(TaxD)%in%c("Professional..scientific..and.technical.services","Computer.systems.design.and.related.services","Miscellaneous.professional..scientific..and.technical.services","Management.of.TaxAnies.and.enterprises","Administrative.and.waste.management.services","Administrative.and.support.services","Waste.management.and.remediation.services","Social.assistance","Arts..entertainment..and.recreation","Performing.arts..spectator.sports..museums..and.related.activities","Accommodation.and.food.services","Accommodation","Food.services.and.drinking.places","Other.services..except.government"),]

TaxD["Ambulatory.health.care.services",] <- TaxD["Hospitals.and.nursing.and.residential.care.facilities",]+TaxD["Ambulatory.health.care.services",]
rownames(TaxD)[rownames(TaxD) == "Ambulatory.health.care.services"] <- 'Health.services'
TaxD = TaxD[!row.names(TaxD)%in%c("Ambulatory.health.care.services","Hospitals.and.nursing.and.residential.care.facilities","Management.of.companies.and.enterprises"),]

TaxD = TaxD[c(1:6,8,7,9:nrow(TaxD)),]
TaxD = TaxD[c(1:27,29,28,30:nrow(TaxD)),]
TaxD = TaxD[c(1:30,32,31,33:nrow(TaxD)),]
TaxD = TaxD[c(1:31,35,34,33,32,36:nrow(TaxD)),]
TaxD <- rbind(TaxD[1:8,], TaxD[10:37,], TaxD[9,], TaxD[38:nrow(TaxD),])

rownames(TaxD)[rownames(TaxD) == "Credit.intermediation.and.related.activities"] <- 'Banking.credit.agencies'
TaxD = TaxD[!row.names(TaxD)%in%c("Federal.Reserve.banks"),]

TaxD = TaxD[c(1:41,45,44,42,43,46:nrow(TaxD)),]

TaxD = TaxD[!row.names(TaxD)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]
TaxD <- head(TaxD,-2) #remove botttom rows
#=========== Correct col names ============
destroyX = function(es) {
        f = es
        for (col in c(1:ncol(f))){ #for each column in dataframe
                if (startsWith(colnames(f)[col], "X") == TRUE)  { #if starts with 'X' ..
                        colnames(f)[col] <- substr(colnames(f)[col], 2, 100) #get rid of it
                }
        }
        assign(deparse(substitute(es)), f, inherits = TRUE) #assign corrected data to original name
}
#=========== Set nan to zero ============
is.nan.data.frame <- function(x) do.call(cbind, lapply(x, is.nan))
#=========== Compensation combined =============================================
wage = cbind(wageA,wageB,wageC,wageD)
wage = data.frame(t(t(wage)*CPImulti))
wage = destroyX(wage)
wage = wage[!row.names(wage)%in%c("Government","Federal","State.and.local","Private.industries","Health.services","Legal.services","Educational.services"),]
#=========== undistrubuted =============================================
und = cbind(undA,undB,undC,undD)
und = data.frame(t(t(und)*CPImulti))
und = destroyX(und)
und = und[!row.names(und)%in%c("Health.services","Legal.services","Educational.services"),]
#=========== dividend =============================================
div = cbind(divA,divB,divC,divD)
div = data.frame(t(t(div)*CPImulti))
div = destroyX(div)
div = div[!row.names(div)%in%c("Health.services","Legal.services","Educational.services"),]
#=========== Before Tax =============================================
bTax = cbind(bTaxA,bTaxB,bTaxC,bTaxD)
bTax = data.frame(t(t(bTax)*CPImulti))
bTax = destroyX(bTax)
bTax = bTax[!row.names(bTax)%in%c("Health.services","Legal.services","Educational.services"),]
#=========== Tax Amount =============================================
aTax = cbind(TaxA,TaxB,TaxC,TaxD)
aTax = data.frame(t(t(aTax)*CPImulti))
aTax = destroyX(aTax)
aTax = aTax[!row.names(aTax)%in%c("Health.services","Legal.services","Educational.services"),]
#=========== Calculate Tax percentage ===========================================
taxRate <- abs((bTax-aTax)/bTax)
taxRate[is.nan(taxRate)] <- 0

taxRate = data.frame(t(taxRate[ ,!(names(taxRate) %in% c("2017","2018","1931"))]))
und = data.frame(t(und[ ,!(names(und) %in% c("2017","2018","1931"))]))
div = data.frame(t(div[ ,!(names(div) %in% c("2017","2018","1931"))]))
wage = data.frame(t(wage[ ,!(names(wage) %in% c("2017","2018","1931"))]))

wage <- wage[-(1:76),] #romve top rows
wage <- as.list(as.data.frame(t(wage)))
wage <- do.call(c, wage)

div <- div[-(1:76),] #romve top rows
div <- as.list(as.data.frame(t(div)))
div <- do.call(c, div)

und <- und[-(1:76),] #romve top rows
und <- as.list(as.data.frame(t(und)))
und <- do.call(c, und)

taxRate <- taxRate[-(1:76),] #romve top rows
taxRate <- as.list(as.data.frame(t(taxRate)))
taxRate <- do.call(c, taxRate)

combinedDF = do.call(rbind, Map(data.frame, Dividends=div, TaxRate=taxRate, UndistributedProfits=und, Wages=wage))
combinedDF = na.omit(combinedDF)

flds <- createFolds(combinedDF, k = 10, list = TRUE, returnTrain = FALSE)
names(flds)[1] <- "train"

reg.tree <- rpart(Dividends ~ TaxRate + UndistributedProfits + Wages, data = combinedDF[flds$train,])
rpart.plot(reg.tree)
reg.tree$variable.importance
