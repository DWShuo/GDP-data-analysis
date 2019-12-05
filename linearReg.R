library("readxl")
library("stringr")
library("ggplot2")
library("blscrapeR")
#set_bls_key("0ccfedf314414057a81e50d7ab69a64a")

#get cpi data to adjust for inflation
#cpi <- bls_api("CUUR0000SA0",startyear = 1929, endyear = 2018)
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
compA <- read_excel("Section6.xlsx",sheet ="T60200A-A")
compB <- read_excel("Section6.xlsx",sheet ="T60200B-A")
compC <- read_excel("Section6.xlsx",sheet ="T60200C-A")
compD <- read_excel("Section6.xlsx",sheet ="T60200D-A")

bTaxA <- read_excel("Section6.xlsx",sheet ="T61700A-A")
bTaxB <- read_excel("Section6.xlsx",sheet ="T61700B-A")
bTaxC <- read_excel("Section6.xlsx",sheet ="T61700C-A")
bTaxD <- read_excel("Section6.xlsx",sheet ="T61700D-A")

TaxA <- read_excel("Section6.xlsx",sheet ="T61900A-A")
TaxB <- read_excel("Section6.xlsx",sheet ="T61900B-A")
TaxC <- read_excel("Section6.xlsx",sheet ="T61900C-A")
TaxD <- read_excel("Section6.xlsx",sheet ="T61900D-A")

empA <- read_excel("Section6.xlsx",sheet ="T60400A-A")
empB <- read_excel("Section6.xlsx",sheet ="T60400B-A")
empC <- read_excel("Section6.xlsx",sheet ="T60400C-A")
empD <- read_excel("Section6.xlsx",sheet ="T60400D-A")

#=========== COMPA =============================================
compA <- data.frame(compA)#put in dataframe
compA <- compA[-(1:6),] #romve top rows
compA <- head(compA,-11) #remove botttom rows
compA <- compA[,-c(1,3)]#remove useless rows
compA[1,1] <- "Industry" #replace title
compA[,1] <- str_remove(compA[,1],"\\\\[:digit:]\\\\")

names(compA) <- compA[1,]#set column names
compA <- compA[-1,]#remove column names rows
rownames(compA) <- make.names(compA[,1],unique=TRUE)#reset row labels
compA <- compA[,-1]#remove row names rows
compA[] <- lapply(compA, as.numeric)

rownames(compA)[rownames(compA) == 'Agricultural.services..forestry..and.fisheries'] <- 'Forestry..fishing..and.related.activities'

compA["Metal.mining",] <- compA["Metal.mining",]+compA["Anthracite.mining",]+compA["Bituminous.and.other.soft.coal.mining",]+compA["Nonmetallic.mining.and.quarrying",]
rownames(compA)[rownames(compA) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
rownames(compA)[rownames(compA) == 'Crude.petroleum.and.natural.gas'] <- 'Oil.and.gas.extraction'
compA = compA[!row.names(compA)%in%c("Anthracite.mining","Bituminous.and.other.soft.coal.mining","Nonmetallic.mining.and.quarrying"),]

rownames(compA)[rownames(compA) == 'Contract.construction'] <- 'Construction'

compA["Lumber.and.basic.timber.products",] <- compA["Lumber.and.basic.timber.products",]+compA["Furniture.and.finished.lumber.products",]
rownames(compA)[rownames(compA) == "Lumber.and.basic.timber.products"] <- 'Wood.products'
compA = compA[!row.names(compA)%in%c("Furniture.and.finished.lumber.products"),]

rownames(compA)[rownames(compA) == "Iron.and.steel.and.their.products..including.ordnance"] <- "Primary.metal.industries"
rownames(compA)[rownames(compA) == "Nonferrous.metals.and.their.products"] <- "Fabricated.metal.products"
rownames(compA)[rownames(compA) == "Machinery..except.electrical"] <- "Machinery"
rownames(compA)[rownames(compA) == "Miscellaneous.manufacturing..including.instruments.and.miscellaneous.plastic.products"] <- "Miscellaneous.manufacturing"

compA["Food.and.kindred.products",] <- compA["Food.and.kindred.products",]+compA["Tobacco.manufactures",]
rownames(compA)[rownames(compA) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
compA = compA[!row.names(compA)%in%c("Tobacco.manufactures"),]

compA["Apparel.and.other.textile.products",] <- compA["Apparel.and.other.textile.products",]+compA["Leather.and.leather.products",]
rownames(compA)[rownames(compA) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
compA = compA[!row.names(compA)%in%c("Leather.and.leather.products"),]

rownames(compA)[rownames(compA) == "Rubber.products"] <- 'Plastics.and.rubber.products'
compA = compA[!row.names(compA)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]

rownames(compA)[rownames(compA) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(compA)[rownames(compA) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

compA = compA[!row.names(compA)%in%c("Telephone.and.telegraph","Radio.and.television.broadcasting","Utilities..electric.and.gas","Local.utilities.and.public.services..n.e.c.","Wholesale.trade","Retail.trade.and.automobile.services"),]
rownames(compA)[rownames(compA) == "Communication"] <- "Broadcasting.and.telecommunications"

compA = compA[!row.names(compA)%in%c("Finance..insurance..and.real.estate"),]
compA["Banking",] <- compA["Banking",]+compA["Credit.agencies..other.than.banks..and.holding.and.other.investment.companies",]
rownames(compA)[rownames(compA) == "Banking"] <- 'Banking.and.credit.agencies'
compA = compA[!row.names(compA)%in%c("Credit.agencies..other.than.banks..and.holding.and.other.investment.companies"),]
rownames(compA)[rownames(compA) == "Security.and.commodity.brokers..and.services"] <- 'Security.and.commodity.brokers'

compA["Insurance.carriers",] <- compA["Insurance.carriers",]+compA["Insurance.agents..brokers..and.service",]
rownames(compA)[rownames(compA) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
compA = compA[!row.names(compA)%in%c("Insurance.agents..brokers..and.service"),]

compA = compA[!row.names(compA)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
compA = compA[!row.names(compA)%in%c("Membership.organizations","Miscellaneous.professional.services","Private.households"),]
rownames(compA)[rownames(compA) == "Educational.services..n.e.c."] <- 'Educational.services'

compA = compA[!row.names(compA)%in%c("General.government","Government.enterprises","General.government.1","Government.enterprises.1"),]

compA = compA[-ncol(compA)]

#=========== COMPB =============================================
compB <- data.frame(compB)#put in dataframe
compB <- compB[-(1:6),] #romve top rows
compB <- head(compB,-10) #remove botttom rows
compB <- compB[,-c(1,3)]#remove useless rows
compB[1,1] <- "Industry" #replace title
compB[,1] <- str_remove(compB[,1],"\\\\[:digit:]\\\\")

names(compB) <- compB[1,]#set column names
compB <- compB[-1,]#remove column names rows
rownames(compB) <- make.names(compB[,1],unique=TRUE)#reset row labels
compB <- compB[,-1]#remove row names rows
compB[] <- lapply(compB, as.numeric)

rownames(compB)[rownames(compB) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

compB["Metal.mining",] <- compB["Metal.mining",]+compB["Coal.mining",]+compB["Nonmetallic.minerals..except.fuels",]
rownames(compB)[rownames(compB) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
compB = compB[!row.names(compB)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]

compB["Lumber.and.wood.products",] <- compB["Lumber.and.wood.products",]+compB["Furniture.and.fixtures",]
rownames(compB)[rownames(compB) == "Lumber.and.wood.products"] <- 'Wood.products'
compB = compB[!row.names(compB)%in%c("Furniture.and.fixtures"),]

rownames(compB)[rownames(compB) == "Machinery..except.electrical"] <- "Machinery"
compB = compB[!row.names(compB)%in%c("Instruments.and.related.products"),]
rownames(compB)[rownames(compB) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

compB["Food.and.kindred.products",] <- compB["Food.and.kindred.products",]+compB["Tobacco.manufactures",]
rownames(compB)[rownames(compB) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
compB = compB[!row.names(compB)%in%c("Tobacco.manufactures"),]

compB["Apparel.and.other.textile.products",] <- compB["Apparel.and.other.textile.products",]+compB["Leather.and.leather.products",]
rownames(compB)[rownames(compB) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
compB = compB[!row.names(compB)%in%c("Leather.and.leather.products"),]

rownames(compB)[rownames(compB) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'

compB = compB[!row.names(compB)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(compB)[rownames(compB) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(compB)[rownames(compB) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

compB = compB[!row.names(compB)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(compB)[rownames(compB) == "Communication"] <- "Broadcasting.and.telecommunications"

compB = compB[!row.names(compB)%in%c("Finance..insurance..and.real.estate"),]
compB["Banking",] <- compB["Banking",]+compB["Credit.agencies.other.than.banks",]
rownames(compB)[rownames(compB) == "Banking"] <- 'Banking.and.credit.agencies'
compB = compB[!row.names(compB)%in%c("Credit.agencies.other.than.banks"),]

compB["Insurance.carriers",] <- compB["Insurance.carriers",]+compB["Insurance.agents..brokers..and.service",]
rownames(compB)[rownames(compB) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
compB = compB[!row.names(compB)%in%c("Insurance.agents..brokers..and.service"),]

compB = compB[!row.names(compB)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
compB = compB[!row.names(compB)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
compB = compB[!row.names(compB)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Miscellaneous.professional.services","Private.households"),]
compB = compB[!row.names(compB)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]

#=========== COMPC =============================================
compC <- data.frame(compC)#put in dataframe
compC <- compC[-(1:6),] #romve top rows
compC <- head(compC,-13) #remove botttom rows
compC <- compC[,-c(1,3,4)]#remove useless rows
compC[1,1] <- "Industry" #replace title
compC[,1] <- str_remove(compC[,1],"\\\\[:digit:]\\\\")
 
names(compC) <- compC[1,]#set column names
compC <- compC[-1,]#remove column names rows
rownames(compC) <- make.names(compC[,1],unique=TRUE)#reset row labels
compC <- compC[,-1]#remove row names rows
compC[] <- lapply(compC, as.numeric)
 
rownames(compC)[rownames(compC) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

compC["Metal.mining",] <- compC["Metal.mining",]+compC["Coal.mining",]+compC["Nonmetallic.minerals..except.fuels",]
rownames(compC)[rownames(compC) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
compC = compC[!row.names(compC)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]
 
compC["Lumber.and.wood.products",] <- compC["Lumber.and.wood.products",]+compC["Furniture.and.fixtures",]
rownames(compC)[rownames(compC) == "Lumber.and.wood.products"] <- 'Wood.products'
compC = compC[!row.names(compC)%in%c("Furniture.and.fixtures"),]

rownames(compC)[rownames(compC) == "Industrial.machinery.and.equipment"] <- "Machinery"
rownames(compC)[rownames(compC) == "Electronic.and.other.electric.equipment"] <- "Electric.and.electronic.equipment"
compC = compC[!row.names(compC)%in%c("Furniture.and.fixtures"),]
rownames(compC)[rownames(compC) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

compC["Food.and.kindred.products",] <- compC["Food.and.kindred.products",]+compC["Tobacco.products",]
rownames(compC)[rownames(compC) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
compC = compC[!row.names(compC)%in%c("Instruments.and.related.products","Tobacco.products"),]
 
compC["Apparel.and.other.textile.products",] <- compC["Apparel.and.other.textile.products",]+compC["Leather.and.leather.products",]
rownames(compC)[rownames(compC) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
compC = compC[!row.names(compC)%in%c("Leather.and.leather.products"),]

rownames(compC)[rownames(compC) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'
 
compC = compC[!row.names(compC)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(compC)[rownames(compC) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(compC)[rownames(compC) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

compC = compC[!row.names(compC)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(compC)[rownames(compC) == "Communications"] <- "Broadcasting.and.telecommunications"

compC = compC[!row.names(compC)%in%c("Finance..insurance..and.real.estate"),]
compC["Depository.institutions",] <- compC["Depository.institutions",]+compC["Nondepository.institutions",]
rownames(compC)[rownames(compC) == "Depository.institutions"] <- 'Banking.and.credit.agencies'
compC = compC[!row.names(compC)%in%c("Nondepository.institutions"),]

compC["Insurance.carriers",] <- compC["Insurance.carriers",]+compC["Insurance.agents..brokers..and.service",]
rownames(compC)[rownames(compC) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
compC = compC[!row.names(compC)%in%c("Insurance.agents..brokers..and.service"),]
 
compC = compC[!row.names(compC)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
compC = compC[!row.names(compC)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
compC = compC[!row.names(compC)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Other.services","Private.households"),]
compC = compC[!row.names(compC)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]

#=========== COMPD =============================================
compD <- data.frame(compD)#put in dataframe
compD <- compD[-(1:6),] #romve top rows
compD <- head(compD,-14) #remove botttom rows
compD <- compD[,-c(1,3)]#remove useless rows
compD[1,1] <- "Industry" #replace title
compD[,1] <- str_remove(compD[,1],"\\\\[:digit:]\\\\")
 
names(compD) <- compD[1,]#set column names
compD <- compD[-1,]#remove column names rows
rownames(compD) <- make.names(compD[,1],unique=TRUE)#reset row labels
compD <- compD[,-1]#remove row names rows
compD[] <- lapply(compD, as.numeric)

compD = compD[-c(1:3)]

compD = compD[!row.names(compD)%in%c("Support.activities.for.mining"),]

rownames(compD)[rownames(compD) == "Nonmetallic.mineral.products"] <- "Stone..clay..and.glass.products"

compD = compD[!row.names(compD)%in%c("Wholesale.trade","Durable.goods.1","Nondurable.goods.1","Retail.trade","Motor.vehicle.and.parts.dealers","Food.and.beverage.stores","General.merchandise.stores","Other.retail"),]

compD["Computer.and.electronic.products",] <- compD["Computer.and.electronic.products",]+compD["Electrical.equipment..appliances..and.components",]
rownames(compD)[rownames(compD) == "Computer.and.electronic.products"] <- 'Electric.and.electronic.equipment'
compD = compD[!row.names(compD)%in%c("Electrical.equipment..appliances..and.components"),]

rownames(compD)[rownames(compD) == "Motor.vehicles..bodies.and.trailers..and.parts"] <- 'Motor.vehicles.and.equipment'
rownames(compD)[rownames(compD) == "Textile.mills.and.textile.product.mills"] <- 'Textile.mill.products'
rownames(compD)[rownames(compD) == "Paper.products"] <- 'Paper.and.allied.products'
rownames(compD)[rownames(compD) == "Printing.and.related.support.activities"] <- 'Printing.and.publishing'
rownames(compD)[rownames(compD) == "Chemical.products"] <- 'Chemicals.and.allied.products'
rownames(compD)[rownames(compD) == "Rail.transportation"] <- 'Railroad.transportation'
rownames(compD)[rownames(compD) == "Transit.and.ground.passenger.transportation"] <- 'Local.and.interurban.passenger.transit'
rownames(compD)[rownames(compD) == "Air.transportation"] <- 'Transportation.by.air'
rownames(compD)[rownames(compD) == "Utilities"] <- 'Electric..gas..and.sanitary.services'
rownames(compD)[rownames(compD) == "Federal.Reserve.banks..credit.intermediation..and.related.activities"] <- 'Banking.credit.agencies'
rownames(compD)[rownames(compD) == "Securities..commodity.contracts..and.investments"] <- 'Security.and.commodity.brokers'
rownames(compD)[rownames(compD) == "Amusements..gambling..and.recreation.industries"] <- 'Amusement.and.recreation.services'

compD = compD[!row.names(compD)%in%c("Furniture.and.related.products","Transportation.and.warehousing"),]
compD = compD[!row.names(compD)%in%c("Other.transportation.and.support.activities","Warehousing.and.storage","Information","Publishing.industries..includes.software.","Motion.picture.and.sound.recording.industries","Information.and.data.processing.services"),]
compD = compD[!row.names(compD)%in%c("Funds..trusts..and.other.financial.vehicles","Real.estate.and.rental.and.leasing","Rental.and.leasing.services.and.lessors.of.intangible.assets"),]
compD = compD[!row.names(compD)%in%c("Finance.and.insurance","Health.care.and.social.assistance"),]
compD = compD[!row.names(compD)%in%c("Professional..scientific..and.technical.services","Computer.systems.design.and.related.services","Miscellaneous.professional..scientific..and.technical.services","Management.of.companies.and.enterprises","Administrative.and.waste.management.services","Administrative.and.support.services","Waste.management.and.remediation.services","Social.assistance","Arts..entertainment..and.recreation","Performing.arts..spectator.sports..museums..and.related.activities","Accommodation.and.food.services","Accommodation","Food.services.and.drinking.places","Other.services..except.government"),]

compD["Hospitals",] <- compD["Hospitals",]+compD["Ambulatory.health.care.services",]+compD["Nursing.and.residential.care.facilities",]
rownames(compD)[rownames(compD) == "Hospitals"] <- 'Health.services'
compD = compD[!row.names(compD)%in%c("Ambulatory.health.care.services","Nursing.and.residential.care.facilities"),]

compD = compD[c(1:7,9,8,10:nrow(compD)),]
compD = compD[c(1:28,30,29,31:nrow(compD)),]
compD = compD[c(1:31,33,32,34:nrow(compD)),]
compD = compD[c(1:32,36,35,34,33,37:nrow(compD)),]
compD <- rbind(compD[1:9,], compD[11:38,], compD[10,], compD[39:nrow(compD),])
compD = compD[c(1:42,46,45,43,44,47:nrow(compD)),]

compD = compD[!row.names(compD)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]

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

#=========== empA =============================================
empA <- data.frame(empA)#put in dataframe
empA <- empA[-(1:6),] #romve top rows
empA <- head(empA,-9) #remove botttom rows
empA <- empA[,-c(1,3)]#remove useless rows
empA[1,1] <- "Industry" #replace title
empA[,1] <- str_remove(empA[,1],"\\\\[:digit:]\\\\")

names(empA) <- empA[1,]#set column names
empA <- empA[-1,]#remove column names rows
rownames(empA) <- make.names(empA[,1],unique=TRUE)#reset row labels
empA <- empA[,-1]#remove row names rows
empA[] <- lapply(empA, as.numeric)
empA[is.na(empA)] <- 0

rownames(empA)[rownames(empA) == 'Agricultural.services..forestry..and.fisheries'] <- 'Forestry..fishing..and.related.activities'

empA["Metal.mining",] <- empA["Metal.mining",]+empA["Anthracite.mining",]+empA["Bituminous.and.other.soft.coal.mining",]+empA["Nonmetallic.mining.and.quarrying",]
rownames(empA)[rownames(empA) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
rownames(empA)[rownames(empA) == 'Crude.petroleum.and.natural.gas'] <- 'Oil.and.gas.extraction'
empA = empA[!row.names(empA)%in%c("Anthracite.mining","Bituminous.and.other.soft.coal.mining","Nonmetallic.mining.and.quarrying"),]

rownames(empA)[rownames(empA) == 'Contract.construction'] <- 'Construction'

empA["Lumber.and.basic.timber.products",] <- empA["Lumber.and.basic.timber.products",]+empA["Furniture.and.finished.lumber.products",]
rownames(empA)[rownames(empA) == "Lumber.and.basic.timber.products"] <- 'Wood.products'
empA = empA[!row.names(empA)%in%c("Furniture.and.finished.lumber.products"),]

rownames(empA)[rownames(empA) == "Iron.and.steel.and.their.products..including.ordnance"] <- "Primary.metal.industries"
rownames(empA)[rownames(empA) == "Nonferrous.metals.and.their.products"] <- "Fabricated.metal.products"
rownames(empA)[rownames(empA) == "Machinery..except.electrical"] <- "Machinery"
rownames(empA)[rownames(empA) == "Miscellaneous.manufacturing..including.instruments.and.miscellaneous.plastic.products"] <- "Miscellaneous.manufacturing"

empA["Food.and.kindred.products",] <- empA["Food.and.kindred.products",]+empA["Tobacco.manufactures",]
rownames(empA)[rownames(empA) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
empA = empA[!row.names(empA)%in%c("Tobacco.manufactures"),]

empA["Apparel.and.other.textile.products",] <- empA["Apparel.and.other.textile.products",]+empA["Leather.and.leather.products",]
rownames(empA)[rownames(empA) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
empA = empA[!row.names(empA)%in%c("Leather.and.leather.products"),]

rownames(empA)[rownames(empA) == "Rubber.products"] <- 'Plastics.and.rubber.products'
empA = empA[!row.names(empA)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]

rownames(empA)[rownames(empA) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(empA)[rownames(empA) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

empA = empA[!row.names(empA)%in%c("Telephone.and.telegraph","Radio.and.television.broadcasting","Utilities..electric.and.gas","Local.utilities.and.public.services..n.e.c.","Wholesale.trade","Retail.trade.and.automobile.services"),]
rownames(empA)[rownames(empA) == "Communication"] <- "Broadcasting.and.telecommunications"

empA = empA[!row.names(empA)%in%c("Finance..insurance..and.real.estate"),]
empA["Banking",] <- empA["Banking",]+empA["Credit.agencies..other.than.banks..and.holding.and.other.investment.companies",]
rownames(empA)[rownames(empA) == "Banking"] <- 'Banking.and.credit.agencies'
empA = empA[!row.names(empA)%in%c("Credit.agencies..other.than.banks..and.holding.and.other.investment.companies"),]
rownames(empA)[rownames(empA) == "Security.and.commodity.brokers..and.services"] <- 'Security.and.commodity.brokers'

empA["Insurance.carriers",] <- empA["Insurance.carriers",]+empA["Insurance.agents..brokers..and.service",]
rownames(empA)[rownames(empA) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
empA = empA[!row.names(empA)%in%c("Insurance.agents..brokers..and.service"),]

empA = empA[!row.names(empA)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
empA = empA[!row.names(empA)%in%c("Membership.organizations","Miscellaneous.professional.services","Private.households"),]
rownames(empA)[rownames(empA) == "Educational.services..n.e.c."] <- 'Educational.services'

empA = empA[!row.names(empA)%in%c("General.government","Government.enterprises","General.government.1","Government.enterprises.1","Other.services"),]

empA = empA[!row.names(empA)%in%c("Private.industries"),]

empA = empA[c(1:(nrow(empA)-12)), ]
empA = empA[-ncol(empA)]

#=========== empB =============================================
empB <- data.frame(empB)#put in dataframe
empB <- empB[-(1:6),] #romve top rows
empB <- head(empB,-6) #remove botttom rows
empB <- empB[,-c(1,3)]#remove useless rows
empB[1,1] <- "Industry" #replace title
empB[,1] <- str_remove(empB[,1],"\\\\[:digit:]\\\\")

names(empB) <- empB[1,]#set column names
empB <- empB[-1,]#remove column names rows
rownames(empB) <- make.names(empB[,1],unique=TRUE)#reset row labels
empB <- empB[,-1]#remove row names rows
empB[] <- lapply(empB, as.numeric)
empB[is.na(empB)] <- 0

rownames(empB)[rownames(empB) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

empB["Metal.mining",] <- empB["Metal.mining",]+empB["Coal.mining",]+empB["Nonmetallic.minerals..except.fuels",]
rownames(empB)[rownames(empB) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
empB = empB[!row.names(empB)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]

empB["Lumber.and.wood.products",] <- empB["Lumber.and.wood.products",]+empB["Furniture.and.fixtures",]
rownames(empB)[rownames(empB) == "Lumber.and.wood.products"] <- 'Wood.products'
empB = empB[!row.names(empB)%in%c("Furniture.and.fixtures"),]

rownames(empB)[rownames(empB) == "Machinery..except.electrical"] <- "Machinery"
empB = empB[!row.names(empB)%in%c("Instruments.and.related.products"),]
rownames(empB)[rownames(empB) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

empB["Food.and.kindred.products",] <- empB["Food.and.kindred.products",]+empB["Tobacco.manufactures",]
rownames(empB)[rownames(empB) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
empB = empB[!row.names(empB)%in%c("Tobacco.manufactures"),]

empB["Apparel.and.other.textile.products",] <- empB["Apparel.and.other.textile.products",]+empB["Leather.and.leather.products",]
rownames(empB)[rownames(empB) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
empB = empB[!row.names(empB)%in%c("Leather.and.leather.products"),]

rownames(empB)[rownames(empB) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'

empB = empB[!row.names(empB)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(empB)[rownames(empB) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(empB)[rownames(empB) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

empB = empB[!row.names(empB)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(empB)[rownames(empB) == "Communication"] <- "Broadcasting.and.telecommunications"

empB = empB[!row.names(empB)%in%c("Finance..insurance..and.real.estate"),]
empB["Banking",] <- empB["Banking",]+empB["Credit.agencies.other.than.banks",]
rownames(empB)[rownames(empB) == "Banking"] <- 'Banking.and.credit.agencies'
empB = empB[!row.names(empB)%in%c("Credit.agencies.other.than.banks","Federal.Reserve.banks","Commercial.and.mutual.banks"),]

empB["Insurance.carriers",] <- empB["Insurance.carriers",]+empB["Insurance.agents..brokers..and.service",]
rownames(empB)[rownames(empB) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
empB = empB[!row.names(empB)%in%c("Insurance.agents..brokers..and.service"),]

empB = empB[!row.names(empB)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
empB = empB[!row.names(empB)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
empB = empB[!row.names(empB)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Miscellaneous.professional.services","Private.households"),]
empB = empB[!row.names(empB)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1","Other.services"),]

empB = empB[!row.names(empB)%in%c("Private.industries"),]

empB = empB[c(1:(nrow(empB)-6)), ]

#=========== empC =============================================
empC <- data.frame(empC)#put in dataframe
empC <- empC[-(1:6),] #romve top rows
empC <- head(empC,-9) #remove botttom rows
empC <- empC[,-c(1,3,4)]#remove useless rows
empC[1,1] <- "Industry" #replace title
empC[,1] <- str_remove(empC[,1],"\\\\[:digit:]\\\\")
 
names(empC) <- empC[1,]#set column names
empC <- empC[-1,]#remove column names rows
rownames(empC) <- make.names(empC[,1],unique=TRUE)#reset row labels
empC <- empC[,-1]#remove row names rows
empC[] <- lapply(empC, as.numeric)
 
rownames(empC)[rownames(empC) == 'Agricultural.services..forestry..and.fishing'] <- 'Forestry..fishing..and.related.activities'

empC["Metal.mining",] <- empC["Metal.mining",]+empC["Coal.mining",]+empC["Nonmetallic.minerals..except.fuels",]
rownames(empC)[rownames(empC) == 'Metal.mining'] <- 'Mining..except.oil.and.gas'
empC = empC[!row.names(empC)%in%c("Coal.mining","Nonmetallic.minerals..except.fuels"),]
 
empC["Lumber.and.wood.products",] <- empC["Lumber.and.wood.products",]+empC["Furniture.and.fixtures",]
rownames(empC)[rownames(empC) == "Lumber.and.wood.products"] <- 'Wood.products'
empC = empC[!row.names(empC)%in%c("Furniture.and.fixtures"),]

rownames(empC)[rownames(empC) == "Industrial.machinery.and.equipment"] <- "Machinery"
rownames(empC)[rownames(empC) == "Electronic.and.other.electric.equipment"] <- "Electric.and.electronic.equipment"
empC = empC[!row.names(empC)%in%c("Furniture.and.fixtures"),]
rownames(empC)[rownames(empC) == "Miscellaneous.manufacturing.industries"] <- "Miscellaneous.manufacturing"

empC["Food.and.kindred.products",] <- empC["Food.and.kindred.products",]+empC["Tobacco.products",]
rownames(empC)[rownames(empC) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
empC = empC[!row.names(empC)%in%c("Instruments.and.related.products","Tobacco.products"),]
 
empC["Apparel.and.other.textile.products",] <- empC["Apparel.and.other.textile.products",]+empC["Leather.and.leather.products",]
rownames(empC)[rownames(empC) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
empC = empC[!row.names(empC)%in%c("Leather.and.leather.products"),]

rownames(empC)[rownames(empC) == "Rubber.and.miscellaneous.plastics.products"] <- 'Plastics.and.rubber.products'
 
empC = empC[!row.names(empC)%in%c("Transportation.and.public.utilities","Transportation","Transportation.services"),]
rownames(empC)[rownames(empC) == "Trucking.and.warehousing"] <- 'Truck.transportation'
rownames(empC)[rownames(empC) == "Pipelines..except.natural.gas"] <- 'Pipeline.transportation'

empC = empC[!row.names(empC)%in%c("Telephone.and.telegraph","Radio.and.television","Wholesale.trade","Retail.trade"),]
rownames(empC)[rownames(empC) == "Communications"] <- "Broadcasting.and.telecommunications"

empC = empC[!row.names(empC)%in%c("Finance..insurance..and.real.estate"),]
empC["Depository.institutions",] <- empC["Depository.institutions",]+empC["Nondepository.institutions",]
rownames(empC)[rownames(empC) == "Depository.institutions"] <- 'Banking.and.credit.agencies'
empC = empC[!row.names(empC)%in%c("Nondepository.institutions","Federal.Reserve.banks","Commercial.and.mutual.depository.institutions"),]

empC["Insurance.carriers",] <- empC["Insurance.carriers",]+empC["Insurance.agents..brokers..and.service",]
rownames(empC)[rownames(empC) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
empC = empC[!row.names(empC)%in%c("Insurance.agents..brokers..and.service"),]
 
empC = empC[!row.names(empC)%in%c("Holding.and.other.investment.offices","Auto.repair..services..and.parking"),]
empC = empC[!row.names(empC)%in%c("Services","Hotels.and.other.lodging.places","Personal.services","Commercial.and.trade.schools.and.employment.agencies","Business.services","Miscellaneous.repair.services","Motion.pictures" ),]
empC = empC[!row.names(empC)%in%c("Social.services.and.membership.organizations","Social.services","Membership.organizations","Other.services","Private.households"),]
empC = empC[!row.names(empC)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1"),]

empC = empC[!row.names(empC)%in%c("Private.industries"),]

empC = empC[c(1:(nrow(empC)-6)), ]

#=========== empD =============================================
empD <- data.frame(empD)#put in dataframe
empD <- empD[-(1:6),] #romve top rows
empD <- head(empD,-12) #remove botttom rows
empD <- empD[,-c(1,3)]#remove useless rows
empD[1,1] <- "Industry" #replace title
empD[,1] <- str_remove(empD[,1],"\\\\[:digit:]\\\\")
 
names(empD) <- empD[1,]#set column names
empD <- empD[-1,]#remove column names rows
rownames(empD) <- make.names(empD[,1],unique=TRUE)#reset row labels
empD <- empD[,-1]#remove row names rows
empD[] <- lapply(empD, as.numeric)

empD = empD[-c(1:3)]

empD = empD[!row.names(empD)%in%c("Support.activities.for.mining"),]

rownames(empD)[rownames(empD) == "Nonmetallic.mineral.products"] <- "Stone..clay..and.glass.products"

empD = empD[!row.names(empD)%in%c("Wholesale.trade","Durable.goods.1","Nondurable.goods.1","Retail.trade","Motor.vehicle.and.parts.dealers","Food.and.beverage.stores","General.merchandise.stores","Other.retail"),]

empD["Computer.and.electronic.products",] <- empD["Computer.and.electronic.products",]+empD["Electrical.equipment..appliances..and.components",]
rownames(empD)[rownames(empD) == "Computer.and.electronic.products"] <- 'Electric.and.electronic.equipment'
empD = empD[!row.names(empD)%in%c("Electrical.equipment..appliances..and.components"),]

rownames(empD)[rownames(empD) == "Motor.vehicles..bodies.and.trailers..and.parts"] <- 'Motor.vehicles.and.equipment'
rownames(empD)[rownames(empD) == "Textile.mills.and.textile.product.mills"] <- 'Textile.mill.products'
rownames(empD)[rownames(empD) == "Paper.products"] <- 'Paper.and.allied.products'
rownames(empD)[rownames(empD) == "Printing.and.related.support.activities"] <- 'Printing.and.publishing'
rownames(empD)[rownames(empD) == "Chemical.products"] <- 'Chemicals.and.allied.products'
rownames(empD)[rownames(empD) == "Rail.transportation"] <- 'Railroad.transportation'
rownames(empD)[rownames(empD) == "Transit.and.ground.passenger.transportation"] <- 'Local.and.interurban.passenger.transit'
rownames(empD)[rownames(empD) == "Air.transportation"] <- 'Transportation.by.air'
rownames(empD)[rownames(empD) == "Utilities"] <- 'Electric..gas..and.sanitary.services'
rownames(empD)[rownames(empD) == "Federal.Reserve.banks..credit.intermediation..and.related.activities"] <- 'Banking.credit.agencies'
rownames(empD)[rownames(empD) == "Securities..commodity.contracts..and.investments"] <- 'Security.and.commodity.brokers'
rownames(empD)[rownames(empD) == "Amusements..gambling..and.recreation.industries"] <- 'Amusement.and.recreation.services'

empD = empD[!row.names(empD)%in%c("Furniture.and.related.products","Transportation.and.warehousing"),]
empD = empD[!row.names(empD)%in%c("Other.transportation.and.support.activities","Warehousing.and.storage","Information","Publishing.industries..includes.software.","Motion.picture.and.sound.recording.industries","Information.and.data.processing.services"),]
empD = empD[!row.names(empD)%in%c("Funds..trusts..and.other.financial.vehicles","Real.estate.and.rental.and.leasing","Rental.and.leasing.services.and.lessors.of.intangible.assets"),]
empD = empD[!row.names(empD)%in%c("Finance.and.insurance","Health.care.and.social.assistance"),]
empD = empD[!row.names(empD)%in%c("Professional..scientific..and.technical.services","Computer.systems.design.and.related.services","Miscellaneous.professional..scientific..and.technical.services","Management.of.TaxAnies.and.enterprises","Administrative.and.waste.management.services","Administrative.and.support.services","Waste.management.and.remediation.services","Social.assistance","Arts..entertainment..and.recreation","Performing.arts..spectator.sports..museums..and.related.activities","Accommodation.and.food.services","Accommodation","Food.services.and.drinking.places","Other.services..except.government"),]

empD["Ambulatory.health.care.services",] <- empD["Ambulatory.health.care.services",]+empD["Hospitals",]+empD["Nursing.and.residential.care.facilities",]
rownames(empD)[rownames(empD) == "Ambulatory.health.care.services"] <- 'Health.services'
empD = empD[!row.names(empD)%in%c("Hospitals","Nursing.and.residential.care.facilities"),]

empD = empD[!row.names(empD)%in%c("Private.industries"),]
empD = empD[c(1:6,8,7,9:nrow(empD)),]
empD = empD[c(1:27,29,28,30:nrow(empD)),]
empD = empD[c(1:30,32,31,33:nrow(empD)),]
empD = empD[c(1:31,35,34,33,32,36:nrow(empD)),]
empD <- rbind(empD[1:8,], empD[10:37,], empD[9,], empD[38:nrow(empD),])

rownames(empD)[rownames(empD) == "Credit.intermediation.and.related.activities"] <- 'Banking.credit.agencies'
empD = empD[!row.names(empD)%in%c("Federal.Reserve.banks"),]

empD = empD[!row.names(empD)%in%c("General.government","Civilian","Military","Government.enterprises","General.government.1","Education","Other","Government.enterprises.1","Management.of.companies.and.enterprises"),]

empD = empD[c(1:41,45,44,42,43,46:nrow(empD)),]

empD = empD[!row.names(empD)%in%c("Government","Federal","State.and.local"),]

empD <- head(empD,-3) #remove botttom rows

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
comp = cbind(compA,compB,compC,compD)
comp = data.frame(t(t(comp)*CPImulti))
comp = destroyX(comp)
comp = comp[!row.names(comp)%in%c("Government","Federal","State.and.local","Private.industries","Health.services","Legal.services","Educational.services"),]
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
#=========== All Employees =============================================
emp = cbind(empA,empB,empC,empD)
#=========== Calculate Tax percentage ===========================================
taxRate <- abs((bTax-aTax)/bTax)
taxRate[is.nan(taxRate)] <- 0
#=========== Calculate Comp per employee ===========================================
compPer <- comp/emp
compPer[is.nan(compPer)] <- 0

#taxRate = data.frame(t(taxRate[ ,!(names(taxRate) %in% c("2017","2018","1929","1930","1932","1933","1934","1931"))]))
#compPer = data.frame(t(compPer[ ,!(names(compPer) %in% c("2017","2018","1929","1930","1932","1933","1934","1931"))]))
taxRate = data.frame(t(taxRate[ ,!(names(taxRate) %in% c("2017","2018","1931"))]))
compPer = data.frame(t(compPer[ ,!(names(compPer) %in% c("2017","2018","1931"))]))

#=========== Make some charats ===========================
t1 = ggplot(taxRate, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Corporate.profits.before.tax, color="Average Corporate Tax", group = 1), linetype = "twodash") + 
        #geom_line(aes(y = Agriculture..forestry..and.fisheries, color="Agriculture, forestry and fisheries", group = 1)) +
        geom_line(aes(y = Mining..except.oil.and.gas, color="Mining, exceot oil & gas", group = 1)) +
        geom_line(aes(y = Oil.and.gas.extraction, color="Oil & gas extraction", group = 1)) +
        geom_line(aes(y = Construction, color="Construction", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

t2 = ggplot(taxRate, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Corporate.profits.before.tax, color = "Average Corporate Tax", group = 1), linetype = "twodash" ) + 
        #geom_line(aes(y = Primary.metal.industries, color="Primary metals industries", group = 1)) +
        geom_line(aes(y = Wood.products, color="Wood products", group = 1)) +
        geom_line(aes(y = Stone..clay..and.glass.products, color="Stone, clay, glass products", group = 1)) +
        geom_line(aes(y = Fabricated.metal.products, color="Fabricated metal industries", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

t3 = ggplot(taxRate, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Corporate.profits.before.tax, color = "Average Corporate Tax", group = 1), linetype = "twodash") + 
        geom_line(aes(y = Machinery, color="Machinery", group = 1)) +
        geom_line(aes(y = Electric.and.electronic.equipment, color="Electric and electronic equipment", group = 1)) +
        geom_line(aes(y = Motor.vehicles.and.equipment, color="Motor Vehivles and equipment", group = 1)) +
        #geom_line(aes(y = Miscellaneous.manufacturing, color="Moscellaneous manufacturing", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

t4 = ggplot(taxRate, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Corporate.profits.before.tax, color = "Average Corporate Tax", group = 1),linetype = "twodash") + 
        geom_line(aes(y = Food.and.beverage.and.tobacco.products, color="Food, beverage and tobacco", group = 1)) +
        #geom_line(aes(y = Textile.mill.products, color="Textile products", group = 1)) +
        geom_line(aes(y = Apparel.and.leather.and.allied.products, color="Apparel and leather products", group = 1)) +
        geom_line(aes(y = Printing.and.publishing, color="printing and publishing", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

t5 = ggplot(taxRate, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Corporate.profits.before.tax, color = "Average corporate tax", group = 1),linetype = "twodash") + 
        geom_line(aes(y = Chemicals.and.allied.products, color = "Chemical products", group = 1)) + 
        #geom_line(aes(y = Petroleum.and.coal.products, color="petroleum & coal products", group = 1)) +
        geom_line(aes(y = Plastics.and.rubber.products, color="Plastics & rubber products", group = 1)) +
        #geom_line(aes(y = Broadcasting.and.telecommunications, color="Broadcasting & telecommunications", group = 1)) +
        geom_line(aes(y = Electric..gas..and.sanitary.services, color="Electric, gas, sanitary services", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

t6 = ggplot(taxRate, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Corporate.profits.before.tax, color = "Average corporate tax", group = 1),linetype = "twodash") + 
        geom_line(aes(y = Banking.and.credit.agencies, color = "Banking and credit agencies", group = 1)) + 
        #geom_line(aes(y = Security.and.commodity.brokers, color="Security and commodity brokers", group = 1)) +
        geom_line(aes(y = Insurance.carriers.and.related.activites, color="Insurance related activites", group = 1)) +
        geom_line(aes(y = Real.estate, color="real estate", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

c1 = ggplot(compPer, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Compensation.of.employees, color="Average Corporate Tax", group = 1), linetype = "twodash") + 
        geom_line(aes(y = Agriculture..forestry..and.fisheries, color="Agriculture, forestry and fisheries", group = 1)) +
        geom_line(aes(y = Mining..except.oil.and.gas, color="Mining, exceot oil & gas", group = 1)) +
        geom_line(aes(y = Oil.and.gas.extraction, color="Oil & gas extraction", group = 1)) +
        geom_line(aes(y = Construction, color="Construction", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

c2 = ggplot(compPer, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Compensation.of.employees, color = "Average Corporate Tax", group = 1), linetype = "twodash" ) + 
        geom_line(aes(y = Primary.metal.industries, color="Primary metals industries", group = 1)) +
        geom_line(aes(y = Wood.products, color="Wood products", group = 1)) +
        geom_line(aes(y = Stone..clay..and.glass.products, color="Stone, clay, glass products", group = 1)) +
        geom_line(aes(y = Fabricated.metal.products, color="Fabricated metal industries", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

c3 = ggplot(compPer, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Compensation.of.employees, color = "Average Corporate Tax", group = 1), linetype = "twodash") + 
        geom_line(aes(y = Machinery, color="Machinery", group = 1)) +
        geom_line(aes(y = Electric.and.electronic.equipment, color="Electric and electronic equipment", group = 1)) +
        geom_line(aes(y = Motor.vehicles.and.equipment, color="Motor Vehivles and equipment", group = 1)) +
        #geom_line(aes(y = Miscellaneous.manufacturing, color="Moscellaneous manufacturing", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

c4 = ggplot(compPer, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Compensation.of.employees, color = "Average Corporate Tax", group = 1),linetype = "twodash") + 
        geom_line(aes(y = Food.and.beverage.and.tobacco.products, color="Food, beverage and tobacco", group = 1)) +
        #geom_line(aes(y = Textile.mill.products, color="Textile products", group = 1)) +
        geom_line(aes(y = Apparel.and.leather.and.allied.products, color="Apparel and leather products", group = 1)) +
        geom_line(aes(y = Printing.and.publishing, color="printing and publishing", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

c5 = ggplot(compPer, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Compensation.of.employees, color = "Average corporate tax", group = 1),linetype = "twodash") + 
        geom_line(aes(y = Chemicals.and.allied.products, color = "Chemical products", group = 1)) + 
        #geom_line(aes(y = Petroleum.and.coal.products, color="petroleum & coal products", group = 1)) +
        geom_line(aes(y = Plastics.and.rubber.products, color="Plastics & rubber products", group = 1)) +
        #geom_line(aes(y = Broadcasting.and.telecommunications, color="Broadcasting & telecommunications", group = 1)) +
        geom_line(aes(y = Electric..gas..and.sanitary.services, color="Electric, gas, sanitary services", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

c6 = ggplot(compPer, aes(x=row.names(taxRate))) + 
        geom_line(aes(y = Compensation.of.employees, color = "Average corporate tax", group = 1),linetype = "twodash") + 
        geom_line(aes(y = Banking.and.credit.agencies, color = "Banking and credit agencies", group = 1)) + 
        #geom_line(aes(y = Security.and.commodity.brokers, color="Security and commodity brokers", group = 1)) +
        geom_line(aes(y = Insurance.carriers.and.related.activites, color="Insurance related activites", group = 1)) +
        geom_line(aes(y = Real.estate, color="real estate", group = 1)) +
        xlab('Years') +
        ylab('Tax rate')+
        theme(axis.text.x = element_text(angle = 90))

# https://www.pewresearch.org/fact-tank/2018/08/07/for-most-us-workers-real-wages-have-barely-budged-for-decades/

temp = (t(compPer))

fit1 <- lm(compPer$Compensation.of.employees ~ taxRate$Corporate.profits.before.tax)
fit4 <- lm(compPer$Construction ~ taxRate$Construction)
fit17 <- lm(compPer$Security.and.commodity.brokers ~ taxRate$Security.and.commodity.brokers)
summary(fit1)

ggplot(fit$model, aes_string(x = names(fit$model)[2],y = names(fit$model)[1]))+geom_point()+stat_smooth(method = "lm", col = "red") + 
        labs(title= "Employee compensation vs. Corporate tax rate", subtitle = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5)," P =",signif(summary(fit)$coef[2,4], 5)), 
             y="Employee compensation (thousands)", x = "Corporate tax rate") + 
        theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
