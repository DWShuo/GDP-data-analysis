library("readxl")
library("stringr")
#parse in data for analysis
compA <- read_excel("Section6.xlsx",sheet ="T60200A-A")
compB <- read_excel("Section6.xlsx",sheet ="T60200B-A")
compC <- read_excel("Section6.xlsx",sheet ="T60200C-A")
compD <- read_excel("Section6.xlsx",sheet ="T60200D-A")

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
compC <- compC[,-c(1,3)]#remove useless rows
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