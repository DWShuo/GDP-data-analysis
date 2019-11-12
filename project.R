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

compA["Metal.mining",] <- compA["Metal.mining",]+compA["Anthracite.mining",]+compA["Bituminous.and.other.soft.coal.mining",]+compA["Nonmetallic.mining.and.quarrying",]+compA["Nonmetallic.mining.and.quarrying",]
rownames(compA)[rownames(compA) == 'Metal.mining'] <- 'Mining,.except.oil.and.gas'
compA = compA[!row.names(compA)%in%c("Anthracite.mining","Bituminous.and.other.soft.coal.mining","Nonmetallic.mining.and.quarrying"),]

compA["Lumber.and.basic.timber.products",] <- compA["Lumber.and.basic.timber.products",]+compA["Furniture.and.finished.lumber.products",]
rownames(compA)[rownames(compA) == "Lumber.and.basic.timber.products"] <- 'Wood.products'
compA = compA[!row.names(compA)%in%c("Furniture.and.finished.lumber.products"),]

compA["Food.and.kindred.products",] <- compA["Food.and.kindred.products",]+compA["Tobacco.manufactures",]
rownames(compA)[rownames(compA) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
compA = compA[!row.names(compA)%in%c("Tobacco.manufactures"),]

compA["Apparel.and.other.textile.products",] <- compA["Apparel.and.other.textile.products",]+compA["Leather.and.leather.products",]
rownames(compA)[rownames(compA) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
compA = compA[!row.names(compA)%in%c("Leather.and.leather.products"),]

compA = compA[!row.names(compA)%in%c("Telephone.and.telegraph","Radio.and.television.broadcasting","Electric..gas..and.sanitary.services","Utilities..electric.and.gas","Local.utilities.and.public.services..n.e.c.","Wholesale.trade","Retail.trade.and.automobile.services"),]
rownames(compA)[rownames(compA) == "Communication"] <- "Broadcasting.and.telecommunications"

compA["Insurance.carriers",] <- compA["Insurance.carriers",]+compA["Insurance.agents..brokers..and.service",]
rownames(compA)[rownames(compA) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
compA = compA[!row.names(compA)%in%c("Insurance.agents..brokers..and.service"),]

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

compB["Metal.mining",] <- compB["Metal.mining",]+compB["Coal.mining",]+compB["Nonmetallic.minerals,.exceot fuels",]
rownames(compB)[rownames(compB) == 'Metal.mining'] <- 'Mining,.except.oil.and.gas'
compB = compB[!row.names(compB)%in%c("Coal.mining","Nonmetallic.minerals,.exceot fuels"),]

compB["Lumber.and.basic.timber.products",] <- compB["Lumber.and.wood.products",]+compB["Furniture.and.fixtures",]
rownames(compB)[rownames(compB) == "Lumber.and.wood.products"] <- 'Wood.products'
compB = compB[!row.names(compB)%in%c("Furniture.and.fixtures"),]

compB["Food.and.kindred.products",] <- compB["Food.and.kindred.products",]+compB["Tobacco.manufactures",]
rownames(compB)[rownames(compB) == "Food.and.kindred.products"] <- 'Food.and.beverage.and.tobacco.products'
compB = compB[!row.names(compB)%in%c("Tobacco.manufactures"),]

compB["Apparel.and.other.textile.products",] <- compB["Apparel.and.other.textile.products",]+compB["Leather.and.leather.products",]
rownames(compB)[rownames(compB) == "Apparel.and.other.textile.products"] <- 'Apparel.and.leather.and.allied.products'
compB = compB[!row.names(compB)%in%c("Leather.and.leather.products"),]

compB = compB[!row.names(compB)%in%c("Telephone.and.telegraph","Radio.and.television","Electric..gas..and.sanitary.services","Wholesale.trade","Retail.trade"),]
rownames(compB)[rownames(compB) == "Communication"] <- "Broadcasting.and.telecommunications"

compA["Insurance.carriers",] <- compA["Insurance.carriers",]+compA["Insurance.agents..brokers..and.service",]
rownames(compA)[rownames(compA) == "Insurance.carriers"] <- 'Insurance.carriers.and.related.activites'
compA = compA[!row.names(compA)%in%c("Insurance.agents..brokers..and.service"),]