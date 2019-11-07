library("readxl")
#parse in data for analysis
compA <- read_excel("Section6.xlsx",sheet ="T60200A-A")
compB <- read_excel("Section6.xlsx",sheet ="T60200B-A")
compC <- read_excel("Section6.xlsx",sheet ="T60200C-A")
compD <- read_excel("Section6.xlsx",sheet ="T60200D-A")

compA <- data.frame(compA)
compA <- compA[-(1:6),]
compA <- head(compA,-7)
compA <- compA[,-c(1,3)]
compA[1,1] <- "Industry"
names(compA) <- compA[1,]
compA <- compA[-1,]
rownames(compA) <- NULL
print(compA)


