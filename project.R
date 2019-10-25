library("readxl")
#parse in data for analysis
section6 <- read_excel("Section6.xlsx",sheet ="T60200A-A")

sec6DF <- data.frame(section6)
sec6DF <- sec6DF[-(1:7),]
sec6DF <- head(sec6DF,-7)
sec6DF <- sec6DF[,-c(1,3)]
names(sec6DF) <- sec6DF[1]
print(sec6DF)




