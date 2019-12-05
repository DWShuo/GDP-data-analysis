library("readxl")
library("stringr")
library("ggplot2")
library("blscrapeR")

#parse in data for analysis
compA <- read_excel("Section6.xlsx",sheet ="T60900B-A")
compC <- read_excel("Section6.xlsx",sheet ="T60900C-A")
compD <- read_excel("Section6.xlsx",sheet ="T60900D-A")

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