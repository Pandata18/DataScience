#Final Project

library(readr)
World_Bank_Data3 <- read_csv(file.choose())# "World Bank Data3.csv"
Country_Region_Income <- read_csv(file.choose()) # Country_Region_Income.csv")
#Please manually select the above data files



dim(World_Bank_Data3)
names(World_Bank_Data3)
#rownames(World_Bank_Data3) = World_Bank_Data3[,1]

#is.na.data.frame(World_Bank_Data3)
which(is.na(World_Bank_Data3[,3]))

Paises=World_Bank_Data3[,1]
rownames(World_Bank_Data3) = as.character(Paises)

length(World_Bank_Data3[,1])

 
#_____________________________________________________________________
names(World_Bank_Data3)



World_Bank_Data_no_consump = World_Bank_Data3[,-14]

World_Bank_Data_no_consump_or_value_added= World_Bank_Data3[,-c(14:15)] #Took out %consum

head(World_Bank_Data_no_consump_or_value_added)
names(World_Bank_Data_no_consump_or_value_added)

#Japan
World_Bank_Data_no_consump_or_value_added[98,1] #Japan is on row 98

World_Bank_Data_no_consump_or_value_added[98,] #Gini is the 9th column

World_Bank_Data_no_consump_or_value_added_w_Japan_Gini = World_Bank_Data_no_consump_or_value_added

World_Bank_Data_no_consump_or_value_added_w_Japan_Gini[98,9] 
World_Bank_Data_no_consump_or_value_added_w_Japan_Gini[98,9] = 33.6
World_Bank_Data_no_consump_or_value_added_w_Japan_Gini[98,9] #Adding GINI from oecd website

#Saudi arabia gini from imf report = 45.9 

World_Bank_Data_no_consump_or_value_added_w_Gini_CPI = World_Bank_Data_no_consump_or_value_added_w_Japan_Gini#New dataframe

World_Bank_Data_no_consump_or_value_added_w_Gini_CPI[166,] #Saudi Arabia

World_Bank_Data_no_consump_or_value_added_w_Gini_CPI[166,9] 
World_Bank_Data_no_consump_or_value_added_w_Gini_CPI[166,9]  = 45.9 #Adding Saudi GINI from imf report
World_Bank_Data_no_consump_or_value_added_w_Gini_CPI[166,9]

#Adding Argentina CPI 

World_Bank_Data_no_consump_or_value_added_w_Gini_CPI[8,4]
World_Bank_Data_no_consump_or_value_added_w_Gini_CPI[8,4]=41.0 #Adding Buenos Aires CPI for Argentina
World_Bank_Data_no_consump_or_value_added_w_Gini_CPI[8,4]

dim(World_Bank_Data_no_consump_or_value_added_w_Gini_CPI)

NewWorld_US_CHN_JPN_SA=na.omit(World_Bank_Data_no_consump_or_value_added_w_Gini_CPI, cols=c(3:15), invert=FALSE)
dim(NewWorld_US_CHN_JPN_SA)

df_NewWorld_US_CHN_JPN_SA=as.data.frame(NewWorld_US_CHN_JPN_SA)
dim(df_NewWorld_US_CHN_JPN_SA)
df_NewWorld_US_CHN_JPN_SA[,1]

names(df_NewWorld_US_CHN_JPN_SA)

#____________________________________________

pairs(df_NewWorld_US_CHN_JPN_SA[,-c(1,2)])
round(cor(df_NewWorld_US_CHN_JPN_SA[,-c(1,2)]),3)

#___________________________________________
#Principal components


Noname=df_NewWorld_US_CHN_JPN_SA[,-c(1,2)]
is.data.frame(Noname)

names(Noname)
dim(Noname)

X <- scale(Noname)
Corr <-cor(Noname)

round(Corr,3)
pairs(X)

pairs(X[,-c(3,9)])#without GDP deflator and gross savings

#Renaming Stuff

names(df_NewWorld_US_CHN_JPN_SA)
Noname2=df_NewWorld_US_CHN_JPN_SA[,-c(1,2,5,11)] #without names, GDP deflator and gross savings
is.data.frame(Noname2)

names(Noname2)
dim(Noname2)

X <- scale(Noname2)
Corr <-cor(Noname2)

round(Corr,3)
pairs(X)


Eigenvector<- eigen(Corr)$vectors # Eigenvector matrix
Eigenvector

rownames(Eigenvector)= names(Noname2)

round(Eigenvector, 3)


PC_noname=X%*%Eigenvector # Principal Component matrix
PC_noname

Eigenvalues<-eigen(Corr)$values
Eigenvalues



plot(Eigenvalues,type="l", main="Scree Plot of Eigenvalues")

cumsum(Eigenvalues)/sum(Eigenvalues) #Total Variation

round(Eigenvector[,1:4],3)

#Plot 2 pcs

plot(PC_noname[,1:2], type="n")
text(PC_noname[,1:2], df_NewWorld_US_CHN_JPN_SA[,1], cex=0.8)
text(PC_noname[,1:2], df_NewWorld_US_CHN_JPN_SA[,2])




#Plot 4 PCS
pairs(PC_noname[,1:4], labels=df_NewWorld_US_CHN_JPN_SA[,1], cex=0.8)
text(PC_noname[,1:4], df_NewWorld_US_CHN_JPN_SA[,1], cex=0.8)

pairs(PC_noname[,1:5])
pairs(PC_noname[,1:3])

plot(PC_noname[,1:2], type="n", main="PC1 and PC2")
text(PC_noname[,1:2], df_NewWorld_US_CHN_JPN_SA[,1], cex=0.8)

plot(PC_noname[,1],PC_noname[,3], type="n",  main="PC1 and PC3")
text(PC_noname[,c(1,3)], df_NewWorld_US_CHN_JPN_SA[,1], cex=0.8)

plot(PC_noname[,2],PC_noname[,3],  main="PC2 and PC3")
text(PC_noname[,c(2,3)], df_NewWorld_US_CHN_JPN_SA[,1], cex=0.7)

#______________________________

# Merging Dataset ---------------------------------------------------------
dim(Merger)
#Trying to merge dataset
library(readr)
#Country_Region_Income <- read_csv(file.choose()) # Country_Region_Income.csv")
names(Country_Region_Income)
dim(Country_Region_Income)

CRI=as.data.frame(Country_Region_Income)
names(CRI)
CRI[220,]
CRI[219,]
CRI[218,]

CRI=CRI[-c(219:278),]
dim(CRI)
CRI[218,]
CRI=CRI[,-9]
names(CRI)

str(CRI)


CRI$Region=as.factor(CRI$Region)
CRI[,4]=as.factor(CRI[,4])

levels(CRI[,3])
levels(CRI[,4])

dim(CRI)
df_NewWorld_US_CHN_JPN_SA[80,]#Romania
df_NewWorld_US_CHN_JPN_SA[103,]#West Bank and Gaza


Merger <- merge( df_NewWorld_US_CHN_JPN_SA,CRI,by=c("Country Name", "Country Code"))

is.data.frame(Merger)
names(Merger)
dim(Merger)
Merger[,1]

#__________ MERGED PC


pairs(PC_noname[,1:4], col= 1+Merger[,19])#Eurozone Countries
pairs(PC_noname[,1:3], col= c(1+Merger[,18]))#Heavily indebted poor countries
pairs(PC_noname[,1:4], col= c(1+Merger[,18]))#Heavily indebted poor countries
pairs(PC_noname[,1:3], col= c(1+Merger[,21]))#OECD


#_____________________________________
#FINAL PCs 
#Without Balance of payments nominal

names(df_NewWorld_US_CHN_JPN_SA)
Noname3=df_NewWorld_US_CHN_JPN_SA[,-c(1,2,5,6,11)] #without names, GDP deflator, nominal BOP and gross savings
is.data.frame(Noname3)

names(Noname3)
dim(Noname3)

EEX <- scale(Noname3)
CorrEEX <-cor(Noname3)

round(CorrEEX,3)
pairs(EEX)


EEX_Eigenvector<- eigen(CorrEEX)$vectors # Eigenvector matrix

rownames(EEX_Eigenvector)= names(Noname3)

round(EEX_Eigenvector, 3)


PC_noname3=EEX%*%EEX_Eigenvector # Principal Component matrix


EEX_Eigenvalues<-eigen(CorrEEX)$values
EEX_Eigenvalues
round(EEX_Eigenvalues,3)


plot(EEX_Eigenvalues,type="l", main="Scree Plot of Eigenvalues", ylab="Eigenvalues")

cumsum(EEX_Eigenvalues)/sum(EEX_Eigenvalues) #Total Variation

round(EEX_Eigenvector[,1:3],3)

names(df_NewWorld_US_CHN_JPN_SA)

df_NewWorld_no_bop_gdp_deflator_or_gross_savings=df_NewWorld_US_CHN_JPN_SA[,-c(5,6,11)]

pairs(PC_noname3[,1:3], pch=16, labels= c("PC1", "PC2", "PC3"))

plot(PC_noname3[,c(1,2)], type="n", main="PC1 and PC2", xlab="PC1", ylab="PC2")
text(PC_noname3[,c(1,2)], df_NewWorld_US_CHN_JPN_SA[,1],cex=0.8)
abline(h=0)
abline(v=0)


####
plot(PC_noname3[,c(1,3)], type="n", main="PC1 and PC3", xlab="PC1", ylab="PC3")
text(PC_noname3[,c(1,3)], df_NewWorld_US_CHN_JPN_SA[,1],cex=0.8)

plot(PC_noname3[,c(2,3)], type="n", main="PC2 and PC3", xlab="PC2", ylab="PC3")
text(PC_noname3[,c(2,3)], df_NewWorld_US_CHN_JPN_SA[,1],cex=0.8)


# Comparing PC to known cat vars ------------------------------------------


###### Comparing the PC to the known groups

par(oldpar)
par(oma=c(4,4,4,4))
par(mar = c(0, 0, 0, 0))
dev.off()

plot(PC_noname_EEX[,1:2],col=c(1+Merger[,21]), cex=0.8)
text(PC_noname_EEX[,c(1,2)], df_NewWorld_US_CHN_JPN_SA[,1],cex=0.8)

#Heavly Indebted Poor Countries
#Text
plot(PC_noname_EEX[,1:2],col=c(1+Merger[,18]), type="n", main="2 PC Plot of Heavily Indebted Poor Countries",  xlab="PC1", ylab="PC2")
text(PC_noname_EEX[,c(1,2)], df_NewWorld_US_CHN_JPN_SA[,1], col=c(1+Merger[,18]), cex=0.8)#Heavily indebted poor countries

#Plot
plot(PC_noname_EEX[,1:2],col=c(1+Merger[,18]), pch=16, main="2 PC Plot of Heavily Indebted Poor Countries",  xlab="PC1", ylab="PC2")
abline(h=0)
abline(v=0)

names(Merger[15:21])

#Eurozone
#Text
plot(PC_noname_EEX[,1:2],col=c(1+Merger[,19]), type="n", main="2 PC Plot of Eurozone Countries",  xlab="PC1", ylab="PC2")
text(PC_noname_EEX[,c(1,2)], df_NewWorld_US_CHN_JPN_SA[,1], col=c(1+Merger[,19]), cex=0.8)#Heavily indebted poor countries

#Plot
plot(PC_noname_EEX[,1:2],col=c(1+Merger[,19]), pch=16, main="2 PC Plot of Eurozone Countries",  xlab="PC1", ylab="PC2")
abline(h=0)
abline(v=0)

#European Union

#Text
plot(PC_noname_EEX[,1:2],col=c(1+Merger[,20]), type="n", main="PC Plot of European Union Countries",  xlab="PC1", ylab="PC2")
text(PC_noname_EEX[,c(1,2)], df_NewWorld_US_CHN_JPN_SA[,1], col=c(1+Merger[,20]), cex=0.8)

#Plot
plot(PC_noname_EEX[,1:2],col=c(1+Merger[,20]), pch=16, main="2 PC Plot of European Union Countries",  xlab="PC1", ylab="PC2")
abline(h=0)
abline(v=0)


#OECD
#Text
plot(PC_noname_EEX[,1:2],col=c(1+Merger[,21]), type="n", main="PC Plot of OECD Countries",  xlab="PC1", ylab="PC2")
text(PC_noname_EEX[,c(1,2)], df_NewWorld_US_CHN_JPN_SA[,1], col=c(1+Merger[,21]), cex=0.8)

#Plot
plot(PC_noname_EEX[,1:2],col=c(1+Merger[,21]), pch=16, main="PC Plot of OECD Countries",  xlab="PC1", ylab="PC2")
abline(h=0)
abline(v=0)


#BRIC
Merger[,1]
#India = 45, Russia = 81, China = 20, Brazil = 14
105-81


Merger[c(14,20,45,81),]

bric_col=c(rep(0,13),1, 0,0,0,0,0,1,rep(0,24),1, rep(0,5), rep(0,30),1,rep(0,24))
length (bric_col)
bric_col[14]
bric_col[20]
bric_col[45]
bric_col[81]

#Text
plot(PC_noname_EEX[,1:2],col=1+bric_col, type="n", main="PC Plot of BRIC Countries",  xlab="PC1", ylab="PC2")
text(PC_noname_EEX[,c(1,2)], df_NewWorld_US_CHN_JPN_SA[,1], col=1+bric_col, cex=0.8)

#Plot
plot(PC_noname_EEX[,1:2],col=1+bric_col, pch=1+bric_col, main="PC Plot of BRIC Countries",  xlab="PC1", ylab="PC2")

abline(h=0)
abline(v=0)

plot(PC_noname_EEX[,1:2],col=1+bric_col, pch=16+bric_col, main="PC Plot of BRIC Countries",  xlab="PC1", ylab="PC2")

abline(h=0)
abline(v=0)



# Mapping Data ------------------------------------------------------------

#install.packages("rworldmap")
library(rworldmap)
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")


names(Merger)


mapped_data <- joinCountryData2Map(Merger, joinCode = "ISO3",  nameJoinColumn = "Country Code")
library(RColorBrewer)

#mapCountryData(mapped_data, nameColumnToPlot = "Gini",mapTitle="Gini Index", colourPalette= "heat", numCats=5)

#GINI
mapCountryData(mapped_data, nameColumnToPlot = "Gini",mapTitle="Gini Index", colourPalette= brewer.pal(7,"YlOrRd"),addLegend=FALSE )
mapParams=mapCountryData(mapped_data, nameColumnToPlot = "Gini",mapTitle="Gini Index", colourPalette= brewer.pal(7,"YlOrRd") , addLegend=FALSE )

do.call(addMapLegend,c(mapParams,legendLabels="all",legendIntervals="data", sigFigs=2, labelFontSize=0.8, legendShrink=0.8,  legendWidth=0.9 , legendMar=3))

#Europe:
mapCountryData(mapped_data, nameColumnToPlot = "Gini",mapTitle="Gini Index", colourPalette= brewer.pal(7,"YlOrRd"),addLegend=FALSE, mapRegion="Eurasia" )
mapParams=mapCountryData(mapped_data, nameColumnToPlot = "Gini",mapTitle="Gini Index", colourPalette= brewer.pal(7,"YlOrRd") , addLegend=FALSE,  mapRegion="Eurasia" )

do.call(addMapLegend,c(mapParams,legendLabels="all",legendIntervals="data", sigFigs=2, labelFontSize=0.8, legendShrink=0.8,  legendWidth=0.9 , legendMar=3))


#GDP per Capita
mapCountryData(mapped_data, nameColumnToPlot = "GDP per capita", colourPalette= brewer.pal(7,"Blues"),addLegend=FALSE,  catMethod = "logFixedWidth" )
mapParams=mapCountryData(mapped_data, nameColumnToPlot = "GDP per capita", colourPalette= brewer.pal(7,"Blues"),addLegend=FALSE , catMethod = "logFixedWidth")

do.call(addMapLegend,c(mapParams,legendLabels="all",legendIntervals="data", sigFigs=2, labelFontSize=0.8, legendShrink=0.8,  legendWidth=0.9 , legendMar=3))





mapCountryData(mapped_data, nameColumnToPlot = "Net FDI",mapTitle="Net Foreign Direct Investment in current USD", colourPalette= rev(brewer.pal(7,"RdYlGn")) )



#Current Account
mapCountryData(mapped_data, nameColumnToPlot = "Current Account (% GDP)", colourPalette= brewer.pal(9,"RdYlGn"), numCats = 9,  catMethod = c(-15,-10,-5,-2.5,-1,0,1,2.5,5,10), addLegend=FALSE) 

mapParams=mapCountryData(mapped_data, nameColumnToPlot = "Current Account (% GDP)", colourPalette= brewer.pal(9,"RdYlGn"), numCats = 9,  catMethod = c(-15,-10,-5,-2.5,-1,0,1,2.5,5,10), addLegend=FALSE ) 

#mapParams
mapParams$cutVector= c(-15,-10,-5,-2.5,-1,0,1,2.5,5,10)

do.call(addMapLegend,c(mapParams,legendLabels="all",legendIntervals="data", sigFigs=2, labelFontSize=0.8, legendShrink=0.8,  legendWidth=0.8 ))

names(Merger)



#Gross savings %: 
mapCountryData(mapped_data, nameColumnToPlot = "Gross savings (% of GDP)", colourPalette= brewer.pal(9,"RdYlGn"), numCats = 9,  catMethod = c(-8,0,5,10, 15, 20, 25, 30,35, 48), addLegend=FALSE) 
mapParams=mapCountryData(mapped_data, nameColumnToPlot = "Gross savings (% of GDP)", colourPalette= brewer.pal(9,"RdYlGn"), numCats = 9,  catMethod = c(-8,0,5,10, 15, 20, 25, 30,35, 48), addLegend=FALSE )

do.call(addMapLegend,c(mapParams,legendLabels="all",legendIntervals="data", sigFigs=2, labelFontSize=0.8, legendShrink=0.8,  legendWidth=0.9 , legendMar=3))

names(Merger)



#% Trade

Purple2green=c("#9970AB","#C2A5CF","#E7D4E8","#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441b")

mapCountryData(mapped_data, nameColumnToPlot = "Trade (% of GDP)", colourPalette= Purple2green, numCats = 8,  catMethod = c(24,50,75, 100, 125, 150, 200, 250, 450),addLegend=FALSE ) 

mapParams=mapCountryData(mapped_data, nameColumnToPlot = "Trade (% of GDP)", colourPalette= c("#9970AB","#C2A5CF","#E7D4E8","#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441b"), numCats = 8,  catMethod = c(24,50,75, 100, 125, 150, 200, 250, 450), borderCol="#43464B",  addLegend=FALSE) 

do.call(addMapLegend,c(mapParams,legendLabels="all",legendIntervals="data", sigFigs=2, labelFontSize=0.8, legendShrink=0.95,  legendWidth=0.9 , legendMar=3))



#Just Europe

mapCountryData(mapped_data, nameColumnToPlot = "Trade (% of GDP)", colourPalette= Purple2green, numCats = 8,  catMethod = c(24,50,75, 100, 125, 150, 200, 250, 450),addLegend=FALSE , mapRegion="Eurasia") 

mapParams=mapCountryData(mapped_data, nameColumnToPlot = "Trade (% of GDP)", colourPalette= c("#9970AB","#C2A5CF","#E7D4E8","#D9F0D3", "#A6DBA0", "#5AAE61", "#1B7837", "#00441b"), numCats = 8,  catMethod = c(24,50,75, 100, 125, 150, 200, 250, 450), borderCol="#43464B",  addLegend=FALSE,  mapRegion="Eurasia" ) 

do.call(addMapLegend,c(mapParams,legendLabels="all",legendIntervals="data", sigFigs=2, labelFontSize=0.8, legendShrink=0.5,  legendWidth=0.9 , legendMar=20))





#Regions
par(mai=c(0.2,0.3,0.6,0),xaxs="i",yaxs="i")
mapDevice("windows")
mapCountryData(mapped_data, nameColumnToPlot = "Region", colourPalette=brewer.pal(7,"Set3"),   catMethod = "categorical", mapTitle = "Regions", addLegend = FALSE) 



# High Income/ Low income groups
mapCountryData(mapped_data, nameColumnToPlot = "Income group", colourPalette= rev(colours17[2:5]),   catMethod = "categorical", mapTitle = "Income Groups") 


#OECD
mapCountryData(mapped_data, nameColumnToPlot = "OECD", colourPalette= c("white",colours17[16]),   catMethod = "categorical", mapTitle = "OECD Members", addLegend=FALSE ) 

#Euro
mapCountryData(mapped_data, nameColumnToPlot = "European Union", colourPalette= c("white",colours17[4]),   catMethod = "categorical", mapTitle = "European Union",borderCol="#43464B", addLegend=FALSE  ,mapRegion="Europe")

which(Merger$`European Union`==1)
hihih=c(6,9, 15, 23, 24, 25, 26, 31, 33, 34, 36, 38, 43, 47, 49, 55, 58, 59, 70, 78, 79, 80, 86, 87, 89, 91, 99)
Merger[hihih,c(1,2,19,20)]

#Eurozone
mapCountryData(mapped_data, nameColumnToPlot = "Eurozone", colourPalette= c("white","gold"),   catMethod = "categorical",borderCol="#43464B", addLegend=FALSE  ,mapRegion="Europe")

#Heavily Indebted Poor Countries

mapCountryData(mapped_data, nameColumnToPlot = "Heavily Indebted Poor Countries HIPC", colourPalette= c("white",colours17[6]),   catMethod = "categorical", addLegend=FALSE, mapTitle="Heavily Indebted Poor Countries" )

hipc_countries=(which(Merger$`Heavily Indebted Poor Countries HIPC`==1))
#hipc_countries= c(10 , 12,  16, 17 , 32,  37 , 40,  41  ,42,  61,  62,  63,  71 , 82 , 85, 93, 97 ,104)
Merger[hipc_countries,c(1,2,18)]


