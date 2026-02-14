# Assignment: Final Project
# Name: Saenz, Nicolas
# Date: 2025-03-2

## Final Step

library(ggplot2)
library(dplyr)
library(Metrics)
library (readxl)



setwd("C:/Users/Nick/DSC 530/project data sets/")

Econ_60_22 <- read.csv("GDP World GDP 1960-2022.csv")
BioData <- read.csv("GRIIS - Country Compendium V1_0.csv")
EconTour <- read.csv("structured_UNWTO_tourism_data.csv")
Econ2 <- read.csv("world-data-2023.csv")
ImportsExp <- read.csv("34_years_world_export_import_dataset.csv")
LandL <- read.csv("Landlocked Countries.csv")



#remove unnecessary columns in the 
BioData <-dplyr::select(BioData, -c("order","family","taxonRank","recordID",
                                    "reportedTaxon","scientificName",
                                    "countryCode_alpha3","countryCode_alpha2"))



#remove years that arent covered in other data sets
Econ_60_22 <-dplyr::select(Econ_60_22, -c("X1960", "X1961", "X1962", "X1963",
                                          "X1964", "X1965", "X1966", "X1967",
                                          "X1968", "X1969", "X1970", "X1971",
                                          "X1972", "X1973", "X1974", "X1975",
                                          "X1976", "X1977", "X1978", "X1979",
                                          "X1980", "X1981", "X1982", "X1983",
                                          "X1984", "X1985", "X1986", "X1987",
                                          "X1988", "X1989", "X1990", "X1991",
                                          "X1992", "X1993", "X1994","X1995",
                                          "X1996", "X1997", "X1998", "X1999",
                                          "X2000", "X2001", "X2002", "X2003",
                                          "X2004", "X2005", "X2006", "X2007",
                                          "X2008", "X2009", "X2010", "X2011",
                                          "X2012", "X2013", "X2014", "X2015",
                                          "X2016", "X2017", "X2018", "X2019","X2020"))
Econ_60_22 <-dplyr::select(Econ_60_22, -c("Country.Code"))



Econ2 <-dplyr::select(Econ2, -c("Calling.Code","Currency.Code",
                                "Official.language","Armed.Forces.size",
                                "Capital.Major.City","CPI.Change....","Largest.city",
                                "Maternal.mortality.ratio","Out.of.pocket.health.expenditure"))

#Get rid of any rows that are not 2021 in imports data
ImportsExp <- ImportsExp[ImportsExp$Year == "2021", ]
#Get rid of any columns that aren't total imports and exports
ImportsExp <- select(ImportsExp, 1:which(names(ImportsExp) == "Import..US..Thousand.")) 


#Standardizing the column name to COUNTRY for later dataframe merge.
names(BioData)[names(BioData) == 'country'] <- 'COUNTRY'
names(Econ_60_22)[names(Econ_60_22) == 'Country'] <- 'COUNTRY'
names(EconTour)[names(EconTour) == 'Country'] <- 'COUNTRY'
names(Econ2)[names(Econ2) == 'Country'] <- 'COUNTRY'
names(ImportsExp)[names(ImportsExp) == 'Partner.Name' ] <- 'COUNTRY'
names(LandL)[names(LandL) == 'country'] <- 'COUNTRY'


#Rename GDP to similar label from other data set
names(Econ2)[names(Econ2) == 'GDP' ] <- 'X2023'

FinalDF <- BioData %>%
  
  left_join(Econ_60_22, by = "COUNTRY") %>%
  
  left_join(Econ2, by = "COUNTRY") %>%
  
  left_join(ImportsExp, by = "COUNTRY") %>%
  
  left_join(LandL, by = "COUNTRY") 

#Convert GDP X2023 to numeric for average
FinalDF$X2023 <- as.numeric(gsub("[$,]", "", FinalDF$X2023)) 

#make a new column for the exponential moving average of years 2018 to 2022
#gdp from Econ-60-22 data set
FinalDF$Avg_21_23 <- rowMeans(FinalDF[,c("X2021","X2022","X2023")], na.rm =TRUE )

print(is.factor(FinalDF$COUNTRY))
# make COUNTRY column a factor
FinalDF$COUNTRY <- factor(FinalDF$COUNTRY)


# Filter for invasive species only
df_counts <- FinalDF %>% filter(isInvasive == "INVASIVE") %>%
  group_by(COUNTRY) %>% summarize(invasive_count = n(), .groups = "drop")

# Replace NA counts with 0 for countries with no invasive species and attach count of Invasive species in each country to FinalDF dataframe
FinalDF <- FinalDF %>% left_join(df_counts, by = "COUNTRY") %>%
  mutate(invasive_count = ifelse(is.na(invasive_count), 0, invasive_count))



FinalDF <-dplyr::select(FinalDF, -c("species"))

FinalDFdist <- FinalDF[,-c(1:4,6:9,12,13,16:19,21,23,24,25:27,28,30:37,38)]

AnalysisFinal <- distinct(FinalDFdist)

AnalysisFinal <- AnalysisFinal[,-c(2,3)]

AnalysisFinal[188, "Agricultural.Land...."] <- "45.09%"
AnalysisFinal[188, "Land.Area.Km2."] <- 9831510
AnalysisFinal[188, "Forested.Area...."] <- 33.87
AnalysisFinal[188, "X2023"] <- 2.772e+13
AnalysisFinal[188, "Import..US..Thousand."] <- 32
AnalysisFinal[188, "Population"] <- 334900000
AnalysisFinal[188, "landlocked"] <- "YES"
AnalysisFinal[188, "Avg_21_23"] <- 2.58e+13


#remove na rows
AnalysisFinalclean <- na.omit(AnalysisFinal)  


#remove rows with blank cells
AnalysisFinal2<- AnalysisFinalclean[-c(58,122,130,165)]

AnalysisFinal <- Final[,-c(1)]

AnalysisFinalclean <- na.omit(AnalysisFinal)  

AnalysisFinal2<- AnalysisFinalclean[-c(58,122,130,165)]

#Removing % sign chracters in Farmland and forested land variables
AnalysisFinal2$Agricultural.Land.... <- gsub("%", "", AnalysisFinal2$Agricultural.Land....) 
AnalysisFinal2$Forested.Area.... <- gsub("%", "", AnalysisFinal2$Forested.Area....) 

#removing commas and converting population to a numeric values
AnalysisFinal2$Population <- gsub(",", "", AnalysisFinal2$Population) 

print(AnalysisFinal2$Population)

AnalysisFinal2$Population <- as.numeric(AnalysisFinal2$Population)
print(typeof(AnalysisFinal2$Population))

AnalysisFinal2$Agricultural.Land.... <- as.numeric(AnalysisFinal2$Agricultural.Land....)
AnalysisFinal2$Avg_21_23 <- as.numeric(AnalysisFinal2$Avg_21_23)
print(typeof(AnalysisFinal2$Avg_21_23))


write.csv(AnalysisFinal2, "C:/Users/Nick/DSC 530/project data sets/FinalBioDF2.csv")

df1 <-  read.csv("FinalBioDF2.csv")
###################################

# explore histograms of our variables

ggplot(df1, aes(x = invasive_count)) +
  geom_histogram(color = "black", fill = "steelblue") +
  labs(x = "Invasive Species Count", y = "Frequency") +
  ggtitle("Histogram of Invasive Species Count (By Country)") +
  theme_minimal()

ggplot(df1, aes(x = Agricultural.Land....)) +
  geom_histogram(color = "black", fill = "darkblue") +
  labs(x = "Percent of Country land Agricultural", y = "Frequency") +
  ggtitle("Histogram of Percent Agricultural land") +
  theme_minimal()

ggplot(df1, aes(x = Population)) +
  geom_histogram(color = "black", fill = "red") +
  labs(x = "Population", y = "Frequency") +
  ggtitle("Histogram of Country Populations") +
  theme_minimal()
ggplot(df1, aes(x = Population)) +
  geom_histogram(color = "black", fill = "red") +
  coord_cartesian(xlim = c(0, 5e+08)) +
  labs(x = "Population", y = "Frequency") +
  ggtitle("Histogram of Country Populations") +
  theme_minimal()

ggplot(df1, aes(x = Forested.Area....)) +
  geom_histogram(color = "black", fill = "orange") +
  labs(x = "Percent of Country land Foresed", y = "Frequency") +
  ggtitle("Histogram of Percent Forested Area") +
  theme_minimal()

ggplot(df1, aes(x = Import..US..Thousand.)) +
  geom_histogram(color = "black", fill = "steelblue") +
  labs(x = "Imports value in (Thousands US $)", y = "Frequency") +
  ggtitle("Histogram of Imports") +
  theme_minimal()
ggplot(df1, aes(x = Import..US..Thousand.)) +
  geom_histogram(color = "black", fill = "steelblue") +
  coord_cartesian(xlim = c(0, 1.5e+09)) + 
  labs(x = "Imports value in (Thousands US $)", y = "Frequency") +
  ggtitle("Histogram of Imports") +
  theme_minimal()

ggplot(df1, aes(x = Avg_21_23)) +
  geom_histogram(color = "black", fill = "steelblue") +
  labs(x = "Average GDP 2021-2023", y = "Frequency") +
  ggtitle("Histogram of Average GDP 2021-2023") +
  theme_minimal()
ggplot(df1, aes(x = Avg_21_23)) +
  geom_histogram(color = "black", fill = "steelblue") +
  coord_cartesian(xlim = c(0, 0.5e+13)) + 
  labs(x = "Average GDP 2021-2023", y = "Frequency") +
  ggtitle("Histogram of Average GDP 2021-2023") +
  theme_minimal()



#Scatter plots

ggplot(df1, aes(x = invasive_count, y = Agricultural.Land....)) +
  geom_point()

ggplot(df1, aes(x = invasive_count, y = Population)) +
  geom_point()

ggplot(df1, aes(x = invasive_count, y = Forested.Area....)) +
  geom_point()

ggplot(df1, aes(x = invasive_count, y = Import..US..Thousand.)) +
  geom_point()

ggplot(df1, aes(x = invasive_count, y = Avg_21_23)) +
  geom_point()


cov(df1$invasive_count, df$Population)
cov(df1$invasive_count, df$Import..US..Thousand.)
cov(df1$invasive_count, df$Avg_21_23)


cor.test(df1$invasive_count, df$Agricultural.Land....)
cor.test(df1$invasive_count, df$Population)
cor.test(df1$invasive_count, df$Forested.Area....)
cor.test(df1$invasive_count, df$Import..US..Thousand.)
cor.test(df1$invasive_count, df$Avg_21_23)


#Model#########


#InvGDPLM models invasive count

InvGDPLM <- lm(invasive_count ~ Avg_21_23, data = df1)
summary(InvGDPLM)


#Our summary shows that the an adjusted r squared value of 0.574


resids <- residuals(InvGDPLM)
fit <- fitted(InvGDPLM)


linePlot <- ggplot(df1,  aes(x=invasive_count, y=Avg_21_23)) + geom_point() + stat_smooth(method=lm, formula=y ~ x, geom="smooth")
linePlot


qqnorm(resids)
qqline(resids)


preds <- predict(object = InvGDPLM, newdata = df1 )
rmse(fit, preds)

