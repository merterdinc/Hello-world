################################################################################ 
##################                 Thesis SoSe23             ###################
##################                                           ###################
################################################################################

#-------------------------------------------------------------------------------
# Surname: Erdinc
# Name: Mert
# Student ID (Matrikelnummer): 615804
#-------------------------------------------------------------------------------

setwd("C:/Users/Administrator/Downloads")


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(mlogit)
library(data.table)
library(dplyr)
library(mvtnorm)
library(gmnl)
library(corrplot)

# seperate the whole data from the completed data
whole_data <- read.csv("RawExportnew.csv")
data <- read.csv("RawExportnew.csv")

# create a subset of the fully completed answers
data <- subset(data, sys_RespStatus == 5)
quiters <- subset(data, sys_RespStatus == 2)

#classic checks

summary(data)
summary(data$sys_ElapsedTime)
hist(data$sys_ElapsedTime)
head(data)
str(data)
ncol(data)
nrow(data)
nrow(whole_data)

#CALCULATING THE RULE OF THUMB SAMPLE SIZE
#NTJ/P >= 500

nrow(data)*14*4/4 #5138 is the result this should be bigger than 500
375*14*4/4

#checking the frequency of which part of the survey respondents left

#create a frequency table of sys_LastQuestion
freq_table <- table(whole_data$sys_LastQuestion)
freq_table
freq_table <- as.data.frame(freq_table)

# create a subset of the freq_table
subset_table <- freq_table[freq_table$Var1 %in% c("Age", "Intro", "Education"), ]
subset_table
# sum the values in the Freq column of the subset_table
subset_sum <- sum(subset_table$Freq)

# print the result
subset_sum/nrow(whole_data) #35.44% left the survey before the choice sets

nrow(data)/nrow(whole_data) #41.67% of the respondents completed the survey



subset_table <- freq_table[freq_table$Var1 %in% c("AnyOtherAttribute", "AnythingElse", "SourceofInfo", "TotalSum"), ]
subset_table
# sum the values in the Freq column of the subset_table
subset_sum <- sum(subset_table$Freq)
subset_sum
# print the result
subset_sum/nrow(whole_data) #2.33% left after the CBC tasks


subset_table <- freq_table[freq_table$Var1 %in% c("CBCIntro", "CBC_Random1", "CBC_Random2", "CBC_Random3", "CBC_Random4", "CBC_Random5", "CBC_Random6", "CBC_Random7", "CBC_Random8", "CBC_Random9", "CBC_Random10", "CBC_Random11", "CBC_Random12", "CBC_Random13", "CBC_Random14"), ]

# sum the values in the Freq column of the subset_table
subset_sum <- sum(subset_table$Freq)

# print the result
subset_sum/nrow(whole_data) #20.56% left during the CBC tasks
#####################################################################


#Analysis of the demographic data

summary(data$Age)
hist(data$Age)
plot(density(data$Age))
boxplot(data$Age)

# Create a histogram of Age
hist(data$Age, breaks = 36, xlab = "Age", main = "Histogram of Age", cex.main = 1.5)


data$Gender <- factor(data$Gender,
                      levels = c(1, 2, 3),
                      labels = c("Male", "Female", "Other"))

table(data$Gender)
prop.table(table(data$Gender)) * 100



data$Education <- factor(data$Education,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c("High school", "Bachelors Degree", "Master's Degree", "Doctorate degree", "Other" ))


table(data$Education)
prop.table(table(data$Education)) * 100



data$EmploymentStatus <- factor(data$EmploymentStatus,
                                levels = c(1, 2, 3, 4, 5, 6),
                                labels = c("Employed Fulltime (including Self-Employed)", "Employed Parttime", "Seeking Opportunities", "Student", "Retired", "Prefer not to answer" ))

table(data$EmploymentStatus)
prop.table(table(data$EmploymentStatus)) * 100



#Which Country do you live in?

# Example country codes and names
country_codes <- 1:196
country_names <- c(
  "Afghanistan  (AFG)", "Albania  (ALB)", "Algeria  (DZA)", "Andorra  (AND)", "Angola  (AGO)",
  "Antigua and Barbuda  (ATG)", "Argentina  (ARG)", "Armenia  (ARM)", "Australia  (AUS)", "Austria  (AUT)",
  "Azerbaijan  (AZE)", "Bahamas (BHS)", "Bahrain  (BHR)", "Bangladesh  (BGD)", "Barbados  (BRB)",
  "Belarus  (BLR)", "Belgium  (BEL)", "Belize  (BLZ)", "Benin  (BEN)", "Bhutan  (BTN)",
  "Bolivia  (BOL)", "Bosnia and Herzegovina  (BIH)", "Botswana  (BWA)", "Brazil  (BRA)",
  "Brunei  (BRN)", "Bulgaria  (BGR)", "Burkina Faso  (BFA)", "Burma  (MMR)", "Burundi  (BDI)",
  "Cabo Verde  (CPV)", "Cambodia  (KHM)", "Cameroon  (CMR)", "Canada  (CAN)", "Central African Republic  (CAF)",
  "Chad  (TCD)", "Chile  (CHL)", "China  (CHN)", "Colombia  (COL)", "Comoros  (COM)",
  "Congo (Brazzaville)  (COG)", "Congo (Kinshasa)  (COD)", "Costa Rica  (CRI)", "Côte d'Ivoire  (CIV)",
  "Croatia  (HRV)", "Cuba  (CUB)", "Cyprus  (CYP)", "Czech Republic (CZE)", "Denmark  (DNK)",
  "Djibouti  (DJI)", "Dominica  (DMA)", "Dominican Republic  (DOM)", "Ecuador  (ECU)",
  "Egypt  (EGY)", "El Salvador  (SLV)", "Equatorial Guinea  (GNQ)", "Eritrea  (ERI)",
  "Estonia  (EST)", "Ethiopia  (ETH)", "Federated States of  ()", "Fiji  (FJI)",
  "Finland  (FIN)", "France  (FRA)", "Gabon  (GAB)", "Gambia (GMB)", "Georgia  (GEO)", "Germany  (DEU)",
  "Ghana  (GHA)", "Greece  (GRC)", "Grenada  (GRD)", "Guatemala  (GTM)", "Guinea  (GIN)",
  "Guinea-Bissau  (GNB)", "Guyana  (GUY)", "Haiti  (HTI)", "Holy See  (VAT)", "Honduras  (HND)",
  "Hungary  (HUN)", "Iceland  (ISL)", "India  (IND)", "Indonesia  (IDN)", "Iran  (IRN)", "Iraq  (IRQ)",
  "Ireland  (IRL)", "Israel  (ISR)", "Italy  (ITA)", "Jamaica  (JAM)", "Japan  (JPN)", "Jordan  (JOR)",
  "Kazakhstan  (KAZ)", "Kenya  (KEN)", "Kiribati  (KIR)", "Kosovo  (XKS)", "Kuwait  (KWT)",
  "Kyrgyzstan  (KGZ)", "Laos  (LAO)", "Latvia  (LVA)", "Lebanon  (LBN)", "Lesotho  (LSO)",
  "Liberia  (LBR)", "Libya  (LBY)", "Liechtenstein  (LIE)", "Lithuania  (LTU)", "Luxembourg  (LUX)",
  "Macedonia  (MKD)", "Madagascar  (MDG)", "Malawi  (MWI)", "Malaysia  (MYS)", "Maldives  (MDV)",
  "Mali  (MLI)", "Malta  (MLT)", "Marshall Islands  (MHL)", "Mauritania  (MRT)", "Mauritius  (MUS)",
  "Mexico  (MEX)", "Micronesia (FSM)", "Moldova  (MDA)", "Monaco  (MCO)", "Mongolia  (MNG)",
  "Montenegro  (MNE)", "Morocco  (MAR)", "Mozambique  (MOZ)", "Namibia  (NAM)", "Nauru  (NRU)",
  "Nepal  (NPL)", "Netherlands  (NLD)", "New Zealand  (NZL)", "Nicaragua  (NIC)", "Niger  (NER)",
  "Nigeria  (NGA)", "North Korea (PRK)", "Norway  (NOR)", "Oman  (OMN)", "Pakistan  (PAK)",
  "Palau  (PLW)", "Panama  (PAN)", "Papua New Guinea  (PNG)", "Paraguay  (PRY)", "Peru  (PER)",
  "Philippines  (PHL)", "Poland  (POL)", "Portugal  (PRT)", "Qatar  (QAT)", "Romania  (ROU)",
  "Russia  (RUS)", "Rwanda  (RWA)", "Saint Kitts and Nevis  (KNA)", "Saint Lucia  (LCA)",
  "Saint Vincent and the Grenadines  (VCT)", "Samoa  (WSM)", "San Marino  (SMR)", "Sao Tome and Principe  (STP)",
  "Saudi Arabia  (SAU)", "Senegal  (SEN)", "Serbia  (SRB)", "Seychelles  (SYC)", "Sierra Leone  (SLE)",
  "Singapore  (SGP)", "Slovakia  (SVK)", "Slovenia  (SVN)", "Solomon Islands  (SLB)", "Somalia  (SOM)",
  "South Africa  (ZAF)", "South Korea (KOR)", "South Sudan  (SSD)", "Spain  (ESP)", "Sri Lanka  (LKA)",
  "Sudan  (SDN)", "Suriname  (SUR)", "Swaziland  (SWZ)", "Sweden  (SWE)", "Switzerland  (CHE)",
  "Syria  (SYR)", "Tajikistan  (TJK)", "Tanzania  (TZA)", "Thailand  (THA)", "Timor-Leste  (TLS)",
  "Togo  (TGO)", "Tonga  (TON)", "Trinidad and Tobago  (TTO)", "Tunisia  (TUN)", "Turkey  (TUR)",
  "Turkmenistan  (TKM)", "Tuvalu  (TUV)", "Uganda  (UGA)", "Ukraine  (UKR)", "United Arab Emirates  (ARE)",
  "United Kingdom  (GBR)", "United States  (USA)", "Uruguay  (URY)", "Uzbekistan  (UZB)",
  "Vanuatu  (VUT)", "Venezuela  (VEN)", "Vietnam  (VNM)", "Yemen  (YEM)", "Zambia  (ZMB)", "Zimbabwe  (ZWE)")

# Create a factor variable from the country codes
country_factor <- factor(country_codes, levels = 1:196, labels = country_names)

# Create a factor variable from the WhichCountry variable
data$WhichCountry <- factor(data$WhichCountry, levels = country_codes, labels = country_names)

# View the levels of the factor variable
levels(data$WhichCountry)


# Analyze the country data
# Create a table of the WhichCountry variable
country_table <- table(data$WhichCountry)

# Subset the table to include only levels with a count greater than 0
subset_table <- country_table[country_table > 0]

# View the subset table
subset_table
prop.table(subset_table) * 100

data$CountryofResidence <- data$WhichCountry
data$CountryofResidence <- ifelse(data$CountryofResidence == "Germany  (DEU)", "Germany",
                                  ifelse(data$CountryofResidence == "Turkey  (TUR)", "Turkey", "Other"))
data$CountryofResidence <- factor(data$CountryofResidence, levels = c("Germany", "Turkey", "Other"))

table(data$CountryofResidence)


# Create a factor variable from the CountryofOrigin variable
data$CountryofOrigin <- factor(data$CountryofOrigin, levels = country_codes, labels = country_names)

# Create a table of the CountryofOrigin variable
country_table <- table(data$CountryofOrigin)

# Subset the table to include only levels with a count greater than 0
subset_table <- country_table[country_table > 0]

# View the subset table
subset_table

prop.table(subset_table) * 100

data$countryoforigin <- data$CountryofOrigin
data$countryoforigin <- ifelse(data$countryoforigin == "Germany  (DEU)", "Germany",
                               ifelse(data$countryoforigin == "Turkey  (TUR)", "Turkey", "Other"))
data$countryoforigin <- factor(data$countryoforigin, levels = c("Germany", "Turkey", "Other"))

table(data$countryoforigin)
table(data$CountryofOrigin)



# Rename the variables
col_names <- c("Full-time", "Part-time", "Seeking Opportunities", "Student", "Retired", "No answer")

# Modify the "EmploymentStatus" column
data_subset <- recode(data$EmploymentStatus,
                              "Employed Fulltime (including Self-Employed)" = "Full-time",
                              "Employed Parttime" = "Part-time",
                              "Seeking Opportunities" = "Seeking Opportunities",
                              "Student" = "Student",
                              "Retired" = "Retired",
                              "Prefer not to answer" = "No answer")
#visualizing the frequency
column_frequency <- table(data_subset)
column_frequency

frequency_df <- data.frame(
  column = names(column_frequency),
  frequency = as.numeric(column_frequency)
)
ggplot<-ggplot(frequency_df, aes(x = column, y = frequency)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  xlab("Employment Status") +
  ylab("Frequency") +
  ggtitle("Frequency of Employment Status"
          )
ggplot

ggplot + theme(panel.background = element_rect(fill = 'white', colour = 'white'))+theme(plot.title = element_text(hjust = 0.5)+
                                                                                          theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20))))


##################################################################################################
###########################################################################################
#SMARTPHONE RELATED QUESTIONS


#Which brand of smartphone do you own?


# Create a table for the first column
table1 <- table(data$WhichBrand_1)

# Create a table for the second column
table2 <- table(data$WhichBrand_2)

# Create a table for the third column
table3 <- table(data$WhichBrand_3)

# Create a table for the fourth column
table4 <- table(data$WhichBrand_4)

# Create a table for the fifth column
table5 <- table(data$WhichBrand_5)

# Create a table for the sixth column
table6 <- table(data$WhichBrand_6)

# Create a table for the seventh column
table7 <- table(data$WhichBrand_7)

# Create a table for the eight column
table8 <- table(data$WhichBrand_7_other)
table1 # Apple #205
table2 # Samsung #89
table3 # Xiaomi #34
table4 # Huawei #19
table5 # Oppo #4
table6 # Vivo #0
table7 # Other # 38 
table8 # Other options # HTC1, Fairphone1, Shift 1, LG1, Nokia1
       #Sony2, Google12, ZTE1, Oneplus9, Motorola3, Xiaomi Redmi 2, Honor1, Cubot1,



#Anyotherattribute important in addition to the CBC attributes?


# Create a table for the first column
table1 <- table(data$AnyOtherAttribute_1)

# Create a table for the second column
table2 <- table(data$AnyOtherAttribute_2)

# Create a table for the third column
table3 <- table(data$AnyOtherAttribute_3)

# Create a table for the fourth column
table4 <- table(data$AnyOtherAttribute_4)

# Create a table for the fifth column
table5 <- table(data$AnyOtherAttribute_5)

# Create a table for the sixth column
table6 <- table(data$AnyOtherAttribute_6)

# Create a table for the seventh column
table7 <- table(data$AnyOtherAttribute_7)

# Create a table for the eight column
table8 <- table(data$AnyOtherAttribute_8)


table1 # Design
table1 / sum(table1) * 100

table2 # Processor
table2 / sum(table2) * 100

table3 # Display resolution
table3 / sum(table3) * 100

table4 # Audio/sound system
table4 / sum(table4) * 100

table5 # Headphone jack/USB port
table5 / sum(table5) * 100

table6 # Charging speed
table6 / sum(table6) * 100

table7 # Other
table7 / sum(table7) * 100

table8 # None of the above
table8 / sum(table8) * 100



#Correlation test was done to see if respondents who find a particular 'other'
#attribute important also find another 'other' attribute important
# Select the columns of interest
columns <- c("AnyOtherAttribute_1", "AnyOtherAttribute_2", "AnyOtherAttribute_3",
             "AnyOtherAttribute_4", "AnyOtherAttribute_5", "AnyOtherAttribute_6",
             "AnyOtherAttribute_7", "AnyOtherAttribute_8")

# Perform correlation tests
cor_tests <- lapply(columns, function(col) cor.test(data[[col]], data$AnyOtherAttribute_6, method = "pearson"))

# Extract correlation coefficients and p-values
cor_coef <- sapply(cor_tests, function(x) x$estimate)
p_values <- sapply(cor_tests, function(x) x$p.value)

# Create a data frame to display results
results <- data.frame(Correlation = cor_coef, P_value = p_values)

# Print the results
print(results)



#Analysis of the TotalSum Question: If you had to allocate 100 points to the 
#following (CBC) attributes in terms of their importance, how would you allocate them?


summary(data[, c("TotalSum_1", "TotalSum_2", "TotalSum_3", "TotalSum_4",
                 "TotalSum_5", "TotalSum_6")])

# Rename the variables
col_names <- c("Brand", "Price", "Screensize", "Battery", "Storage", "Camera")
data_subset <- data[, c("TotalSum_1", "TotalSum_2", "TotalSum_3", "TotalSum_4", "TotalSum_5", "TotalSum_6")]
colnames(data_subset) <- col_names

# Create the boxplot
boxplot(data_subset, main = "Respondent Indicated Importances",
        xlab = "Variable", ylab = "Importance",
        col = "lightblue", border = "black")

# Customize the appearance
axis(1, at = 1:length(col_names), labels = col_names)

# Adjust the plot margins and font size
par(cex.axis = 0.8)



#Correlation test was made in order to check if some respondents that find
#a certain (CBC) attribute important finds some others important as well

# Compute correlation matrix
cor_matrix <- cor(data_subset, use = "pairwise.complete.obs")
cor_matrix
# Perform Pearson correlation test
corr_test <- cor.test(data_subset$Brand, data_subset$Price)

# Check if the correlation is significant
if (corr_test$p.value < 0.05) {
  print("The correlation between Brand and Price is significant.")
} else {
  print("The correlation between Brand and Price is not significant.")
}

# Create correlation plot
corrplot::corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.8)

cor_pvalues <- function(cor_matrix, n) {
  r <- cor_matrix
  df <- n - 2
  p <- 2 * (1 - pt(abs(r) * sqrt(df) / sqrt(1 - r^2), df))
  p
}

# Check for significance
p_values <- cor_pvalues(cor_matrix, n = nrow(data_subset))
significant_cor <- cor_matrix * (p_values < 0.05)

# Display significant correlations
print(significant_cor)

# Identify significant correlations
significance_level <- 0.05
significant_corr <- cor_matrix * (p_values < significance_level)

# Print significant correlations
print(significant_corr)



#Question of: For how long have you had your current phone?
table(data$ForHowLong)
prop.table(table(data$ForHowLong))
#1  2  3  4  5  6 
#54 56 86 84 55 40
distribution <- table(data$ForHowLong)

barplot(distribution, xlab = "ForHowLong", ylab = "Frequency", main = "Distribution of ForHowLong")


#How often do you purchase a new smartphone?
table(data$HowOften)
prop.table(table(data$HowOften))

#1  2  3  4  5  6 
#5 17 93 109 103 48


#Potential buyers
Potentialbuyers <- data[data$ForHowLong %in% c(1, 2) & data$HowOften == 1 |
                          data$ForHowLong == 3 & data$HowOften == 2 |
                          data$ForHowLong == 4 & data$HowOften == 3 |
                          data$ForHowLong == 5 & data$HowOften == 4 |
                          data$ForHowLong == 6 & (data$HowOften == 5 | data$HowOften == 6), ]


nrow(Potentialbuyers)


#Respondents' primary source for researching and gathering information
#about smartphones before making a purchase

# Create a table for the first column
table1 <- table(data$SourceofInfo_1)

# Create a table for the second column
table2 <- table(data$SourceofInfo_2)

# Create a table for the third column
table3 <- table(data$SourceofInfo_3)

# Create a table for the fourth column
table4 <- table(data$SourceofInfo_4)

# Create a table for the fifth column
table5 <- table(data$SourceofInfo_5)

# Create a table for the sixth column
table6 <- table(data$SourceofInfo_6)

# Create a table for the seventh column
table7 <- table(data$SourceofInfo_7)

# Create a table for the eight column
table8 <- table(data$SourceofInfo_7_other)


table1 # Online reviews 263
table2 # Family and friends 143
table3 # In-store demos 63
table4 # Word-of-mouth recommendations 72
table5 # Advertisements 29
table6 # Own research 229
table7 # Other 9
table8 # Other answers

columns <- c("SourceofInfo_1", "SourceofInfo_2", "SourceofInfo_3", "SourceofInfo_4",
             "SourceofInfo_5", "SourceofInfo_6", "SourceofInfo_7")

#Checking if some source of information correlates with another type of 
#source of information
# Perform correlation tests
cor_tests <- lapply(columns, function(col) cor.test(data[[col]], 
                                      data$SourceofInfo_5, method = "pearson"))

# Extract correlation coefficients and p-values
cor_coef <- sapply(cor_tests, function(x) x$estimate)
p_values <- sapply(cor_tests, function(x) x$p.value)

# Create a data frame to display results
results <- data.frame(Correlation = cor_coef, P_value = p_values)

# Print the results
print(results)




#The question of: How much time do you spend on your phone per day on average?


table(data$TimeonPhone)
#1  2  3  4  5  6 
#10 38 80 93 73 81 

#Checking average time on phone based on genders
head(data)
Males <- subset(data,Gender == "Male")
nrow(Males)
summary(Males)
summary(Females)
prop.table(table(Males$TimeonPhone))
Females <- subset(data,Gender == "Female")
nrow(Females)
prop.table(table(Females$TimeonPhone))


#Checking time on phone and age
less_one_hour <- subset(data,TimeonPhone == 1)
onetotwo <- subset(data,TimeonPhone == 2)
twotothree <- subset(data,TimeonPhone == 3)
threetofour <- subset(data,TimeonPhone == 4)
fourtofive <- subset(data,TimeonPhone == 5)
morethanfive <- subset(data,TimeonPhone == 6)
summary(less_one_hour$Age)
less_one_hour$Age
summary(onetotwo$Age)
summary(twotothree$Age)
summary(threetofour$Age)
summary(fourtofive$Age)
summary(morethanfive$Age)


#Checking nationality and time on phone
Turkish <- subset(data,countryoforigin == "Turkey")
German <- subset(data,countryoforigin == "Germany")
prop.table(table(Turkish$TimeonPhone))
prop.table(table(German$TimeonPhone))


#Grouping respondents according to their phone brands
Apple_users <- subset(data,WhichBrand_1 == 1)
Samsung_users <- subset(data,WhichBrand_2 == 1)
Xiaomi_users <- subset(data,WhichBrand_3 == 1)
Huawei_users <- subset(data,WhichBrand_4 == 1)
Oppo_users <- subset(data,WhichBrand_5 == 1)
Vivo_users <- subset(data,WhichBrand_6 == 1)
Other_users <- subset(data,WhichBrand_7 == 1)
Google_users <- subset(data,WhichBrand_7_other == "Google")
Oneplus_users <- subset(data, WhichBrand_7_other %in% c("Oneplus", "One plus"))



#Analyzing which brand users are satisfied with their phones and which are not
#BrandLikelihood_1: I will stick to the current brand
#BrandLikelihood_2: I may switch the brand

plot(density(data$BrandLikelihood_1))
plot(density(data$BrandLikelihood_2))

table(data$BrandLikelihood_1)
nonswitchers <- subset(data, BrandLikelihood_1 == 100)
columns <- c("WhichBrand_1", "WhichBrand_2", "WhichBrand_3", "WhichBrand_4",
             "WhichBrand_5", "WhichBrand_6", "WhichBrand_7")
counts <- colSums(nonswitchers[, columns])
print(counts)
nrow(Apple_users)

table(data$BrandLikelihood_2)
summary(Apple_users$BrandLikelihood_1)
summary(Apple_users$BrandLikelihood_2)

summary(Samsung_users$BrandLikelihood_1)
summary(Samsung_users$BrandLikelihood_2)

summary(Xiaomi_users$BrandLikelihood_1)
summary(Xiaomi_users$BrandLikelihood_2)

summary(Huawei_users$BrandLikelihood_1)
summary(Huawei_users$BrandLikelihood_2)

summary(Oppo_users$BrandLikelihood_1)
summary(Oppo_users$BrandLikelihood_2)

summary(Google_users$BrandLikelihood_1)
summary(Google_users$BrandLikelihood_2)

summary(Oneplus_users$BrandLikelihood_1)
summary(Oneplus_users$BrandLikelihood_2)


summary(data$BrandLikelihood_1) ##1 I will stick to the current brand mean 73.55
summary(data$BrandLikelihood_2) ##2 I may switch the brand mean 26.45

#Checking brand satisfaction based on education level
means <- aggregate(data$BrandLikelihood_1, by = list(Education = data$Education), FUN = mean)
# Print the mean values
print(means)


#Question: "If you think you might switch to a different phone brand,
#which brand or brands might it be?"

# Create a table for the first column
table1 <- table(data$PossibleSwitch_1)

# Create a table for the second column
table2 <- table(data$PossibleSwitch_2)

# Create a table for the third column
table3 <- table(data$PossibleSwitch_3)

# Create a table for the fourth column
table4 <- table(data$PossibleSwitch_4)

# Create a table for the fifth column
table5 <- table(data$PossibleSwitch_5)

# Create a table for the sixth column
table6 <- table(data$PossibleSwitch_6)

# Create a table for the seventh column
table7 <- table(data$PossibleSwitch_7)

# Create a table for the eight column
table8 <- table(data$PossibleSwitch_7_other)


table1 # Apple 92
table2 # Samsung 116
table3 # Xiaomi 57
table4 # Huawei 40
table5 # Oppo 30
table6 # Vivo 13
table7 # I am satisfied with my current choice of phone 41
table8 # Other stuff and not sures


#Calculating how many of the each brand owners think of switching to different brands

data_subset <- subset(Samsung_users, select = c(PossibleSwitch_1, PossibleSwitch_2,
                                                PossibleSwitch_3,PossibleSwitch_4,
                                                PossibleSwitch_5,PossibleSwitch_6,
                                                PossibleSwitch_7, PossibleSwitch_8))
counts <- colSums(data_subset)
print(counts)
print(counts)/nrow(Samsung_users)








######################################################
######################################################
#Preparing the data for the utility estimation

cbc <- read.csv("CBC.csv")

summary(cbc)
str(cbc)

has_duplicates <- any(duplicated(cbc))
has_duplicates #no duplicates in the data set

head(cbc)
cbc_copy <- cbc
cbc_copy1 <- cbc

#Dummy coding the variables
cbc$Apple <- ifelse(cbc$brand == 1,1,0)
cbc$Samsung <- ifelse(cbc$brand == 2,1,0)
cbc$Xiaomi <- ifelse(cbc$brand == 3,1,0)
cbc$Oppo <- ifelse(cbc$brand == 4,1,0)

# Replace values in price column
cbc$Price <- ifelse(cbc$price == 1, 650, 
                    ifelse(cbc$price == 2, 950, 
                           ifelse(cbc$price == 3, 1150, cbc$price)))

cbc$price_650 <- ifelse(cbc$price == 1,1,0)
cbc$price_950 <- ifelse(cbc$price == 2,1,0)
cbc$price_1150 <- ifelse(cbc$price == 3,1,0)


cbc$small_screen <- ifelse(cbc$screen == 1,1,0)
cbc$middle_screen <- ifelse(cbc$screen == 2,1,0)
cbc$big_screen <- ifelse(cbc$screen == 3,1,0)


cbc$short_battery <- ifelse(cbc$battery == 1,1,0)
cbc$medium_battery <- ifelse(cbc$battery == 2,1,0)
cbc$long_battery <- ifelse(cbc$battery == 3,1,0)

cbc$small_storage <- ifelse(cbc$storage == 1,1,0)
cbc$medium_storage <- ifelse(cbc$storage == 2,1,0)
cbc$big_storage <- ifelse(cbc$storage == 3,1,0)

cbc$low_quality <- ifelse(cbc$camera == 1,1,0)
cbc$medium_quality <- ifelse(cbc$camera == 2,1,0)
cbc$high_quality <- ifelse(cbc$camera == 3,1,0)

cbc$price_scaled<-cbc$Price/100

head(cbc)

cbc_copy2 <- cbc
cbc_copy3 <- cbc 


#################################################################
#################################################################
#Estimation

#Preparing the CBC data

# Update the values in the "none" column based on the condition
cbc$none <- 0
cbc$none[(seq_along(cbc$none) - 1) %% 5 == 4] <- 1

head(cbc)

str(cbc)

cbc$Apple <- as.integer(cbc$Apple)
cbc$Samsung <- as.integer(cbc$Samsung)
cbc$Xiaomi <- as.integer(cbc$Xiaomi)
cbc$Oppo <- as.integer(cbc$Oppo)

cbc$small_screen <- as.integer(cbc$small_screen)
cbc$middle_screen <- as.integer(cbc$middle_screen)
cbc$big_screen <- as.integer(cbc$big_screen)
cbc$short_battery <- as.integer(cbc$short_battery)
cbc$medium_battery <- as.integer(cbc$medium_battery)
cbc$long_battery <- as.integer(cbc$long_battery)
cbc$small_storage <- as.integer(cbc$small_storage)
cbc$medium_storage <- as.integer(cbc$medium_storage)
cbc$big_storage <- as.integer(cbc$big_storage)
cbc$low_quality <- as.integer(cbc$low_quality)
cbc$medium_quality <- as.integer(cbc$medium_quality)
cbc$high_quality <- as.integer(cbc$high_quality)
cbc$price_650 <- as.integer(cbc$price_650)
cbc$price_950 <- as.integer(cbc$price_950)
cbc$price_1150 <- as.integer(cbc$price_1150)
cbc$none <- as.integer(cbc$none)
cbc$Response <- as.integer(cbc$Response)


cbc_copy4 <- cbc
cbc_copy5 <- cbc


#Preparing the data so that the estimation function can use it


chid <- unique(cbc_copy4[, c("sys_RespNum", "Task")])
head(chid)
dim(chid)
chid$chid <- 1:nrow(chid)

cbc_copy4 <- merge(chid, cbc_copy4, by = c("sys_RespNum", "Task"))
head(cbc_copy4)

cbc_copy4 <- mlogit.data(cbc_copy4, 
                         choice = "Response", 
                         shape = "long",
                         id.var = "sys_RespNum",
                         chid.var = "chid")


#Fixing a column name
new_names <- c("WhichCountry")
old_names <- c("CountryofResidence")
names(data)[match(old_names, names(data))] <- new_names


#Gathering all non-conjoint data
other_data <- data[, c(1,48:55, 70:118)]

head(other_data)

#Coding the variables numerically
other_data$Gender <- as.integer(other_data$Gender,
                                levels = c(1, 2, 3),
                                labels = c("Male", "Female", "Other"))

other_data$Education <- as.integer(other_data$Education,
                                   levels = c(1, 2, 3, 4, 5),
                                   labels = c("High school", "Bachelors Degree", "Master's Degree", "Doctorate degree", "Other"))

other_data$EmploymentStatus <- as.integer(other_data$EmploymentStatus,
                                          levels = c(1, 2, 3, 4, 5, 6),
                                          labels = c("Employed Fulltime (including Self-Employed)", "Employed Parttime", "Seeking Opportunities", "Student", "Retired", "Prefer not to answer" ))

head(other_data)

#Seperating the text data from other data
textdata <- other_data[, c("PossibleSwitch_7_other", "AnythingElse", "SourceofInfo_7_other", "WhichBrand_7_other", "AnyOtherAttribute_7_other", "Education_5_other", "Gender_3_other")]
other_data <- other_data[, !(names(other_data) %in% c("PossibleSwitch_7_other", "AnythingElse", "SourceofInfo_7_other", "WhichBrand_7_other", "AnyOtherAttribute_7_other", "Education_5_other", "Gender_3_other"))]


head(other_data)


#Checking the correlation between age and respondent-indicated importances
correlation <- cor(other_data[, 2], other_data[, 8:13])
correlation

p_values <- cor.test(other_data[, 2], other_data[, 13])$p.value
significant <- p_values < 0.05
significant

#Checking the correlation between gender and respondent-indicated importances
correlation <- cor(other_data$Gender, other_data[, 8:13])
correlation
p_values <- cor.test(other_data$Gender, other_data[, 11])$p.value #girls may find bat life less imp
significant <- p_values < 0.05
significant



#Checking the correlation between time on phone and respondent-indicated importances
correlation <- cor(other_data$TimeonPhone, other_data[, 8:13])
correlation

p_values <- cor.test(other_data$TimeonPhone, other_data[, 13])$p.value
significant <- p_values < 0.05
significant


######################################
######################################
#random coefficient logit model

load("mxl_smartphones_ln.dist.price_scaled_ln_100")


cbc_copy4$ln.dist.price <- cbc_copy4$price*-1


mxl_smartphones_ln.dist.price_scaled_ln_100 <- gmnl(Response ~ -1 + none + ln.dist.price_scaled + 
                                                      Apple + Samsung + Xiaomi +  
                                                      middle_screen + big_screen + 
                                                      medium_battery + long_battery +
                                                      medium_storage + big_storage +
                                                      medium_quality + high_quality, 
                                                    data = cbc_copy4, 
                                                    model = "mixl", correlation = FALSE, 
                                                    haltons = NULL, R = 100, panel = TRUE, 
                                                    tol = 1e-12, print.level = 1,
                                                    ranp = c(none = "n", ln.dist.price_scaled = "ln", Apple = "n", Samsung = "n", 
                                                             Xiaomi = "n", middle_screen = "n", 
                                                             big_screen = "n", medium_battery = "n", 
                                                             long_battery = "n", medium_storage = "n", 
                                                             big_storage = "n", medium_quality = "n",
                                                             high_quality = "n"))
summary(mxl_smartphones_ln.dist.price_scaled_ln_100) #-4853.3

#likelihood index ratio of the model above
1-((-4853.3)/(log(0.20)*(375*14-1))) #0.42#this model seems okay actually, the prices are dist. better


mxl_betai = effect.gmnl(mxl_smartphones_ln.dist.price_scaled_ln_100)$mean
mxl_betai<-as.data.frame(mxl_betai)
names(mxl_betai)[names(mxl_betai) == "ln.dist.price_scaled"] <- "price"
mxl_betai[,2]<-mxl_betai[,2]*-1
hist(mxl_betai[,2])

themodel <- mxl_smartphones_ln.dist.price_scaled_ln_100

nrow(mxl_betai)
ncol(mxl_betai)






#CBC TABLE CONSTRUCTION

br1 <- mxl_betai$Apple              
br2 <- mxl_betai$Samsung            
br3 <- mxl_betai$Xiaomi

middle_screen       <- mxl_betai$middle_screen      
big_screen          <- mxl_betai$big_screen         
medium_battery      <- mxl_betai$medium_battery     
long_battery        <- mxl_betai$long_battery       
medium_storage      <- mxl_betai$medium_storage     
big_storage         <- mxl_betai$big_storage        
medium_quality      <- mxl_betai$medium_quality     
high_quality        <- mxl_betai$high_quality       

pw_Oppo <- -(br1+br2+br3)/4
pw_Apple <- br1+pw_Oppo    
pw_Samsung <- br2+pw_Oppo
pw_Xiaomi <- br3+pw_Oppo
cbc_table_brand_pw <- data.frame(pw_Apple, pw_Samsung, pw_Xiaomi, pw_Oppo)
head(cbc_table_brand_pw)
cbc_table <- data.frame(pw_Apple, pw_Samsung, pw_Xiaomi, pw_Oppo)
head(cbc_table)
summary(cbc_table_brand_pw)



pw_small_screen <- -(middle_screen+big_screen)/3
pw_middle_screen <- middle_screen+pw_small_screen    
pw_big_screen <- big_screen+pw_small_screen

cbc_table_screen_pw <- data.frame(pw_small_screen, pw_middle_screen, pw_big_screen)
head(cbc_table_screen_pw)
cbc_table$pw_small_screen <- pw_small_screen
cbc_table$pw_middle_screen <- pw_middle_screen
cbc_table$pw_big_screen <- pw_big_screen
head(cbc_table)
summary(cbc_table_screen_pw)



pw_short_battery <- -(medium_battery+long_battery)/3
pw_medium_battery <- medium_battery+pw_short_battery    
pw_long_battery <- long_battery+pw_short_battery

cbc_table_battery_pw <- data.frame(pw_short_battery, pw_medium_battery, pw_long_battery)
head(cbc_table_battery_pw)
cbc_table$pw_short_battery <- pw_short_battery
cbc_table$pw_medium_battery <- pw_medium_battery
cbc_table$pw_long_battery <- pw_long_battery
summary(cbc_table_screen_pw)
head(cbc_table)


pw_small_storage <- -(medium_storage+big_storage)/3
pw_medium_storage <- medium_storage+pw_small_storage    
pw_big_storage <- big_storage+pw_small_storage

cbc_table_storage_pw <- data.frame(pw_small_storage, pw_medium_storage, pw_big_storage)
head(cbc_table_storage_pw)
cbc_table$pw_small_storage <- pw_small_storage
cbc_table$pw_medium_storage <- pw_medium_storage
cbc_table$pw_big_storage <- pw_big_storage
head(cbc_table)
summary(cbc_table_storage_pw)


pw_low_quality <- -(medium_quality+high_quality)/3
pw_medium_quality <- medium_quality+pw_low_quality    
pw_high_quality <- high_quality+pw_low_quality

cbc_table_camera_pw <- data.frame(pw_low_quality, pw_medium_quality, pw_high_quality)
cbc_table$pw_low_quality <- pw_low_quality
cbc_table$pw_medium_quality <- pw_medium_quality
cbc_table$pw_high_quality <- pw_high_quality
head(cbc_table)
summary(cbc_table_camera_pw)

range_price<-mxl_betai$price*(-5)
head(range_price)

cbc_table$range_price <- range_price
head(cbc_table)

#The left-out attribute levels (for the calculation of range and willingness to pay)
mxl_betai$Oppo<-0  
mxl_betai$small_screen<-0   
mxl_betai$short_battery<-0
mxl_betai$small_storage<-0
mxl_betai$low_quality<-0


max.brand<-apply(mxl_betai[,c("Apple","Samsung","Xiaomi","Oppo")],1,max)
min.brand<-apply(mxl_betai[,c("Apple","Samsung","Xiaomi","Oppo")],1,min)

range_brand<-max.brand-min.brand

cbc_table$range_brand <- range_brand


max.screen<-apply(mxl_betai[,c("small_screen","middle_screen","big_screen")],1,max)
min.screen<-apply(mxl_betai[,c("small_screen","middle_screen","big_screen")],1,min)

range_screen<-max.screen-min.screen

cbc_table$range_screen <- range_screen



max.battery<-apply(mxl_betai[,c("short_battery","medium_battery","long_battery")],1,max)
min.battery<-apply(mxl_betai[,c("short_battery","medium_battery","long_battery")],1,min)

range_battery<-max.battery-min.battery

cbc_table$range_battery <- range_battery


max.storage<-apply(mxl_betai[,c("small_storage","medium_storage","big_storage")],1,max)
min.storage<-apply(mxl_betai[,c("small_storage","medium_storage","big_storage")],1,min)

range_storage<-max.storage-min.storage

cbc_table$range_storage <- range_storage


max.quality<-apply(mxl_betai[,c("low_quality","medium_quality","high_quality")],1,max)
min.quality<-apply(mxl_betai[,c("low_quality","medium_quality","high_quality")],1,min)

range_quality<-max.quality-min.quality

cbc_table$range_quality <- range_quality



ranges <- cbind(range_price, range_brand, range_screen, range_battery, range_storage, range_quality)

sum.range <- (cbc_table$range_price + cbc_table$range_brand +
                cbc_table$range_screen + cbc_table$range_battery +
                cbc_table$range_storage + cbc_table$range_quality)

imp_price   <- cbc_table$range_price/sum.range
imp_brand   <- cbc_table$range_brand/sum.range
imp_screen   <- cbc_table$range_screen/sum.range
imp_battery   <- cbc_table$range_battery/sum.range
imp_storage   <- cbc_table$range_storage/sum.range
imp_quality   <- cbc_table$range_quality/sum.range


cbc_table$imp_price   <- cbc_table$range_price/sum.range
cbc_table$imp_brand   <- cbc_table$range_brand/sum.range
cbc_table$imp_screen   <- cbc_table$range_screen/sum.range
cbc_table$imp_battery   <- cbc_table$range_battery/sum.range
cbc_table$imp_storage   <- cbc_table$range_storage/sum.range
cbc_table$imp_quality   <- cbc_table$range_quality/sum.range

importances <- data.frame(imp_price, imp_brand, imp_screen,
                          imp_battery,imp_storage,imp_quality)


#Calculation of willingness to pay
#we multiply by hundred to convert
wtp_brandApple.vs.Oppo <- (mxl_betai$Apple/(-mxl_betai$price))*100 
wtp_brandSamsung.vs.Oppo <- (mxl_betai$Samsung/(-mxl_betai$price))*100
wtp_brandXiaomi.vs.Oppo <- (mxl_betai$Xiaomi/(-mxl_betai$price))*100
wtp_batteryOppo.vs.Oppo <- (mxl_betai$Oppo/(-mxl_betai$price))*100 #this is 0 ofcourse.


wtp_brand <- cbind(wtp_brandApple.vs.Oppo, wtp_brandSamsung.vs.Oppo,
                   wtp_brandXiaomi.vs.Oppo, wtp_batteryOppo.vs.Oppo)

cbc_table$wtp_brandApple.vs.Oppo <- (mxl_betai$Apple/(-mxl_betai$price))*100
cbc_table$wtp_brandSamsung.vs.Oppo <- (mxl_betai$Samsung/(-mxl_betai$price))*100
cbc_table$wtp_brandXiaomi.vs.Oppo <- (mxl_betai$Xiaomi/(-mxl_betai$price))*100
cbc_table$wtp_brandOppo.vs.Oppo <- (mxl_betai$Oppo/(-mxl_betai$price))*100


head(cbc_table)
wtp_middlescreen.vs.smallscreen <- (mxl_betai$middle_screen/(-mxl_betai$price))*100
wtp_bigscreen.vs.smallscreen <- (mxl_betai$big_screen/(-mxl_betai$price))*100
wtp_smallscreen.vs.smallscreen <- (mxl_betai$small_screen/(-mxl_betai$price))*100

wtp_screen <- cbind(wtp_middlescreen.vs.smallscreen, wtp_bigscreen.vs.smallscreen,
                    wtp_smallscreen.vs.smallscreen)

cbc_table$wtp_middlescreen.vs.smallscreen <- (mxl_betai$middle_screen/(-mxl_betai$price))*100
cbc_table$wtp_bigscreen.vs.smallscreen <- (mxl_betai$big_screen/(-mxl_betai$price))*100
cbc_table$wtp_smallscreen.vs.smallscreen <- (mxl_betai$small_screen/(-mxl_betai$price))*100     


wtp_mediumbattery.vs.shortbattery <- (mxl_betai$medium_battery/(-mxl_betai$price))*100
wtp_longbattery.vs.shortbattery <- (mxl_betai$long_battery/(-mxl_betai$price))*100
wtp_shortbattery.vs.shortbattery <- (mxl_betai$short_battery/(-mxl_betai$price))*100



wtp_battery <- cbind(wtp_mediumbattery.vs.shortbattery, wtp_longbattery.vs.shortbattery,
                     wtp_shortbattery.vs.shortbattery)

cbc_table$wtp_mediumbattery.vs.shortbattery <- (mxl_betai$medium_battery/(-mxl_betai$price))*100
cbc_table$wtp_longbattery.vs.shortbattery <- (mxl_betai$long_battery/(-mxl_betai$price))*100
cbc_table$wtp_shortbattery.vs.shortbattery <- (mxl_betai$short_battery/(-mxl_betai$price))*100



wtp_mediumstorage.vs.smallstorage <- (mxl_betai$medium_storage / (-mxl_betai$price)) * 100
wtp_bigstorage.vs.smallstorage <- (mxl_betai$big_storage / (-mxl_betai$price)) * 100
wtp_smallstorage.vs.smallstorage <- (mxl_betai$small_storage / (-mxl_betai$price)) * 100

wtp_storage <- cbind(wtp_mediumstorage.vs.smallstorage, wtp_bigstorage.vs.smallstorage,
                     wtp_smallstorage.vs.smallstorage)

cbc_table$wtp_mediumstorage.vs.smallstorage <- (mxl_betai$medium_storage / (-mxl_betai$price)) * 100
cbc_table$wtp_bigstorage.vs.smallstorage <- (mxl_betai$big_storage / (-mxl_betai$price)) * 100
cbc_table$wtp_smallstorage.vs.smallstorage <- (mxl_betai$small_storage / (-mxl_betai$price)) * 100



wtp_mediumquality.vs.lowquality <- (mxl_betai$medium_quality / (-mxl_betai$price)) * 100
wtp_highquality.vs.lowquality <- (mxl_betai$high_quality / (-mxl_betai$price)) * 100
wtp_lowquality.vs.lowquality <- (mxl_betai$low_quality / (-mxl_betai$price)) * 100

wtp_quality <- cbind(wtp_mediumquality.vs.lowquality, wtp_highquality.vs.lowquality,
                     wtp_lowquality.vs.lowquality)

cbc_table$wtp_mediumquality.vs.lowquality <- (mxl_betai$medium_quality / (-mxl_betai$price)) * 100
cbc_table$wtp_highquality.vs.lowquality <- (mxl_betai$high_quality / (-mxl_betai$price)) * 100
cbc_table$wtp_lowquality.vs.lowquality <- (mxl_betai$low_quality / (-mxl_betai$price)) * 100


########################################################
########################################################
#the analysis of CBC Table

summary(cbc_table)
head(cbc_table)
head(other_data)

nrow(other_data)
nrow(cbc_table)



#Part-worth Brand
# Rename the variables
data_subset <- cbc_table
col_names <- c("Apple", "Samsung", "Xiaomi", "Oppo")
data_subset <- cbc_table[, c("pw_Apple", "pw_Samsung", "pw_Xiaomi", "pw_Oppo")]
colnames(data_subset) <- col_names

# Create the boxplot
boxplot(data_subset, main = "Brand Part-Worth",
        xlab = "Variable", ylab = "Part-Worth",
        col = "lightblue", border = "black")

# Customize the appearance
axis(1, at = 1:length(col_names), labels = col_names)



#Part-worth Screensize
# Rename the variables
data_subset <- cbc_table
col_names <- c("Small Screen", "Medium Screen", "Big Screen")
data_subset <- cbc_table[, c("pw_small_screen", "pw_middle_screen", "pw_big_screen")]
colnames(data_subset) <- col_names

# Create the boxplot
boxplot(data_subset, main = "Screen Part-Worth",
        xlab = "Variable", ylab = "Part-Worth",
        col = "lightblue", border = "black")

# Customize the appearance
axis(1, at = 1:length(col_names), labels = col_names)


#Part-worth Storage
# Rename the variables
data_subset <- cbc_table
col_names <- c("Small Storage", "Medium Storage", "Big Storage")
data_subset <- cbc_table[, c("pw_small_storage", "pw_medium_storage", "pw_big_storage")]
colnames(data_subset) <- col_names

# Create the boxplot
boxplot(data_subset, main = "Storage Part-Worth",
        xlab = "Variable", ylab = "Part-Worth",
        col = "lightblue", border = "black")

# Customize the appearance
axis(1, at = 1:length(col_names), labels = col_names)



#Part-worth Battery
# Rename the variables
data_subset <- cbc_table
col_names <- c("Short Battery", "Medium Battery", "Long Battery")
data_subset <- cbc_table[, c("pw_short_battery", "pw_medium_battery", "pw_long_battery")]
colnames(data_subset) <- col_names

# Create the boxplot
boxplot(data_subset, main = "Battery Part-Worth",
        xlab = "Variable", ylab = "Part-Worth",
        col = "lightblue", border = "black")

# Customize the appearance
axis(1, at = 1:length(col_names), labels = col_names)


#Part-worth Camera
# Rename the variables
data_subset <- cbc_table
col_names <- c("Low Quality", "Medium Quality", "High Quality")
data_subset <- cbc_table[, c("pw_low_quality", "pw_medium_quality", "pw_high_quality")]
colnames(data_subset) <- col_names

# Create the boxplot
boxplot(data_subset, main = "Camera Part-Worth",
        xlab = "Variable", ylab = "Part-Worth",
        col = "lightblue", border = "black")

# Customize the appearance
axis(1, at = 1:length(col_names), labels = col_names)




#Correlation between the importances
dataframe <- importances
colnames(dataframe) <- c("Price", "Brand", "Screen", "Battery", "Storage", "Camera")

corrplot.mixed(cor(dataframe), 
               lower = "circle", upper = "number",tl.cex=0.8,number.cex = .7)

title("Importance Correlations", line = 3.25)



##############################################
##############################################
#merge the cbc table and the survey questions (other data)
other_data_and_cbc_table <- bind_cols(other_data, cbc_table)
nrow(other_data_and_cbc_table)


Turkish <- subset(other_data_and_cbc_table,countryoforigin == "Turkey")
German <- subset(other_data_and_cbc_table,countryoforigin == "Germany")


Turkey_residents <- subset(other_data_and_cbc_table,WhichCountry.1 == "Turkey")
Germany_residents <- subset(other_data_and_cbc_table,WhichCountry.1 == "Germany")

Turkish_Turkey_residents <- subset(Turkey_residents,countryoforigin == "Turkey")
German_Turkey_residents <- subset(Turkey_residents,countryoforigin == "Germany")
Turkish_Germany_residents <- subset(Germany_residents,countryoforigin == "Turkey")
German_Germany_residents <- subset(Germany_residents,countryoforigin == "Germany")

nrow(Turkish_Turkey_residents) #47
nrow(German_Turkey_residents) #0
nrow(Turkish_Germany_residents) #127
nrow(German_Germany_residents) #83

summary(Turkish_Turkey_residents[,74:79]) #importances 
summary(Turkish_Germany_residents[,74:79])
summary(German_Germany_residents[,74:79]) 


summary(Turkey_residents)
summary(Germany_residents)



#comparing the brand and price importance of different phone users
Apple_users <- subset(other_data_and_cbc_table,WhichBrand_1 == 1)
Samsung_users <- subset(other_data_and_cbc_table,WhichBrand_2 == 1)
Xiaomi_users <- subset(other_data_and_cbc_table,WhichBrand_3 == 1)
Huawei_users <- subset(other_data_and_cbc_table,WhichBrand_4 == 1)

summary(Apple_users)
summary(Samsung_users)
summary(Xiaomi_users)
summary(Huawei_users)


head(other_data_and_cbc_table)
#The template for checking the correlation between "other data" and  the importances
correlation <- cor(other_data_and_cbc_table$TimeonPhone, other_data_and_cbc_table[, 74:79])
correlation

p_values <- cor.test(other_data_and_cbc_table$TimeonPhone,
                     other_data_and_cbc_table[, 74])$p.value
significant <- p_values < 0.05
significant


#####################################################
#####################################################
#Segmentation

#segmentation based on importances
smartphone.dist <- dist(apply(importances,2,scale)) 


smartphoneclust <- hclust(smartphone.dist, method ="ward.D2")
plot(smartphoneclust)
summary(smartphoneclust)

smartphoneclust.segment <- cutree(smartphoneclust, k=4)
table(smartphoneclust.segment)

other_data_and_cbc_table$cluster_id <- cutree(smartphoneclust, k=4)
other_data_and_cbc_table$cluster_id

head(other_data_and_cbc_table)
cluster1 <- subset(other_data_and_cbc_table, cluster_id == 1)
cluster2 <- subset(other_data_and_cbc_table, cluster_id == 2)
cluster3 <- subset(other_data_and_cbc_table, cluster_id == 3)
cluster4 <- subset(other_data_and_cbc_table, cluster_id == 4)

summary(cluster1)
summary(cluster2)
summary(cluster3)
summary(cluster4)

summary(themodel)
summary(mxl_betai)

#Segment Analysis

nrow(cluster1)
table(cluster1$PossibleSwitch_1) 
table(cluster1$PossibleSwitch_2) 
table(cluster1$PossibleSwitch_3) 
table(cluster1$PossibleSwitch_4) 
table(cluster1$PossibleSwitch_5) 
table(cluster1$PossibleSwitch_6) 
table(cluster1$PossibleSwitch_7)
table(cluster1$PossibleSwitch_8)


table(cluster1$SourceofInfo_1) #online reviews
table(cluster1$SourceofInfo_2) #family and friends
table(cluster1$SourceofInfo_3) #In-store demos
table(cluster1$SourceofInfo_4) #word-of-mouth
table(cluster1$SourceofInfo_5) #ads
table(cluster1$SourceofInfo_6) #own research
table(cluster1$SourceofInfo_7)

density.plot(cluster1$Age)
plot(density(cluster1$Age))
plot(density(cluster2$Age))
plot(density(cluster3$Age))
plot(density(cluster4$Age))



###############################################
###############################################
#Market Simulation


predict.mxl <- function(X, theta) {
  eu <- exp(as.matrix(X) %*% t(theta))
  p <- t(eu) / colSums(eu)
  #  return(p)
  return(colMeans(p))
}


MarketSimulation <- read.csv("phonesmarketsimulation.csv")
MarketSimulation$Price <- MarketSimulation$Price/100
MarketSimulation_overall <- MarketSimulation[-c(5, 10), ] #sets up overall market simulation

low_end_ms <- MarketSimulation[1:5, ]
mid_end_ms <- MarketSimulation[6:10, ]
high_end_ms <- MarketSimulation[11:15, ]

shares_low_end<-predict.mxl(low_end_ms, mxl_betai)
shares_low_end
shares_mid_end<-predict.mxl(mid_end_ms, mxl_betai)
shares_mid_end
shares_high_end<-predict.mxl(high_end_ms, mxl_betai)
shares_high_end
shares_overall<-predict.mxl(MarketSimulation_overall, mxl_betai)
shares_overall







####################################
# END
####################################