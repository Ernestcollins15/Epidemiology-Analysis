#Research Question
#Is there a Disparity in the health conditions in children under 5 years i in different counties of the country?


# Loading the necessary library for data manipulation
install.packages("tidyverse")
library(tidyverse)
library(dplyr)



#Importing the data
cema_data <- read.csv("C:/Users/Ernest/Downloads/cema_internship_task.csv")

# Checking summary statistics
summary(cema_data)

# Checking structure of data 
structure <- str(cema_data)   
structure

# Checking column names of data
columns <- colnames(cema_data)
columns

#Getting unique counties
counties <- unique(cema_data$county)
counties

##Checking for missing values in each column
missing_data <- colSums(is.na(cema_data))
missing_data

#Dealing with missing data
#Dropping the missing data in "stunted.6.23.months",stunted.24.59.months,stunted.0..6.months Since they are cumulatively less than 4% of the entire data
cema_data <- cema_data[!(is.na(cema_data$stunted.6.23.months)),] #Dropping missing data in stunted.6.23.months column
cema_data <- cema_data[!(is.na(cema_data$stunted.24.59.months)),]#Dropping missing data in stunted.24.59.months column
cema_data <- cema_data[!(is.na(cema_data$stunted.0..6.months)),]#Dropping missing data in stunted.0..6.months column

View(cema_data)

#Checking if the missing data was dropped as intended
colSums(is.na(cema_data))

# Aggregating & getting the rounded mean Acute Malnutrition values (nearest whole number) by county
agg <- aggregate(Acute.Malnutrition ~ county, data = cema_data, FUN = function(x) round(mean(x), digits = 0))
agg

# Sample aggregated Acute Malnutrition mean for Each county 
aggregated_mean_data <- data.frame(
  county = c("Baringo County", "Bomet County", "Bungoma County", "Busia County", "Embu County", "Garissa County",
             "Homa Bay County", "Isiolo County", "Kajiado County", "Kakamega County", "Kiambu County",
             "Kilifi County", "Kirinyaga County", "Kisii County", "Kisumu County", "Kitui County", "Kwale County",
             "Laikipia County", "Machakos County", "Makueni County", "Mandera County", "Marsabit County",
             "Meru County", "Migori County", "Mombasa County", "Muranga County", "Nairobi County", "Nakuru County",
             "Nandi County", "Narok County", "Nyamira County", "Nyandarua County", "Nyeri County", "Samburu County",
             "Siaya County", "Taita Taveta County", "Tana River County", "Tharaka Nithi County", "Trans Nzoia County",
             "Turkana County", "Uasin Gishu County", "Vihiga County", "Wajir County", "West Pokot County"),
  Acute.Malnutrition <- c(9, 1, 10, 6, 44, 268, 26, 66, 78, 24, 105, 79, 3, 3, 137, 13, 159, 32, 113, 51,
                          195, 114, 40, 26, 259, 15, 327, 157, 17, 26, 4, 39, 21, 82, 2, 8, 265, 22, 174,
                          328, 1, 28, 1086, 33)
)

# Aggregating & getting the rounded mean diarrhea cases values (nearest whole number) by county
agg_diarr <- aggregate(diarrhoea.cases ~ county, data = cema_data, FUN = function(x) round(mean(x), digits = 0))
agg_diarr
#Aggregated mean Diarrhea Cases for each county
aggregated_mean_diarr_cases <- data.frame(
  county = c("Baringo County", "Bomet County", "Bungoma County", "Busia County", "Elgeyo Marakwet County",
             "Embu County", "Garissa County", "Homa Bay County", "Isiolo County", "Kajiado County",
             "Kakamega County", "Kericho County", "Kiambu County", "Kilifi County", "Kirinyaga County",
             "Kisii County", "Kisumu County", "Kitui County", "Kwale County", "Laikipia County", "Lamu County",
             "Machakos County", "Makueni County", "Mandera County", "Marsabit County", "Meru County",
             "Migori County", "Mombasa County", "Muranga County", "Nairobi County", "Nakuru County",
             "Nandi County", "Narok County", "Nyamira County", "Nyandarua County", "Nyeri County",
             "Samburu County", "Siaya County", "Taita Taveta County", "Tana River County", "Tharaka Nithi County",
             "Trans Nzoia County", "Turkana County", "Uasin Gishu County", "Vihiga County", "Wajir County",
             "West Pokot County"),
  diarrhoea.cases = c(2451, 2343, 3081, 1690, 2012, 998, 2193, 1817, 1117, 3925, 3732, 2883, 5182, 7640,
                      2011, 2084, 3077, 2197, 3308, 1445, 937, 3394, 2296, 6301, 1755, 2056, 2498, 3886,
                      1747, 10139, 6287, 1610, 2724, 940, 978, 1127, 2305, 2090, 845, 1823, 967, 1471,
                      7359, 5094, 777, 3117, 2607)
)


#Aggregating & getting the rounded mean stunted.6.23.months cases by county
agg_stunted_6to23 <- aggregate(stunted.6.23.months ~ county, data = cema_data, FUN = function(x) round(mean(x), digits = 0))
agg_stunted_6to23



#Aggregated mean stunted 6 to 23 months cases by county
agg_stunted_6to23_months <- data.frame(
  county = c("Baringo County", "Bomet County", "Bungoma County", "Busia County", "Elgeyo Marakwet County",
             "Embu County", "Garissa County", "Homa Bay County", "Isiolo County", "Kajiado County",
             "Kakamega County", "Kericho County", "Kiambu County", "Kilifi County", "Kirinyaga County",
             "Kisii County", "Kisumu County", "Kitui County", "Kwale County", "Laikipia County", "Lamu County",
             "Machakos County", "Makueni County", "Mandera County", "Marsabit County", "Meru County",
             "Migori County", "Mombasa County", "Muranga County", "Nairobi County", "Nakuru County",
             "Nandi County", "Narok County", "Nyamira County", "Nyandarua County", "Nyeri County",
             "Samburu County", "Siaya County", "Taita Taveta County", "Tana River County", "Tharaka Nithi County",
             "Trans Nzoia County", "Turkana County", "Uasin Gishu County", "Vihiga County", "Wajir County",
             "West Pokot County"),
  stunted.6.23.months = c(262, 30, 121, 339, 80, 264, 41, 144, 59, 275, 304, 44, 561, 1255, 159, 59,
                          155, 647, 539, 229, 22, 46, 459, 123, 318, 208, 113, 453, 507, 1888, 526,
                          123, 81, 61, 135, 245, 85, 40, 252, 142, 198, 168, 694, 274, 184, 63, 103)
)

#Aggregating & getting the rounded mean stunted 0 to 6 months cases by county
agg_stunted_0to6 <- aggregate(stunted.0..6.months ~ county, data = cema_data, FUN = function(x) round(mean(x), digits = 0))
agg_stunted_0to6

agg_stunted_0to6_months <- data.frame(
  county = c("Baringo County", "Bomet County", "Bungoma County", "Busia County", "Elgeyo Marakwet County",
             "Embu County", "Garissa County", "Homa Bay County", "Isiolo County", "Kajiado County",
             "Kakamega County", "Kericho County", "Kiambu County", "Kilifi County", "Kirinyaga County",
             "Kisii County", "Kisumu County", "Kitui County", "Kwale County", "Laikipia County", "Lamu County",
             "Machakos County", "Makueni County", "Mandera County", "Marsabit County", "Meru County",
             "Migori County", "Mombasa County", "Muranga County", "Nairobi County", "Nakuru County",
             "Nandi County", "Narok County", "Nyamira County", "Nyandarua County", "Nyeri County",
             "Samburu County", "Siaya County", "Taita Taveta County", "Tana River County", "Tharaka Nithi County",
             "Trans Nzoia County", "Turkana County", "Uasin Gishu County", "Vihiga County", "Wajir County",
             "West Pokot County"),
  stunted.0..6.months = c(122, 6, 95, 173, 68, 97, 26, 120, 27, 167, 220, 34, 374, 329, 63, 87, 96, 175, 135,
                          231, 11, 43, 106, 86, 37, 124, 64, 157, 265, 1229, 281, 75, 86, 47, 96, 179, 35, 26,
                          89, 41, 88, 135, 239, 192, 60, 26, 38)
)


#Aggregating & getting the mean stunted 24 to 59.months cases by county
agg_stunted_24to59 <- aggregate(stunted.24.59.months ~ county, data = cema_data, FUN = function(x) round(mean(x), digits = 0))
agg_stunted_24to59

agg_stunted_24to59_months <- data.frame(
  county = c("Baringo County", "Bomet County", "Bungoma County", "Busia County", "Elgeyo Marakwet County",
             "Embu County", "Garissa County", "Homa Bay County", "Isiolo County", "Kajiado County",
             "Kakamega County", "Kericho County", "Kiambu County", "Kilifi County", "Kirinyaga County",
             "Kisii County", "Kisumu County", "Kitui County", "Kwale County", "Laikipia County", "Lamu County",
             "Machakos County", "Makueni County", "Mandera County", "Marsabit County", "Meru County",
             "Migori County", "Mombasa County", "Muranga County", "Nairobi County", "Nakuru County",
             "Nandi County", "Narok County", "Nyamira County", "Nyandarua County", "Nyeri County",
             "Samburu County", "Siaya County", "Taita Taveta County", "Tana River County", "Tharaka Nithi County",
             "Trans Nzoia County", "Turkana County", "Uasin Gishu County", "Vihiga County", "Wajir County",
             "West Pokot County"),
  stunted.24.59.months = c(238, 7, 35, 67, 9, 49, 232, 26, 64, 100, 89, 21, 75, 273, 26, 15, 85, 255, 333,
                           69, 6, 107, 186, 143, 323, 54, 48, 78, 115, 427, 93, 30, 55, 9, 16, 32, 64,
                           30, 67, 88, 55, 26, 656, 56, 53, 223, 65)
)


#Aggregating & getting the mean Underweight cases 0 to 6 months cases by county
agg_underweight_0to6 <- aggregate(Underweight.0..6.months ~ county, data = cema_data, FUN = function(x) round(mean(x), digits = 0))
agg_underweight_0to6 

agg_underweight_0to6_months <- data.frame(
  county = c("Baringo County", "Bomet County", "Bungoma County", "Busia County", "Elgeyo Marakwet County",
             "Embu County", "Garissa County", "Homa Bay County", "Isiolo County", "Kajiado County",
             "Kakamega County", "Kericho County", "Kiambu County", "Kilifi County", "Kirinyaga County",
             "Kisii County", "Kisumu County", "Kitui County", "Kwale County", "Laikipia County", "Lamu County",
             "Machakos County", "Makueni County", "Mandera County", "Marsabit County", "Meru County",
             "Migori County", "Mombasa County", "Muranga County", "Nairobi County", "Nakuru County",
             "Nandi County", "Narok County", "Nyamira County", "Nyandarua County", "Nyeri County",
             "Samburu County", "Siaya County", "Taita Taveta County", "Tana River County", "Tharaka Nithi County",
             "Trans Nzoia County", "Turkana County", "Uasin Gishu County", "Vihiga County", "Wajir County",
             "West Pokot County"),
  Underweight.0..6.months = c(111, 78, 296, 230, 73, 141, 102, 142, 29, 179, 281, 137, 670, 388, 175,
                              129, 205, 236, 255, 360, 27, 355, 155, 201, 71, 291, 103, 295, 376, 1284,
                              730, 121, 104, 68, 159, 316, 64, 101, 176, 51, 148, 216, 327, 277, 92, 129, 64)
)

 
#Aggregating & getting the mean Underweight cases 6 to 23 months cases by county
agg_underweight_6to23 <- aggregate(Underweight.6.23.months ~ county, data = cema_data, FUN = function(x) round(mean(x), digits = 0))
agg_underweight_6to23 

agg_underweight_6to23_months <- data.frame(
  county = c("Baringo County", "Bomet County", "Bungoma County", "Busia County", "Elgeyo Marakwet County",
             "Embu County", "Garissa County", "Homa Bay County", "Isiolo County", "Kajiado County",
             "Kakamega County", "Kericho County", "Kiambu County", "Kilifi County", "Kirinyaga County",
             "Kisii County", "Kisumu County", "Kitui County", "Kwale County", "Laikipia County", "Lamu County",
             "Machakos County", "Makueni County", "Mandera County", "Marsabit County", "Meru County",
             "Migori County", "Mombasa County", "Muranga County", "Nairobi County", "Nakuru County",
             "Nandi County", "Narok County", "Nyamira County", "Nyandarua County", "Nyeri County",
             "Samburu County", "Siaya County", "Taita Taveta County", "Tana River County", "Tharaka Nithi County",
             "Trans Nzoia County", "Turkana County", "Uasin Gishu County", "Vihiga County", "Wajir County",
             "West Pokot County"),
  Underweight.6.23.months = c(534, 111, 395, 561, 151, 533, 959, 239, 171, 601, 647, 344, 1272, 1847, 496,
                              155, 493, 1002, 1023, 500, 64, 881, 858, 467, 854, 479, 211, 1119, 851, 3404,
                              1288, 304, 235, 98, 272, 512, 474, 173, 553, 541, 406, 377, 2283, 642, 277,
                              697, 317)
)


agg_underweight_24to59 <- aggregate(Underweight.24.59.Months ~ county, data = cema_data, FUN = function(x) round(mean(x), digits = 0))
agg_underweight_24to59

agg_underweight_24to59_months <- data.frame(
  county = c("Baringo County", "Bomet County", "Bungoma County", "Busia County", "Elgeyo Marakwet County",
             "Embu County", "Garissa County", "Homa Bay County", "Isiolo County", "Kajiado County",
             "Kakamega County", "Kericho County", "Kiambu County", "Kilifi County", "Kirinyaga County",
             "Kisii County", "Kisumu County", "Kitui County", "Kwale County", "Laikipia County", "Lamu County",
             "Machakos County", "Makueni County", "Mandera County", "Marsabit County", "Meru County",
             "Migori County", "Mombasa County", "Muranga County", "Nairobi County", "Nakuru County",
             "Nandi County", "Narok County", "Nyamira County", "Nyandarua County", "Nyeri County",
             "Samburu County", "Siaya County", "Taita Taveta County", "Tana River County", "Tharaka Nithi County",
             "Trans Nzoia County", "Turkana County", "Uasin Gishu County", "Vihiga County", "Wajir County",
             "West Pokot County"),
  Underweight.24.59.Months = c(477, 23, 141, 141, 19, 98, 1048, 61, 212, 386, 192, 74, 235, 503, 66, 41,
                               143, 527, 509, 117, 19, 199, 402, 612, 1261, 153, 84, 216, 170, 543, 224,
                               68, 63, 14, 51, 60, 478, 79, 171, 381, 75, 64, 2621, 82, 68, 1019, 196)
)


#Correlation


#Creating a subset of the data containing relevant columns
subset_data <- cema_data %>%
  select(county,period,Total.Dewormed,Acute.Malnutrition)

subset_data


# Filtering out groups with zero variance
filtered_data <- subset_data %>%
  group_by(county) %>%
  filter(sd(Total.Dewormed) > 0 & sd(Acute.Malnutrition) > 0) 

# Calculating the correlation coefficient for each county
correlation_by_county <- filtered_data %>%
  group_by(county) %>%
  summarise(correlation = cor(Total.Dewormed, Acute.Malnutrition, use = "pairwise.complete.obs"))

correlation_by_county


# Creating a scatter plot to visualize the correlation
ggplot(subset_data, aes(x = Total.Dewormed, y = Acute.Malnutrition)) +
  geom_point() +
  facet_wrap(~ county, scales = "free") +
  labs(title = "Correlation between Total Dewormed and Acute Malnutrition by County",
       x = "Total Dewormed",
       y = "Acute Malnutrition")

# Create the scatter plot
ggplot(subset_data, aes(x = Total.Dewormed, y = Acute.Malnutrition)) +
  geom_point(size = 3, alpha = 0.7) +   # Adjust point size and transparency (alpha)
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  facet_wrap(~ county, scales = "free", ncol = 3) +   # Adjust facet labels
  labs(title = "Correlation between Total Dewormed and Acute Malnutrition by County",
       x = "Total Dewormed",
       y = "Acute Malnutrition") +
  theme_minimal() +   # Customize the theme to minimal
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis label rotation




  
#Visualizations 
#Regional Disparities in Acute Malnutrition Cases
#Box plot to compare Acute Malnutrition Cases across different counties
ggplot(cema_data, aes(x = county, y = Acute.Malnutrition)) +
geom_boxplot() +
labs(x = "County", y = "Acute Malnutrition",
       title = "Regional Disparities in Child Acute Malnutrition Cases")



#Box plot to compare Diarrhea Cases Across Different counties
ggplot(cema_data, aes(x = county, y = diarrhoea.cases)) +
  geom_boxplot() +
  labs(x = "County", y = "diarrhoea cases",
       title = "Regional Disparities in Diarrhoea Cases")


#Box plot to compare stunted growth cases(6to23_months) Across Different counties
ggplot(cema_data, aes(x = county, y = stunted.6.23.months)) +
  geom_boxplot() +
  labs(x = "County", y = "stunted_6to23_months",
       title = "Regional Disparities in Stunted Growth Cases 6_23_months")

#Box plot to compare stunted growth cases(0to6_months) Across Different counties
ggplot(cema_data, aes(x = county, y = stunted.0..6.months)) +
  geom_boxplot() +
  labs(x = "County", y = "stunted _0to6_months",
       title = "Regional Disparities in Stunted Growth Cases 0_6_months")

#Box plot to compare stunted growth cases(24to59_months) Across Different counties
ggplot(cema_data, aes(x = county, y = stunted.24.59.months)) +
  geom_boxplot() +
  labs(x = "County", y = "stunted_24to_59_months",
       title = "Regional Disparities in Stunted Growth Cases 24_59_months")



#Box plot to compare Total Dewormed Across Different counties
ggplot(cema_data, aes(x = county, y = Total.Dewormed)) +
  geom_boxplot() +
  labs(x = "County", y = "Total Dewormed",
       title = "Regional Disparities Dewormed children")


# Line plot to analyze seasonal patterns in Diarrhea cases

ggplot(cema_data, aes(x = period, y = diarrhoea.cases, group = county, color = as.factor(county))) +
  geom_line() +
  labs(x = "Period", y = "Diarrhea Cases",
       title = "Seasonal Patterns of Diarrhea Cases by County",
       color = "County") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels by 45 degrees

# Bar chart to show the average Acute malnutrition cases by County
ggplot(aggregated_mean_data, aes(x = county, y = Acute.Malnutrition)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "County", y = "Acute Malnutrition Cases",
       title = "Average Acute Malnutrition Cases by County") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar Chart Showing the Average Diarrhea cases by county
ggplot(aggregated_mean_diarr_cases, aes(x = county, y = diarrhoea.cases)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "County", y = "Diarrhea Cases",
       title = "Average Diarrhea Cases by County") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar Chart Showing the Average stunted growth(6to23 months)cases by county
ggplot(agg_stunted_6to23_months, aes(x = county, y = stunted.6.23.months)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "County", y = "Stunted growth Cases-6to23 months",
       title = "Average Stunted growth 6 to 23 months cases by County") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar Chart Showing the Average stunted growth(0to6 months)cases by county
ggplot(agg_stunted_0to6_months, aes(x = county, y = stunted.0..6.months)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "County", y = "Stunted growth Cases 0 to 6 months",
       title = "Average Stunted growth 0 to 6 months cases by County") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar Chart Showing the Average stunted growth(24to59 months)cases by county
ggplot(agg_stunted_24to59_months, aes(x = county, y = stunted.24.59.months)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "County", y = "Stunted growth Cases 24 to 59 months",
       title = "Average Stunted growth 24 to 59 months cases by County") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Chart Showing the Average Underweight cases 0 to 6 months by county
ggplot(agg_underweight_0to6_months, aes(x = county, y = Underweight.0..6.months)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "County", y = "Underweight Cases 0 to 6 months",
       title = "Average Underweight 0 to 6 months cases by County") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar Chart Showing the Average Underweight cases 6 to 23 months by county
ggplot(agg_underweight_6to23_months, aes(x = county, y = Underweight.6.23.months)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "County", y = "Underweight Cases 6 to 23 months",
       title = "Average Underweight 6 to 23 months cases by County") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar Chart Showing the Average Underweight cases 24 to 60 months by county
ggplot(agg_underweight_24to59_months, aes(x = county, y = Underweight.24.59.Months)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "County", y = "Underweight Cases 24 to 59 months",
       title = "Average Underweight 24 to 59 months cases by County") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  

























