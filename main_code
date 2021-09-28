# CIS 4730 - Summer 2021 (Professor Zhang) - Course Project (Group #3)
# By: David Catalan Perez, Vandyck Buabeng, Kristen Hanold, & Reuben Akipogu
# Dataset used ("phone_user_review_file_1"): https://www.kaggle.com/masaladata/14-million-cell-phone-reviews


# Installing Packages into the R Environment
install.packages("rvest")
install.packages("stringr")
install.packages("tidyverse")
install.packages("readr")
install.packages("dplyr")

# Setting up the R Environment
library(rvest)
library(stringr)
library(tidyverse)
library(readr)
library(dplyr)

# Acquiring the Data - Reading Phone Reviews File
User_Reviews = read_csv("phone_user_review_file_1.csv")

# Filtering the Data - Finding the Top 5 Regions in Device Purchases
User_Reviews_Top5_Regions = User_Reviews %>%
  group_by(country) %>%
  summarise(Number_of_Region_Purchases = n()) %>%
  arrange(desc(Number_of_Region_Purchases))

User_Reviews_Top5_Regions <- top_n(User_Reviews_Top5_Regions, 5)

# Extracting the Data - Getting United States Reviews
User_Reviews_US = User_Reviews %>% filter(country == "us")

# Extracting the Data - Getting India Reviews
User_Reviews_IN = User_Reviews %>% filter(country == "in")

# Extracting the Data - Getting Italy Reviews
User_Reviews_IT = User_Reviews %>% filter(country == "it")

# Extracting the Data - Getting Germany Reviews
User_Reviews_DE = User_Reviews %>% filter(country == "de")

# Extracting the Data - Getting France Reviews
User_Reviews_FR = User_Reviews %>% filter(country == "fr")

# US Device and Average Rating
Device_Ratings_US = User_Reviews_US %>%
  group_by(product) %>%
  summarise(Number_of_Products = n(), 
            Average_Product_Rating = mean(score)) %>%
  mutate(Product_Percentage = round((Number_of_Products / 
                                       sum(Number_of_Products) * 100), 2)) %>%
  arrange(desc(Number_of_Products))

# India Device and Average Rating
Device_Ratings_IN = User_Reviews_IN %>%
  group_by(product) %>%
  summarise(Number_of_Products = n(), 
            Average_Product_Rating = mean(score)) %>%
  mutate(Product_Percentage = round((Number_of_Products / 
                                       sum(Number_of_Products)* 100), 2)) %>%
  arrange(desc(Number_of_Products))

# Italy Device and Average Rating
Device_Ratings_IT = User_Reviews_IT %>%
  group_by(product) %>%
  summarise(Number_of_Products = n(), 
            Average_Product_Rating = mean(score)) %>%
  mutate(Product_Percentage = round((Number_of_Products / 
                                       sum(Number_of_Products) * 100), 2)) %>%
  arrange(desc(Number_of_Products))

# Germany Device and Average Rating
Device_Ratings_DE = User_Reviews_DE %>%
  group_by(product) %>%
  summarise(Number_of_Products = n(), 
            Average_Product_Rating = mean(score)) %>%
  mutate(Product_Percentage = round((Number_of_Products / 
                                       sum(Number_of_Products) * 100), 2)) %>%
  arrange(desc(Number_of_Products))

# France Device and Average Rating
Device_Ratings_FR = User_Reviews_FR %>%
  group_by(product) %>%
  summarise(Number_of_Products = n(), 
            Average_Product_Rating = mean(score)) %>%
  mutate(Product_Percentage = round((Number_of_Products / 
                                       sum(Number_of_Products) * 100), 2)) %>%
  arrange(desc(Number_of_Products))

# Top Source Methods of Purchasing a Device
Source_Purchases = User_Reviews %>%
  group_by(source) %>%
  summarise(Number_of_Source_Purchases = n()) %>%
  mutate(Source_Percentage = round((Number_of_Source_Purchases / 
                                      sum(Number_of_Source_Purchases) * 
                                      100), 2) * 100) %>%
  arrange(desc(Number_of_Source_Purchases))

Source_Purchases <- top_n(Source_Purchases, 10)

# Top Regions - Purchase Domain URLs
Region_Purchases = User_Reviews %>%
  group_by(domain) %>%
  summarise(Domain_Purchases = n()) %>%
  mutate(Domain_Percentage = round((Domain_Purchases / 
                                      sum(Domain_Purchases) * 100), 2)) %>%
  arrange(desc(Domain_Purchases))

# Filtering the Data - Finding the Top 5 Regions in Device Purchases
Top5 = c("us", "fr", "in", "it", "de")

User_Reviews_Top_Products = User_Reviews %>%
  select(country, product, score) %>%
  group_by(product, country, score) %>%
  summarise(Total_Products = n()) %>%
  arrange(desc(Total_Products))

User_Reviews_Top_Products = User_Reviews_Top_Products %>%
  filter(country %in% Top5)

User_Reviews_Top_Products

# Counts the Number of Devices in the US 
us <- User_Reviews_US %>%
  select(country, product) %>%
  summarise(product = n())

# Counts the Number of Devices in the India
In <- User_Reviews_IN %>%
  select(country, product) %>%
  summarise(product = n())

#Counts the Number of Devices in the Italy
it <- User_Reviews_IT %>%
  select(country, product) %>%
  summarise(product = n())

# Counts the Number of Devices in the Germany
de <- User_Reviews_DE %>%
  select(country, product) %>%
  summarise(product = n())

# Counts the Number of Devices in the France
fr <- User_Reviews_FR %>%
  select(country, product) %>%
  summarise(product = n())

# Shows Devices with a score from most to least from our top 5 regions. 
High_to_Low = User_Reviews_Top_Products %>%
  arrange(desc(score))

# Shows Devices in the US with a score of 10.
Ten_Rated_Devices_US = User_Reviews_Top_Products %>% 
  filter(country == "us", score == 10) %>%
  select(country, product, score)

# Shows Devices in the US with a score of 5. 
Five_Rated_Devices_US = User_Reviews_Top_Products %>% 
  filter(country == "us", score == 5) %>%
  select(country, product, score)

# Show individuals in the Top 5 Countries that uses a Samsung Galaxy.
Samsung = "Samsung Galaxy"

Samsung_Galaxy = User_Reviews_Top_Products %>%
  group_by(country) %>%
  filter(str_detect(product, Samsung)) %>%
  summarise(Device_Purchases = sum(Total_Products)) %>%
  arrange(desc(Device_Purchases))

# Show individuals in the Top 5 Countries that uses a Apple iPhone.
Apple = "Apple iPhone"

Apple_iPhone = User_Reviews_Top_Products %>%
  group_by(country) %>%
  filter(str_detect(product, Apple)) %>%
  summarise(Device_Purchases = sum(Total_Products)) %>%
  arrange(desc(Device_Purchases))

# Show individuals in the Top 5 Countries that uses a Huawei.
Huawei = "Huawei"

Huawei_Device = User_Reviews_Top_Products %>%
  group_by(country) %>%
  filter(str_detect(product, Huawei)) %>%
  summarise(Device_Purchases = sum(Total_Products)) %>%
  arrange(desc(Device_Purchases))

# Show individuals in the Top 5 Countries that uses a Honor.
Honor = "Honor"

Honor_Device = User_Reviews_Top_Products %>%
  group_by(country) %>%
  filter(str_detect(product, Honor)) %>%
  summarise(Device_Purchases = sum(Total_Products)) %>%
  arrange(desc(Device_Purchases))

# Show individuals in the Top 5 Countries that uses a OnePlus.
OnePlus = "OnePlus"

OnePlus_Device = User_Reviews_Top_Products %>%
  group_by(country) %>%
  filter(str_detect(product, OnePlus)) %>%
  summarise(Device_Purchases = sum(Total_Products)) %>%
  arrange(desc(Device_Purchases))

# Finding Keyword "love" in Reviews and Displaying the top 5 devices.
reviews_keywords_love <- User_Reviews %>%
  select(extract, product) %>%
  filter(str_detect(extract, "[Ll]ove")) %>%
  count(product) %>%
  arrange(desc(n))
head(reviews_keywords_love, 5)

# Using Regex to confirm str_detect received the correct number.
Phone_Reviews = User_Reviews$extract
my_regex_Love = "[Ll]ove"
Love = (stringr::str_extract_all(Phone_Reviews, my_regex_Love))
Love[lengths(Love) == 0] <- NA_character_
Love = Love[!is.na(Love)]
Love_Match = length(Love)

# Finding Keyword "best" in Reviews and Displaying the top 5 devices
reviews_keywords_Best <- User_Reviews %>%
  select(extract, product) %>%
  filter(str_detect(extract, "[Bb]est")) %>%
  count(product) %>%
  arrange(desc(n))
head(reviews_keywords_Best, 5)

# Using Regex to confirm str_detect received the correct number.
Phone_Reviews = User_Reviews$extract
my_regex_Best = "[Bb]est"
Best = (stringr::str_extract_all(Phone_Reviews, my_regex_Best))
Best[lengths(Best) == 0] <- NA_character_
Best = Best[!is.na(Best)]
Best_Match = length(Best)
Best_Match

# Finding Keyword "battery" in Reviews and Displaying the top 5 devices
reviews_keywords_Battery <- User_Reviews %>%
  select(extract, product) %>%
  filter(str_detect(extract, "[Bb]attery")) %>%
  count(product) %>%
  arrange(desc(n))
head(reviews_keywords_Battery, 5)

# Using Regex to confirm str_detect received the correct number.
Phone_Reviews = User_Reviews$extract
my_regex_Battery = "[Bb]attery"
Battery = (stringr::str_extract_all(Phone_Reviews, my_regex_Battery))
Battery[lengths(Battery) == 0] <- NA_character_
Battery = Battery[!is.na(Battery)]
Battery_Match = length(Battery)

# Finding Keyword "issues" in Reviews and Displaying the top 5 devices.
reviews_keywords_Issues <- User_Reviews %>%
  select(extract, product) %>%
  filter(str_detect(extract, "[Ii]ssues*")) %>%
  count(product) %>%
  arrange(desc(n))
head(reviews_keywords_Issues, 5)

# Using Regex to confirm str_detect received the correct number.
Phone_Reviews = User_Reviews$extract
my_regex_Issues = "[Ii]ssues*"
Issues = (stringr::str_extract_all(Phone_Reviews, my_regex_Issues))
Issues[lengths(Issues) == 0] <- NA_character_
Issues = Issues[!is.na(Issues)]
Issues_Match = length(Issues)

# Finding Keyword "price" in Reviews and Displaying the top 5 devices.
reviews_keywords_Price <- User_Reviews %>%
  select(extract, product) %>%
  filter(str_detect(extract, "[Pp]rice")) %>%
  count(product) %>%
  arrange(desc(n))
head(reviews_keywords_Price, 5)

# Using Regex to confirm str_detect received the correct number.
Phone_Reviews = User_Reviews$extract
my_regex_Price = "[Pp]rice"
Price = (stringr::str_extract_all(Phone_Reviews, my_regex_Price))
Price[lengths(Price) == 0] <- NA_character_
Price = Price[!is.na(Price)]
Price_Match = length(Price)

# Finding the Most Popular Locations Where Devices were Bought.
device_source_us = User_Reviews %>% 
  group_by(source) %>%
  summarise(num_of_device = n()) %>%
  mutate(source_percentage = round((num_of_device / sum(num_of_device) * 
                                      100), 2)) %>%
  arrange(desc(num_of_device))
head(device_source_us, 5)

# Generating Figures

# Average Product Ratings
hist(Device_Ratings_US$Average_Product_Rating)
hist(Device_Ratings_IT$Average_Product_Rating)
hist(Device_Ratings_IN$Average_Product_Rating)
hist(Device_Ratings_DE$Average_Product_Rating)
hist(Device_Ratings_FR$Average_Product_Rating)

# Popular Source Locations
pie(device_source_us$source_percentage)
device_source_us

# Domain Purchases
barplot(Region_Purchases$Domain_Purchases)
Region_Purchases

# Number of Reviews from the Top 5 Countries
pie(User_Reviews_Top5_Regions$Number_of_Region_Purchases)
User_Reviews_Top5_Regions
