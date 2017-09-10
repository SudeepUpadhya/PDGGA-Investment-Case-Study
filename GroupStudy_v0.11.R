#
# Spark Funds Case Study Group Submission by:
# 1. Sudeep Upadhya
# 2. Rishab Nigam
# 3. Vikash Prasad
# 4. Amit Mankikar
#
#

#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("countrycode")
library(tidyr)
library(dplyr)
library(countrycode)

setwd("E:/UpGrade Training/Group Case Study")

#Table 1.1
#
# Load the 2 data frames
#
companies<-read.delim("companies.txt", sep="\t",stringsAsFactors = F)
rounds2<-read.csv("rounds2.csv",stringsAsFactors = F)

# Convert all permalinks in companies df to upper and store in unique1
companies[,1] = toupper(companies[,1])
unique1<-as.data.frame(unique((companies[,1])))
# 1. How many unique companies are present in companies?
message("Number of unique companies in companies.txt = ", nrow(unique1))

#
# Convert all permalinks in rounds2 df to upper and store in unique2
rounds2[,1] = toupper(rounds2[,1])
unique2<-as.data.frame(unique(rounds2[,1]))
# 2. How many unique companies are present in rounds2?
message("Number of unique companies in rounds2.csv= ", nrow(unique2))

# 3. In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
#Ans. permalink
#

# 4. Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
#No, All companies in rounds2.csv are present in companies.txt

# Add colnames to both unique permalinks
colnames(unique1)[1] <- "permalink"
colnames(unique2)[1] <- "permalink"
#
# Left outer join will give all companies in left table + matching companies in right table
#
tmp <- merge(x = unique2, y = unique1, by="permalink", all.x = TRUE)
diff <- nrow(tmp) - nrow(unique2)
if( diff == 0L ){
  message("No, All companies in rounds2.csv are present in companies.txt")
  
} else
{
  message("Yes, Number of companies in rounds2.csv not present in companies.txt = ", diff)
}

#Merge the two data frames
#
names(rounds2)[1]<-"permalink" # (This is rename column header in round2 to match with companies header)
master_frame<-merge(companies,rounds2, all= TRUE)

# 5. How many observations are present in master_frame ?
message("Number of observations in master_frame =", nrow(master_frame))

#
#alternate merge:( this method discussed in the discussion form)
master_frame1<-merge(companies,rounds2,by.x="permalink", by.y="permalink")
#


#Table 2.1
# 1. How many NA values are present in the column raised_amount_usd?
na_count <- sum(is.na(master_frame$raised_amount_usd))
message("Number of NA values in column raise_amount_usd = ", na_count)

#
# 2. What do you replace NA values of raised_amount_usd with? Enter a numeric value.
# 
# Ans. We can impute the NA values by the mean using the following command
# master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] = mean(master_frame$raised_amount_usd, na.rm = TRUE)


#Table 3.1

#To reduce unwanted rows/column from dataframes
#(to remove zero values for raised_amount_usd)
master_frame$raised_amount_usd[which(is.na(master_frame$raised_amount_usd))]<-0
master_frame$funding_round_type[which(is.na(master_frame$funding_round_type))]<-0
master_frame<-master_frame[-which(master_frame$raised_amount_usd == 0), ]

#
#(create columns for each funding_round_type)
spread_master_frame<-spread(master_frame,key= funding_round_type,value = raised_amount_usd )

# 1. Average funding amount of venture type
message("Average funding amount of venture type = ", round(mean(spread_master_frame$venture,na.rm=TRUE)))
# 11755737
#
# 2. Average funding amount of angel type
message("Average funding amount of angel type = ", round(mean(spread_master_frame$angel,na.rm=TRUE)))
# 964849
#
# 3. Average funding amount of seed type
message("Average funding amount of seed type = ", round(mean(spread_master_frame$seed,na.rm = TRUE)))
# 722727
#
# 4. Average funding amount of private equity type
message("Average funding amount of private equity type = ", round(mean(spread_master_frame$private_equity,na.rm=TRUE)))
# 63788087

# 5. Which investment type is the most suitable for it?
# Ans. Venture investment is suitalbe as its in the 11.7 Million which is between 5 to 15 Million

#Table 4.1
# Read the English speaking countries list text file
engcountry<-read.delim("EnglishCountry.txt", sep="\t",stringsAsFactors = F)
engcountry["country_code"] = countrycode(engcountry$Country, "country.name", "iso3c")

# In Data exploration, we saw country code in data is incorrect, so replace with correct country code
master_frame$country_code[which(master_frame$country_code == "BAH")] = "BHR"
master_frame$country_code[which(master_frame$country_code == "TAN")] = "TZA"
master_frame$country_code[which(master_frame$country_code == "ROM")] = "ROU"

# Merge master_frame with English speaking countries
country_frame<- merge(master_frame, engcountry, by="country_code")
country_frame1 <- group_by(country_frame, Country)
top_countries <- summarize(country_frame1, investment = sum(raised_amount_usd))
top9 <- head(arrange(top_countries, desc(investment)), 9)
top3 <- head(arrange(top9, desc(investment)), 3)
message("Top 3 countries are ", top3$Country[1]," ", top3$Country[2], " ", top3$Country[3])


# Checkpoint 5
#1.Extract the primary sector of each category list from the category_list column
#
primary<-separate(country_frame,category_list,into = "primary_sector",sep =c("\\|"),remove = FALSE)
primary<-primary[!(primary$category_list == ""),]      #(to remove blank category_list from the list)

#
#2.Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors
mapping<-read.csv("mapping.csv",stringsAsFactors = F)
#
#Expected Results: Code for a merged data frame with each primary sector mapped to its main sector 
mapping<-gather(mapping,key=main_sector,value = value,Automotive...Sports:Social..Finance..Analytics..Advertising)
mapping<-mapping[!(mapping$value == 0),]      #(to remove 0 from the list)
mapping<-mapping[ ,-3]    #(to remove value column)
colnames(mapping)[1] <- "primary_sector"

master_frame1<-merge(x = primary,y = mapping, by = "primary_sector", all.x = TRUE)
#
# The master_frame1 data frame contains each primary sector mapped to its main sector 
# (the primary sector ispresent in a separate column). 
#


# Table 5/ Checkpoint 6-Sector Analysis 2

for (CountryIdx in 1:3){
  
# For Country 1-D1
  D1<-filter(master_frame1, Country == top3$Country[CountryIdx],funding_round_type == "venture",raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000)
    
# Count of Total investment
  message ("1. Total number of investments in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " = ", nrow(D1) )

  
# 2. Total amount of investment (USD)
  message ("2. Total amount of investment (USD) in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " = ", sum(D1$raised_amount_usd) )

  
# 3. Top sector (based on count of investments)
  D1_sector<-group_by(D1,main_sector)
  D1_summa<-summarise(D1_sector, number = length(main_sector))
  D1_summa<-arrange(D1_summa, desc(number))
  
  message("3. Top sector (based on count of investments) in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " = ", D1_summa$main_sector[1] )
  message("4. Second-best sector (based on count of investments) in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " = ", D1_summa$main_sector[2] )
  message("5. Third-best sector (based on count of investments) in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " = ", D1_summa$main_sector[3] )
  message("6. Number of investments in the top sector in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " = ", D1_summa$number[1])
  message("7. Number of investments in the second-best sector in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " = ", D1_summa$number[2])
  message("8. Number of investments in the third-best sector in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " = ", D1_summa$number[3])
  
  D1_top_sectorcompany<-filter(D1, main_sector == D1_summa$main_sector[1])
  D1_top_company<-group_by(D1_top_sectorcompany,permalink)
  D1_top_company_summa<-summarise(D1_top_company, total_raised_amt_usd = sum(raised_amount_usd))
  D1_top_company_summa<-arrange(D1_top_company_summa, desc(total_raised_amt_usd))

  D1_secondtop_sectorcompany<-filter(D1, main_sector == D1_summa$main_sector[2])
  D1_secondtop_company<-group_by(D1_secondtop_sectorcompany,permalink)
  D1_secondtop_company_summa<-summarise(D1_secondtop_company, total_raised_amt_usd = sum(raised_amount_usd))
  D1_secondtop_company_summa<-arrange(D1_secondtop_company_summa, desc(total_raised_amt_usd))
  
  message("9. In the best sector \"",  D1_summa$main_sector[1], "\" in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " Company that received most investment is ", D1_top_company_summa$permalink[1])
  message("10. In the second-best sector \"",  D1_summa$main_sector[2], "\" in Country ", CountryIdx, " : ", top3$Country[CountryIdx], " Company that received most investment is ", D1_secondtop_company_summa$permalink[1])
  message("\n")

}


