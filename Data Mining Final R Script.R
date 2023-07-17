### Group 18

#Load libraries
library(tidyverse)
library(caret)
library(tree)
library(class)
library(randomForest)
library(gbm)
library(splitstackshape)
library(text2vec)
library(tm)
library(SnowballC)
library(vip)
library(pROC)
library(ROCR)
library(glmnet)
library(vip) 
library(naivebayes)
library(ranger)
library(xgboost)
library(textdata)
library(tidytext)
library(quanteda)
library(Matrix)
library(modeest)
library(stringr)
library(stats)
library(car)
library(openxlsx)
library(ggplot2)
library(factoextra)
library(ggplot2)

options(scipen=999)


#Set seed
set.seed(1)


#####  DATA LOADING

#Load data files
train_x <- read_csv("airbnb_train_x_2023.csv")
train_y <- read_csv("airbnb_train_y_2023.csv")
test_x <- read_csv("airbnb_test_x_2023.csv")
zipcode_train <- read_csv("pop-by-zip-code.csv")  # external dataset


#join the training y to the training x file
#also turn the target variables into factors
train <- cbind(train_x, train_y) %>%
  mutate(high_booking_rate = as.factor(high_booking_rate))


##combine train and test records 
fulldata_df <- rbind(train_x, test_x)

# Add an id column to the dataframe
fulldata_df <- fulldata_df %>% mutate(id = 1:nrow(fulldata_df))

# rename the first column in the external dataset
names(zipcode_train)[1] <- "zipcode"

join_df <- merge(fulldata_df, zipcode_train, by = "zipcode", all.x = TRUE)

# Sort the data frame by the ID column
join_df <- join_df[order(join_df$id),]
join_df$aggregate <- ifelse(is.na(join_df$aggregate), mean(join_df$aggregate, na.rm = TRUE), join_df$aggregate)

# Use cbind to add the aggregate column
fulldata_df <- cbind(fulldata_df, population = join_df$aggregate)
airbnb <- fulldata_df


#####  DATA CLEANING AND PROCESSING

#Define functions


# Classification as 0/1
# define a function that uses scores to classify based on a cutoff c
classify <- function(scores, c){
  classifications <- ifelse(scores > c, 1 , 0) 
  return(classifications) 
}

# define a function to calculate accuracy
accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}


# Calculate and return accuracy
tree_predict_classify <- function(predicting_dataset, tree_name, cutoff){
  
  #make predictions in predicting_dataset (could be train, valid, etc.)
  predictions <- predict(tree_name, newdata = predicting_dataset)
  
  #classify using a cutoff
  classifications <- ifelse(predictions > cutoff, 1, 0)
  
  #calculate and return accuracy
  acc <- accuracy(predicting_dataset$high_booking_rate, classifications)
  return(acc)
}

## Functions for variables. Input: fulldata dataframe which includes train and testdata
features <- function(dataset){
  

  # for cancellation_policy, grouping "strict" and "super_strict_30" into "strict"
  airbnb$cancellation_policy[airbnb$cancellation_policy == "super_strict_30"] <- "strict"
  
  # convert cleaning_fee and price into numbers
  airbnb$cleaning_fee<-parse_number(airbnb$cleaning_fee)
  airbnb$price<-parse_number(airbnb$price)
  
  # convert extra_people into numbers
  airbnb$extra_people<-parse_number(airbnb$extra_people)
  
  
  # convert monthly_price, weekly_price, security_deposit, and zipcode into numbers
  airbnb$monthly_price<-parse_number(airbnb$monthly_price)
  airbnb$weekly_price<-parse_number(airbnb$weekly_price)
  airbnb$security_deposit<-parse_number(airbnb$security_deposit)
  airbnb$zipcode<-parse_number(airbnb$zipcode)
  
  table(airbnb$zipcode)

  
  ## NA Values 
  # replace NAs in cleaning_fee and price with 0
  airbnb$cleaning_fee[is.na(airbnb$cleaning_fee)] <- 0
  airbnb$price[is.na(airbnb$price)] <- 0
  
  
  # replace NAs in monthly_price, weekly_price, and security_deposit with 0
  airbnb$monthly_price[is.na(airbnb$monthly_price)] <- 0
  airbnb$weekly_price[is.na(airbnb$weekly_price)] <- 0
  airbnb$security_deposit[is.na(airbnb$security_deposit)] <- 0
  
  # replace NAs in other numerical variables with their mean
  airbnb$bedrooms[is.na(airbnb$bedrooms)] <- mean(airbnb$bedrooms, na.rm = TRUE)
  airbnb$beds[is.na(airbnb$beds)] <- mean(airbnb$beds, na.rm = TRUE)

  # replace NAs in bathrooms with the median value
  airbnb$bathrooms[is.na(airbnb$bathrooms)] <- median(airbnb$bathrooms, na.rm = TRUE)
  
  # replace NAs in host_is_superhost with FALSE
  airbnb$host_is_superhost[is.na(airbnb$host_is_superhost)] <- FALSE
  
  # replace NAs in square feet with median value
  airbnb$square_feet[is.na(airbnb$square_feet)] <- median(airbnb$square_feet, na.rm = TRUE)
  
  # replace NAs in host_response_time with mode (highest repeated value)  
  airbnb$host_response_time=as.factor(ifelse(is.na(airbnb$host_response_time),
                                             mfv(airbnb$host_response_time),
                                             airbnb$host_response_time))  
  
  # replace NAs in host_has_profile_pic with FALSE
  airbnb$host_has_profile_pic[is.na(airbnb$host_has_profile_pic)] <- FALSE
  
  # replace NAs in is_business_travel_ready with FALSE
  airbnb$is_business_travel_ready[is.na(airbnb$is_business_travel_ready)] <- FALSE 
  
  #replace NAs in host_total_listing_count with 0 
  airbnb$host_total_listings_count[is.na(airbnb$host_total_listings_count)] <- 0
  
  #replace NAs in host_identity_verified with FALSE 
  airbnb$host_identity_verified[is.na(airbnb$host_identity_verified)] <- FALSE
  
  #replace NAs in license with FALSE
  airbnb$requires_license[is.na(airbnb$requires_license)] <- FALSE
  
  # replace NAs in text columns with space
  airbnb$transit[is.na(airbnb$transit)] <- "" 
  airbnb$access[is.na(airbnb$access)] <- "" 
  airbnb$amenities[is.na(airbnb$amenities)] <- "" 
  airbnb$host_verifications[is.na(airbnb$host_verifications)] <- "" 
  airbnb$house_rules[is.na(airbnb$house_rules)] <- "" 
  airbnb$interaction[is.na(airbnb$interaction)] <- "" 
  airbnb$license[is.na(airbnb$license)] <- "" 
  airbnb$jurisdiction_names[is.na(airbnb$jurisdiction_names)] <- "" 
  airbnb$host_neighbourhood[is.na(airbnb$host_neighbourhood)] <- "" 
  airbnb$notes[is.na(airbnb$notes)] <- "" 
  airbnb$neighborhood_overview[is.na(airbnb$neighborhood_overview)] <- "" 
  
  
  
  
  ## New features
  #Creating new column for Bed Convenience 
  airbnb$bed_convenience <- ifelse(airbnb$beds >= airbnb$bedrooms, "YES", "NO")
  airbnb$bed_convenience <- as.factor(airbnb$bed_convenience)
  
  #privacy as per accomodates
  airbnb$privacy_accomodates <- ifelse(airbnb$accommodates > airbnb$bedrooms, "NO", "YES")
  airbnb$privacy_accomodates <- as.factor(airbnb$privacy_accomodates)
  
  
  # create new variables
  airbnb <- mutate(airbnb, 
                   price_per_person = price / accommodates,
                   has_cleaning_fee = ifelse(cleaning_fee > 0, "YES", "NO"),
                   bed_category = ifelse(bed_type == "Real Bed", "bed", "other"),
                   property_category = case_when(
                     property_type %in% c("Apartment","Serviced apartment","Loft") ~ "apartment",
                     property_type %in% c("Bed & Breakfast","Boutique hotel","Hostel") ~ "hotel",
                     property_type %in% c("Townhouse","Condominium") ~ "condo",
                     property_type %in% c("Bungalow","House") ~ "house",
                     TRUE ~ "other")
  )

  airbnb <- mutate(airbnb, 
                   charges_for_extra = as.factor(ifelse(extra_people > 0, "YES", "NO")),
                   host_acceptance = as.factor(ifelse(is.na(host_acceptance_rate), "MISSING", ifelse(host_acceptance_rate == "100%", "ALL", "SOME"))),
                   host_response = as.factor(ifelse(is.na(host_response_rate), "MISSING", ifelse(host_response_rate == "100%", "ALL", "SOME"))),
                   has_min_nights = as.factor(ifelse(minimum_nights > 1, "YES", "NO"))
  )
  
  airbnb <- mutate(airbnb, 
                   has_guests_included = as.factor(ifelse(guests_included > 0, "YES", "NO")),
                   has_weekly_price = as.factor(ifelse(weekly_price > 0 ,"YES", "NO")),
                   has_monthly_price = as.factor(ifelse(monthly_price > 0,"YES", "NO")),
                   has_security_deposit = as.factor(ifelse(security_deposit > 0,"YES", "NO")),
                   # median of minimum_nights is 2, and maximum_nights is 1125 (~3 years)
                   has_minnight_rental = as.factor(ifelse(minimum_nights > median(minimum_nights),"YES", "NO")),
                   has_maxnight_rental = as.factor(ifelse(maximum_nights >= median(maximum_nights),"YES", "NO")),
                   bathroom_luxury = ifelse(bathrooms>0, accommodates / bathrooms,0),
                   privacy = ifelse(bedrooms>0, beds / bedrooms, 0)
                   
  )                 
  
    
  # convert into a factor
  airbnb$property_category = as.factor(airbnb$property_category) 
  
  # create variable ppp_ind to indicate if per person price is greater than median of that property category
  airbnb<- airbnb %>%
    group_by(property_category) %>%  
    mutate(median_category = median(price_per_person, na.rm = TRUE),  #median value for each category
           ppp_ind = ifelse(price_per_person > median_category, 1,0))  
  
  # convert character variables
  airbnb$property_category <- as.factor(airbnb$property_category)   #property category is converted
  airbnb$bed_category <- as.factor(airbnb$bed_category) #bed category is converted
  airbnb$cancellation_policy <- as.factor(airbnb$cancellation_policy) #cancellation policy is converted
  airbnb$room_type <- as.factor(airbnb$room_type) #room type is converted
  airbnb$ppp_ind <- as.factor(airbnb$ppp_ind) #ppp ind is converted

  airbnb$property_type <- as.factor(ifelse(is.na(airbnb$property_type), "MISSING",airbnb$property_type))
  table(airbnb$property_type)  
  
  airbnb$city_name = as.factor(airbnb$city_name)
  table(airbnb$city_name)
  
  #price by property category
  airbnb <- airbnb %>%
    group_by(property_category) %>%
    mutate(category_mean = mean(price),
           overpriced_category = ifelse(price > category_mean, "YES", "NO"))
  
  #overpriced or not
  airbnb$overpriced_category <- as.factor(airbnb$overpriced_category)  
  

  #Total price variable including all costs
  airbnb <- airbnb %>%
    mutate(total_price = price + security_deposit + cleaning_fee) 
  
  
  ## DATE
  # Convert the date column to a proper Date type
  airbnb$host_since <- as.Date(airbnb$host_since)
  
  # Extract the year from the date column
  airbnb$year <- year(airbnb$host_since)
  airbnb$year[is.na(airbnb$year)] <- 2023
  
  # Get the present year
  present_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # Calculate the number of years
  airbnb$host_experience <- present_year - airbnb$year
  
  # Few interactions between columns- checking highly experienced hosts 
  
  airbnb$host_is_experienced <- ifelse(airbnb$host_experience > mean(airbnb$host_experience), "YES", "NO")
  airbnb$host_is_experienced <- as.factor(airbnb$host_is_experienced)
  
  airbnb$host_preferred <- ifelse(airbnb$host_identity_verified == TRUE & airbnb$host_has_profile_pic == TRUE & airbnb$host_response_rate > 70, "YES", "NO")
  airbnb$host_preferred <- as.factor(ifelse(is.na(airbnb$host_preferred), "MISSING",airbnb$host_preferred))
  
  airbnb$host_preferred_accceptance <- ifelse(airbnb$host_identity_verified == TRUE & airbnb$host_has_profile_pic == TRUE & airbnb$host_acceptance_rate > 70, "YES", "NO")
  airbnb$host_preferred_accceptance <- as.factor(ifelse(is.na(airbnb$host_preferred_accceptance), "MISSING",airbnb$host_preferred_accceptance))
  
  airbnb$host_preferred_location <- ifelse(airbnb$is_business_travel_ready == TRUE & airbnb$is_location_exact == TRUE & airbnb$instant_bookable == TRUE, "YES", "NO")
  airbnb$host_preferred_location <- as.factor(airbnb$host_preferred_location)
  
  
  
  ####### Number of instances for features
  ##1. MARKET
  
  # count the number of instances in each market
  market_counts <- table(airbnb$market)
  
  # identify markets with under 300 instances
  under_300_markets <- names(market_counts[market_counts < 300])
  
  # replace under 300 markets with "OTHER"
  airbnb$market[airbnb$market %in% under_300_markets] <- "OTHER"
  
  # replace NAs in market
  airbnb$market[is.na(airbnb$market)] <- "OTHER"
  
  # convert market to a factor variable
  airbnb$market <- as.factor(airbnb$market)
  
  
  #Creating a new variable to check if listing is overpriced or not compared to market mean price
  airbnb <- airbnb %>%
    group_by(market) %>%
    mutate(market_mean_price = mean(price),
           overpriced_market = ifelse(price > market_mean_price, "YES", "NO"))
  
  airbnb$overpriced_market <- as.factor(airbnb$overpriced_market)
  
  #Count of Overpriced Listings
  table(airbnb$overpriced_market)
  
  
  #checking for underpricing by market and property value
  airbnb <- airbnb %>%
    group_by(market, property_category) %>%
    mutate(market_category_mean = mean(price),
           verification_for_underpricing = ifelse(price < market_category_mean, "YES", "NO"))
  
  airbnb$verification_for_underpricing <- as.factor(airbnb$verification_for_underpricing)
  
  table(airbnb$verification_for_underpricing)
  

  

  ##2. SMART LOCATION
  
  airbnb$smart_location <- ifelse(is.na(airbnb$smart_location), "OTHER", airbnb$smart_location)
  # Extract the values until the first comma in the column
  airbnb$smart_location <- sub(",.*", "", airbnb$smart_location)
  # Replace spaces with underscores
  airbnb$smart_location <- gsub(" ", "_", airbnb$smart_location)
  
  # count the number of instances in smart location
  smlocations_counts <- table(airbnb$smart_location)
  
  # identify markets with under 100 instances
  under_100_smlocations <- names(smlocations_counts[smlocations_counts < 100])
  
  # replace under 100 smart locations with "OTHER"
  airbnb$smart_location[airbnb$smart_location %in% under_100_smlocations] <- "OTHER"
  
  # convert smart_location to a factor variable
  airbnb$smart_location <- as.factor(airbnb$smart_location)  
  
  
  
  
  ## 3. STATE   
  
  # replace NAs in smart_location
  airbnb$state[is.na(airbnb$state)] <- "OTHER"
  
  # count the number of instances in state
  state_counts <- table(airbnb$state)
  
  # identify states with under 3000 instances
  under_3000_state <- names(state_counts[state_counts < 3000])
  
  # replace under 3000 states with "OTHER"
  airbnb$state[airbnb$state %in% under_3000_state] <- "OTHER"
  
  # convert state to a factor variable
  airbnb$state <- as.factor(airbnb$state) 
  
  #creating a new variable to check if security deposit is high as per state
  airbnb <- airbnb %>%
    group_by(state) %>%
    mutate(state_mean_deposit = mean(security_deposit),
           high_deposit = ifelse(security_deposit > state_mean_deposit, "YES", "NO"))
  
  airbnb$high_deposit <- as.factor(airbnb$high_deposit)
  
  table(airbnb$high_deposit)
  
  
  
  
  ## 4. ZIPCODE   
  
  airbnb$zipcode <- ifelse(is.na(airbnb$zipcode), "OTHER", airbnb$zipcode)
  # count the number of instances in zipcode
  zipcode_counts <- table(airbnb$zipcode)
  
  # identify zipcode with under 700 instances
  under_700_zipcode <- names(zipcode_counts[zipcode_counts < 700])
  
  # replace under 700 zipcode with "OTHER"
  airbnb$zipcode[airbnb$zipcode %in% under_700_zipcode] <- "OTHER"
  
  # convert zipcode to a factor variable
  airbnb$zipcode <- as.factor(airbnb$zipcode)    


  return(airbnb)
  
}





##### TEXT MINING AND SENTIMENT ANALYSIS


## 1. AMENITIES
datadf <- airbnb %>%
  mutate(
    amenities= gsub(' ', "", amenities),
    amenities= gsub('\\,', " ", amenities)
  )

# Split the amenities column into multiple column
cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% #remove all numbers
    removePunctuation %>% #remove all punctuation
    removeWords(stopwords("en")) %>% #remove stopwords
    stemDocument %>%
    word_tokenizer 
}
it_train = itoken(datadf$amenities, 
                  preprocessor = tolower, 
                  tokenizer = cleaning_tokenizer, 
                  progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab = create_vocabulary(it_train)
vocab_small = prune_vocabulary(vocab)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer = vocab_vectorizer(vocab_small)

# Convert the training documents into a DTM and make it a binary BOW matrix
dtm_train = create_dtm(it_train, vectorizer)
dtm_train_dense <- as.matrix(dtm_train)
dtm_train_df <- as.data.frame(dtm_train_dense)
dim(dtm_train_df)


## 2. HOST VERIFICATIONS
it_train_host = itoken(airbnb$host_verifications, 
                       preprocessor = tolower, 
                       tokenizer = cleaning_tokenizer, 
                       progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_host_verifications = create_vocabulary(it_train_host)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_host_verificaitons = vocab_vectorizer(vocab_host_verifications)

# Convert the training documents into a DTM and make it a binary BOW matrix
dtm_train_hv = create_dtm(it_train_host, vectorizer_host_verificaitons)
dtm_train_dense_hv <- as.matrix(dtm_train_hv)
dtm_train_hv <- as.data.frame(dtm_train_dense_hv)
dim(dtm_train_hv)

## 3. TRANSIT


it_train_transit = itoken(airbnb$transit, 
                          preprocessor = tolower, 
                          tokenizer = cleaning_tokenizer, 
                          progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_transit = create_vocabulary(it_train_transit)
vocab_transit_small = prune_vocabulary(vocab_transit, vocab_term_max = 100)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_transit = vocab_vectorizer(vocab_transit_small)

# Convert the training documents into a DTM and make it a binary BOW matrix
dtm_train_transit = create_dtm(it_train_transit, vectorizer_transit)
dtm_train_dense_transit <- as.matrix(dtm_train_transit)
dtm_train_transit <- as.data.frame(dtm_train_dense_transit)
dim(dtm_train_transit)

##4. HOUSE RULES

it_train_rules = itoken(airbnb$house_rules, 
                        preprocessor = tolower, 
                        tokenizer = cleaning_tokenizer, 
                        progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_rules = create_vocabulary(it_train_rules)
vocab_rules_small = prune_vocabulary(vocab_rules,  vocab_term_max = 100)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_rules = vocab_vectorizer(vocab_rules_small)

# Convert the training documents into a DTM and make it a binary BOW matrix
dtm_train_rules = create_dtm(it_train_rules, vectorizer_rules)
dtm_train_dense_rules <- as.matrix(dtm_train_rules)
dtm_train_rules <- as.data.frame(dtm_train_dense_rules)
dim(dtm_train_rules)

##5. INTERACTIONS
it_train_interaction = itoken(airbnb$interaction, 
                              preprocessor = tolower, 
                              tokenizer = cleaning_tokenizer, 
                              progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_interaction = create_vocabulary(it_train_interaction)
vocab_interac_small = prune_vocabulary(vocab_interaction, vocab_term_max = 300)


#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_interaction = vocab_vectorizer(vocab_interac_small)

dtm_train_interac = create_dtm(it_train_interaction, vectorizer_interaction)
dtm_train_dense_interac <- as.matrix(dtm_train_interac)
dtm_train_interac_df <- as.data.frame(dtm_train_dense_interac)
dim(dtm_train_interac_df)


##6. LICENSE
it_train_license = itoken(airbnb$license, 
                          preprocessor = tolower, 
                          tokenizer = cleaning_tokenizer, 
                          progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_license = create_vocabulary(it_train_license)
vocab_license_small = prune_vocabulary(vocab_license, vocab_term_max = 100)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_license = vocab_vectorizer(vocab_license_small)

dtm_train_license = create_dtm(it_train_license, vectorizer_license)
dtm_train_dense_license <- as.matrix(dtm_train_license)
dtm_train_license <- as.data.frame(dtm_train_dense_license)



##7. JURISDICTION NAMES
datadf <- airbnb %>%
  mutate(
    jurisdiction_names= gsub(' ', "", jurisdiction_names),
    jurisdiction_names= gsub('\\,', " ", jurisdiction_names)
  )

it_train_jurisdiction = itoken(airbnb$jurisdiction_names, 
                               preprocessor = tolower, 
                               tokenizer = cleaning_tokenizer, 
                               progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_jurisdiction = create_vocabulary(it_train_jurisdiction)
vocab_jurisdiction_small = prune_vocabulary(vocab_jurisdiction, vocab_term_max = 100)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_jurisdiction = vocab_vectorizer(vocab_jurisdiction_small)

dtm_train_jurisdiction = create_dtm(it_train_jurisdiction, vectorizer_jurisdiction)
dtm_train_dense_jurisdiction <- as.matrix(dtm_train_jurisdiction)
dtm_train_jurisdiction_df <- as.data.frame(dtm_train_dense_jurisdiction)
dim(dtm_train_jurisdiction_df)


##8. HOST NEIGHBORHOOD
it_train_hneighbourhood = itoken(airbnb$host_neighbourhood, 
                                 preprocessor = tolower, 
                                 tokenizer = cleaning_tokenizer, 
                                 progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_hneighbourhood = create_vocabulary(it_train_hneighbourhood)
vocab_hneighbourhood_small = prune_vocabulary(vocab_hneighbourhood, vocab_term_max = 100)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_hneighbourhood = vocab_vectorizer(vocab_hneighbourhood_small)

dtm_train_hneighbourhood = create_dtm(it_train_hneighbourhood, vectorizer_hneighbourhood)
dtm_train_dense_hneighbourhood <- as.matrix(dtm_train_hneighbourhood)
dtm_train_hneighbourhood_df <- as.data.frame(dtm_train_dense_hneighbourhood)
dim(dtm_train_hneighbourhood_df)

##9. DESCRIPTION
it_train_description = itoken(airbnb$description, 
                              preprocessor = tolower, 
                              tokenizer = cleaning_tokenizer, 
                              progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_description = create_vocabulary(it_train_description)
# vocab_small = prune_vocabulary(vocab, vocab_term_max = 500)
vocab_description_small = prune_vocabulary(vocab_description, vocab_term_max = 100)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_description = vocab_vectorizer(vocab_description_small)

dtm_train_description = create_dtm(it_train_description, vectorizer_description)
dtm_train_dense_description <- as.matrix(dtm_train_description)
dtm_train_description_df <- as.data.frame(dtm_train_dense_description)
dim(dtm_train_description_df)




##10. NOTES FOR SENTIMENT ANALYSIS

it_train_notes = itoken(airbnb$notes, 
                        preprocessor = tolower, 
                        tokenizer = cleaning_tokenizer, 
                        progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_notes = create_vocabulary(it_train_notes)
# vocab_small = prune_vocabulary(vocab, vocab_term_max = 500)
vocab_notes_small = prune_vocabulary(vocab_notes, vocab_term_max = 300)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_notes = vocab_vectorizer(vocab_notes_small)

dtm_train_notes = create_dtm(it_train_notes, vectorizer_notes)
dtm_train_dense_notes <- as.matrix(dtm_train_notes)
dtm_train_notes_df <- as.data.frame(dtm_train_dense_notes)
dim(dtm_train_notes_df)


get_sentiments("bing")

# get the negative and positive sentiment words
bing_negative <- get_sentiments("bing") %>%
  filter(sentiment == 'negative')

bing_positive <- get_sentiments("bing") %>%
  filter(sentiment == 'positive')

# Construct the dictionary
mydict <- dictionary(list(negative = bing_negative$word, positive = bing_positive$word))

# convert the dtm into a dfm (document feature matrix)
dtm_train_notes_dfm <- as.dfm(dtm_train_notes)

# Compare the words to the sentiment lists and count them
sentiments <- dfm_lookup(dtm_train_notes_dfm, mydict, valuetype = 'fixed')

# Convert the sentiments to a data frame
# If there are more positive than negative words, predict P, otherwise predict N
sentiments <- convert(sentiments, to = "data.frame") %>%
  mutate(sent_score = as.factor(ifelse(positive >= negative, 'P', 'N')))

#count of sentiments
table(sentiments$sent_score)

#adding to airbnb
airbnb$sent_score <- sentiments$sent_score

#converting to factor
airbnb$sent_score <- as.factor(airbnb$sent_score)

#using Neighbourhood Overview for sentiment analysis
it_train_neighbour = itoken(airbnb$neighborhood_overview, 
                            preprocessor = tolower, 
                            tokenizer = cleaning_tokenizer, 
                            progressbar = FALSE)

# Step 3: Create the vocabulary from the tokenized itoken object
vocab_neighbour = create_vocabulary(it_train_neighbour)
# vocab_small = prune_vocabulary(vocab, vocab_term_max = 500)
vocab_neighbour_small = prune_vocabulary(vocab_neighbour, vocab_term_max = 100)

#Step 4: Vectorize (convert into DTM)
# Create a vectorizer object using the vocabulary we learned
vectorizer_neighbour = vocab_vectorizer(vocab_neighbour_small)

dtm_train_neighbour = create_dtm(it_train_neighbour, vectorizer_neighbour)
dtm_train_dense_neighbour <- as.matrix(dtm_train_neighbour)
dtm_train_neighbour_df <- as.data.frame(dtm_train_dense_neighbour)
dim(dtm_train_neighbour_df)


# convert the dtm into a dfm (document feature matrix)
dtm_train_neighbour_dfm <- as.dfm(dtm_train_neighbour)

# Compare the words to the sentiment lists and count them
sentiments_neighbour <- dfm_lookup(dtm_train_neighbour_dfm, mydict, valuetype = 'fixed')

# Convert the sentiments to a data frame
# If there are more positive than negative words, predict P, otherwise predict N
sentiments_neighbour <- convert(sentiments_neighbour, to = "data.frame") %>%
  mutate(sent_score_neighbour = as.factor(ifelse(positive >= negative, 'P', 'N')))

#count of sentiments
table(sentiments_neighbour$sent_score_neighbour)

#adding to airbnb
airbnb$sent_score_neighbour <- sentiments_neighbour$sent_score_neighbour

airbnb$sent_score_neighbour <- as.factor(airbnb$sent_score_neighbour)



## CLUSTERING

# select the latitude and longitude columns
coords <- airbnb[, c("latitude", "longitude")]

#try using "elbow method" to select number of centers
possible_centers <- c(1:10)
numcenters <- length(possible_centers)
withins <- rep(0, numcenters)

#for every number of centers, measure the within-ss
for (i in c(1:numcenters)){
  centers <- possible_centers[i]
  km.out=kmeans(coords,centers=centers,nstart=10)
  within <- km.out[[5]]
  withins[i] <- within
}

plot(withins)

# perform clustering using k-means algorithm
k <- 3  # number of clusters
set.seed(1)  # set seed for reproducibility
kmeans_model <- kmeans(coords, centers = k)

# add cluster assignment to the original dataset as a new column
airbnb$cluster <- kmeans_model$cluster

##### FUNCTION CALL

# call function
airbnb <- features(airbnb)

# verify if conversion is successful for above variables
summary(airbnb)


# Replace spaces with underscores
airbnb$market <- as.factor(gsub(" ", "_", levels(airbnb$market), fixed = TRUE)[airbnb$market])
airbnb$room_type <- as.factor(gsub(" ", "_", levels(airbnb$room_type), fixed = TRUE)[airbnb$room_type])
airbnb$host_response_time <- as.factor(gsub(" ", "_", levels(airbnb$host_response_time), fixed = TRUE)[airbnb$host_response_time])


# dataframe rows and columns 
dim(airbnb)


##### FEATURES DATAFRAME FOR MODELING

columns <- c("accommodates","availability_30","availability_90","availability_365","bathrooms","bed_category",
             "bedrooms","beds","charges_for_extra","host_total_listings_count","host_acceptance","host_response",
             "has_min_nights","host_is_superhost", "has_guests_included", "host_has_profile_pic","has_security_deposit",
             "has_monthly_price", "has_weekly_price", "has_minnight_rental", "has_maxnight_rental",
             "host_response_time","is_business_travel_ready","is_location_exact","instant_bookable","market","price","ppp_ind",
             "property_category", "require_guest_phone_verification","square_feet", "requires_license", "bed_convenience", 
             "overpriced_market", "host_identity_verified", "bathroom_luxury",
             "privacy","cluster", "sent_score", "high_deposit", "overpriced_category", "verification_for_underpricing",
             "privacy_accomodates", "total_price", "year", "host_is_experienced", "zipcode", 
             "host_preferred", "host_preferred_accceptance", "host_preferred_location",
             "population", "room_type","extra_people", "weekly_price","monthly_price","minimum_nights",
             "maximum_nights","cancellation_policy","cleaning_fee","guests_included","require_guest_profile_picture"
)

# create the dataframe with the above variables
airbnb_df <- airbnb[columns]
dim(airbnb_df)
summary(airbnb_df)

# learn the various dummy variable values
dummy_matrix <- dummyVars(~. , data=airbnb_df, fullRank = TRUE)
airbnb_dummy_matrix <- predict(dummy_matrix, newdata = airbnb_df)


#########
## Separating train and validation

airbnb_dummy_matrix_df <- as.data.frame(airbnb_dummy_matrix)

airbnb_dummy_matrix_df$`market.East_Bay,_CA` <- NULL
airbnb_dummy_matrix_df$`market.Other_(Domestic)` <- NULL


airbnb_dummy_matrix_train <- airbnb_dummy_matrix_df[1:99981,]
airbnb_dummy_matrix_test <- airbnb_dummy_matrix_df[99982:nrow(airbnb_dummy_matrix_df),]


## Target variable as factor
trgt_y <- subset(train_y, select = -c(perfect_rating_score))
trgt_y$high_booking_rate = as.factor(trgt_y$high_booking_rate)

## Target variable as numeric
tr_y_num <- ifelse(trgt_y$high_booking_rate == "YES",1,0)
tr_y_num_factor <- as.factor(tr_y_num)

# adding target variable to all train data
airbnb_dummy_matrix_train$high_booking_rate <- tr_y_num
  
## Splitting

set.seed(1)
train_matrix <- sample(nrow(airbnb_dummy_matrix_train),.7*nrow(airbnb_dummy_matrix_train))

# split the data into training and validation data using the dummy variables dataframe
tr_x <- airbnb_dummy_matrix_train[train_matrix,]
va_x <- airbnb_dummy_matrix_train[-train_matrix,]


tr_y <- tr_y_num[train_matrix]
va_y <- tr_y_num[-train_matrix]


#####  DATA MODELING - MODEL COMPARISON

##1. LOGISTIC MODEL

## Logistic Model 
logistic_model <- glm(high_booking_rate~., data= tr_x, family = "binomial")

# display summary
summary(logistic_model)

# logistic- predict values in validation data
preds_logistic <- predict(logistic_model, newdata = va_x, type = "response")

# Compute the performance of the model using the prediction classes and the true labels
perf_logistic <- prediction(preds_logistic, va_y)

# Compute the AUC of the Logistic model
auc_logistic <- performance(perf_logistic, "auc")@y.values[[1]]
auc_logistic

##########

### FITTING CURVE for logistice Model

# Convert the probabilities to class predictions based on a chosen threshold 
# va classification
pred_class_logistic <- ifelse(preds_logistic >= 0.5, 1, 0)

# Create a table of the predicted classes and the true labels
accuracy_table <- table(pred_class_logistic, va_y)

# Calculate the accuracy for different threshold values:
thresholds <- seq(0.1, 0.9, by = 0.05)
accuracy_values <- numeric(length(thresholds))

for (i in 1:length(thresholds)) {
  pred_class <- ifelse(preds_logistic >= thresholds[i], 1, 0)
  accuracy_values[i] <- sum(pred_class == va_y) / length(va_y)
  
}


fitting_curve_data <- data.frame(Threshold = thresholds, Accuracy = accuracy_values)

# Plot the fitting curve:
ggplot(fitting_curve_data, aes(x = Threshold, y = Accuracy)) +
  geom_line(color="red") +
  geom_point() +
  labs(title = "Fitting Curve - Logistic Model",
       x = "Threshold- Cutoff",
       y = "Validation Accuracy")

#####
## LEARNING CURVE


# split the data into training and validation data using the dummy variables dataframe
tr_x <- airbnb_dummy_matrix_train[train_matrix,]
va_x <- airbnb_dummy_matrix_train[-train_matrix,]

tr_y <- tr_y_num[train_matrix]
va_y <- tr_y_num[-train_matrix]

#define the training set sizes
train_sizes = c(250,500,1000,1500,3000,5000,10000,20000,40000,50000,60000)
auc_logistic = c(0,0,0,0,0,0,0,0,0,0,0)

# Repeat num_draws times
for (i in c(1:11)){
  
tr_x_sub <- tr_x[1:train_sizes[i],]

## Logistic Model 
logistic_model <- glm(high_booking_rate~., data= tr_x_sub, family = "binomial")

# display summary
summary(logistic_model)

# logistic- predict values in validation data
preds_logistic <- predict(logistic_model, newdata = va_x, type = "response")

# Compute the performance of the model using the prediction classes and the true labels
perf_logistic <- prediction(preds_logistic, va_y)

# Compute the AUC of the model
auc_logistic[i] <- performance(perf_logistic, "auc")@y.values[[1]]
auc_logistic[i]

}


# Create a data frame for the learning curve
learning_curve <- data.frame(train_sizes = train_sizes, auc_logistic = auc_logistic)

# Plot the learning curve
ggplot(data = learning_curve, aes(x = log(train_sizes), y = auc_logistic)) +
  geom_line(color="red") +
  geom_point() +
  labs(x = "Training Data Size", y = "Performance") +
  ggtitle("Learning Curve for Logistic Model") +
  theme_classic()

##############

#########
### Significant variables

# Extract p-values
pvals <- summary(logistic_model)$coefficients[, 4]

# Define threshold for p-values
threshold <- 0.10

# Identify predictors with p-values above threshold
high_pvals <- names(pvals[pvals > threshold])
# Remove predictors with high p-values
airbnb_dummy_matrix_df_x <- airbnb_dummy_matrix_df[, !(names(airbnb_dummy_matrix_df) %in% high_pvals)]


##### Lets check logistic again with significant variables

airbnb_dummy_matrix_train <- airbnb_dummy_matrix_df_x[1:99981,]
airbnb_dummy_matrix_test <- airbnb_dummy_matrix_df_x[99982:nrow(airbnb_dummy_matrix_df),]

# adding target variable to all train data
airbnb_dummy_matrix_train$high_booking_rate <- tr_y_num

## Splitting

set.seed(1)
train_matrix <- sample(nrow(airbnb_dummy_matrix_train),.7*nrow(airbnb_dummy_matrix_train))

# split the data into training and validation data using the dummy variables dataframe
tr_x <- airbnb_dummy_matrix_train[train_matrix,]
va_x <- airbnb_dummy_matrix_train[-train_matrix,]


tr_y <- tr_y_num[train_matrix]
va_y <- tr_y_num[-train_matrix]

#########

logistic_model <- glm(high_booking_rate~., data= tr_x, family = "binomial")

# display summary
summary(logistic_model)

# logistic- predict values in validation data
preds_logistic <- predict(logistic_model, newdata = va_x, type = "response")

# Compute the performance of the model using the prediction classes and the true labels
perf_logistic <- prediction(preds_logistic, va_y)

# Compute the AUC of the model
auc_logistic <- performance(perf_logistic, "auc")@y.values[[1]]
auc_logistic

## accuracy
# make a confusion matrix given a cutoff of 0.5
predicted_classifications_airbnb <- classify(preds_logistic, 0.5)
predicted_classifications_airbnb <- factor(predicted_classifications_airbnb, levels = unique(va_y))

valid_actuals_airbnb <- factor(va_y, levels = unique(va_y))

CM1 = confusionMatrix(data = predicted_classifications_airbnb, 
                      reference = valid_actuals_airbnb,
                      positive = "1")

print(CM1)
print(as.numeric(CM1$overall["Accuracy"]))





##2. TREES

## a. large (unpruned) tree
mycontrol = tree.control(nobs = nrow(tr_x), mincut = 1, minsize = 2, mindev = .001)
full_tree <- tree(high_booking_rate ~ ., data = tr_x, control = mycontrol)
summary(full_tree)

full_tree_probs <- predict(full_tree, newdata = va_x)

## b. pruned tree
pruned_tree <- prune.tree(full_tree, best = 20)
pruned_tree_probs <- predict(pruned_tree, newdata = va_x)

# (For ROC curve it's TPR and FPR)
preds_full <- prediction(full_tree_probs, va_y)
roc_full <- performance(preds_full, "tpr", "fpr")

preds_pruned <- prediction(pruned_tree_probs, va_y)
roc_pruned <- performance(preds_pruned, "tpr", "fpr")

# Measure the AUC
# Compute AUC for the full tree
auc_full <- performance(preds_full, measure = "auc")@y.values[[1]]

# Compute AUC for the pruned tree
auc_pruned <- performance(preds_pruned, measure = "auc")@y.values[[1]]



############ 
## accuracy
# make a confusion matrix given a cutoff of 0.5
predicted_classifications_trees <- classify(full_tree_probs, 0.5)
predicted_classifications_trees <- factor(predicted_classifications_trees, levels = unique(va_y))

valid_actuals_airbnb <- factor(va_y, levels = unique(va_y))

CM2 = confusionMatrix(data = predicted_classifications_trees, 
                      reference = valid_actuals_airbnb,
                      positive = "1")

print(CM2)
print(as.numeric(CM2$overall["Accuracy"]))


####### ROC Curve

# Create a data frame with ROC curve data for the full tree
roc_full_data <- data.frame(
  FPR = roc_full@x.values[[1]],
  TPR = roc_full@y.values[[1]]
)

# Create a data frame with ROC curve data for the pruned tree
roc_pruned_data <- data.frame(
  FPR = roc_pruned@x.values[[1]],
  TPR = roc_pruned@y.values[[1]]
)

# Create the fitting curve plot using ggplot
ggplot() +
  geom_line(data = roc_full_data, aes(x = FPR, y = TPR), color = "red", size = 1) +
  geom_line(data = roc_pruned_data, aes(x = FPR, y = TPR), color = "blue", size = 1) +
  labs(title = "ROC Curve - Large Tree vs Pruned Tree",
       x = "False Positive Rate",
       y = "True Positive Rate",
       caption = paste("AUC (Full Tree):", round(auc_full, 4),
                       "\nAUC (Pruned Tree):", round(auc_pruned, 4))) +
  theme_minimal()

##3. RANDOM FOREST

rf.mod <- randomForest(high_booking_rate~.,
                       data=tr_x,
                       mtry=10, ntree=500, strata = 0.3,
                       importance=TRUE)


preds_rf <- predict(rf.mod, newdata=va_x, type = "prob")
preds_rf <- preds_rf[,2]
predictions_rf <- prediction(preds_rf, va_y)

roc_rf_full <- performance(predictions_rf, "tpr", "fpr")
plot(predictions_rf, col = "red", lwd = 2)

performance(predictions_rf, measure = "auc")@y.values[[1]]


##4. RIDGE MODEL

# create a grid of 100 lambda values
grid <- 10^seq(-7, 7, length = 100)


tr_sub <- tr_x
va_sub <- va_x

tr_sub$high_booking_rate <- NULL
va_sub$high_booking_rate <- NULL

tr_mx <- as.matrix(tr_sub)
va_mx <- as.matrix(va_sub)

#train a ridge model with lambda
cv.out <- cv.glmnet(tr_mx, tr_y, family="binomial", alpha=0, lambda=grid, nfolds=5)


#Plot the fitting curve 
plot(cv.out, xlab = "Lambda", ylab = "Cross-Validated Error")
title(main = "Fitting Curve - Ridge Model", line = 2.5, cex.main = 1.2)
#get the lambda that gave the lowest cross-validated error
bestlam_ridge <- cv.out$lambda.min
bestlam_ridge


#Make predictions with the best lambda ridge model
preds_ridge <- predict(cv.out, s=bestlam_ridge, newx = va_mx ,type="response")

# Compute the performance of the model using the prediction classes and the true labels
prediction_ridge <- prediction(preds_ridge, va_y)

# Compute the AUC of the ridge model
auc_ridge <- performance(prediction_ridge, "auc")@y.values[[1]]
auc_ridge


##5. LASSO MODEL

## Lasso Model 
#train a lasso model with lambda
cv.out <- cv.glmnet(tr_mx, tr_y, family="binomial", alpha=1, lambda=grid, nfolds=5)

#Plot fitting curve
plot(cv.out, xlab = "Lambda", ylab = "Cross-Validated Error")
title(main = "Fitting Curve - Lasso Model", line = 2.5, cex.main = 1.2)
#get the lambda that gave the lowest cross-validated error
bestlam_lasso <- cv.out$lambda.min
bestlam_lasso

#get the lambda that gave the lowest cross-validated error
bestlam_lasso <- cv.out$lambda.min
bestlam_lasso


#Make predictions from the best lasso model
preds_lasso <- predict(cv.out, s=bestlam_lasso, newx = va_mx ,type="response")

# Compute the performance of the model using the prediction classes and the true labels
prediction_lasso <- prediction(preds_lasso, va_y)

# Compute the AUC of the lasso model
auc_lasso <- performance(prediction_lasso, "auc")@y.values[[1]]
auc_lasso


##6. KNN

# Function to calculate accuracy
calculate_accuracy <- function(predicted, actual) {
  correct_predictions <- sum(predicted == actual)
  total_predictions <- length(predicted)
  accuracy <- correct_predictions / total_predictions
  return(accuracy)
}

knn.pred=knn(tr_sub, #the training instances' features
             va_sub, #the new instances we want to make predictions for
             tr_y, #the y values in the training data
             k=5, #choose k
             prob = TRUE) #get probabilities as well
knn.pred

accuracy(knn.pred, va_y)
knn.probs <- attr(knn.pred, "prob")
knn.probs
knn_probofYES <- ifelse(knn.pred == "YES", knn.probs, 1-knn.probs)
knn_probofYES

# Now we could classify with a different cutoff

knn_class.5 <- ifelse(knn_probofYES >= 0.5, 1, 0)
accuracy.5 <- accuracy(knn_class.5, va_y)


# Define the K values to test
k_values <- c(1,2,3,4,5)

for (k in k_values) {
  
  # Fit the KNN model
  knn_model <- knn(train = tr_x, test = va_x, cl = tr_y, k = k)
  
  # Make predictions on validation data
  knn_predictions <- as.factor(knn_model)
  
  # Calculate accuracy
  knn_accuracy <- calculate_accuracy(knn_predictions, va_y)
  
  # Print accuracy
  print(paste("KNN Accuracy:", knn_accuracy)) 
  
}

### Fitting curve for KNN
# Function to calculate accuracy
calculate_accuracy <- function(predicted, actual) {
  correct_predictions <- sum(predicted == actual)
  total_predictions <- length(predicted)
  accuracy <- correct_predictions / total_predictions
  return(accuracy)
}

# Define the K values to test
k_values <- c(2, 5, 10, 20, 100,200)

for (k in k_values) {
  
  # Fit the KNN model
  knn_model <- knn(train = tr_x, test = va_x, cl = tr_y, k = k)
  
  # Make predictions on validation data
  knn_predictions <- as.factor(knn_model)
  
  # Calculate accuracy
  knn_accuracy <- calculate_accuracy(knn_predictions, va_y)
  
  # Print accuracy
  print(paste("KNN Accuracy:", knn_accuracy)) 
  
}

accuracy_knn =c(0.702350391731955,0.695182530421737,0.725420903483914, 0.721820303383897, 0.734455742623771)
fitting_curve_data_knn <- data.frame(k = k_values, Accuracy = accuracy_knn)

# Plotting the fitting curve
ggplot(fitting_curve_data_knn , aes(x = k_values, y = accuracy_knn)) +
  geom_line(color="red") +
  geom_point() +
  labs(title = "Fitting Curve - KNN Model",
       x = "k",
       y = "Validation Accuracy")


######


##7. RANGER- RANDOM FOREST

tr_rf_y <- as.factor(tr_y)
va_rf_y <- as.factor(va_y)

ranger.mod <- ranger(x = tr_mx, y = tr_rf_y,
                 mtry=10, num.trees=500,
                 importance="impurity",
                 probability = TRUE)


#Estimate predictions in the validation set
preds_ranger  <- predict(ranger.mod, data=va_mx)$predictions[,2]

#Compute the performance of the model using the prediction classes and the true labels
prediction_ranger <- prediction(preds_ranger, va_rf_y)

#Compute the AUC for ranger model
auc_rf <- performance(prediction_ranger, measure = "auc")@y.values[[1]]
auc_rf


### ROC CURVE for Ranger-RF

# Compute the True Positive Rate (TPR) and False Positive Rate (FPR)
perf_ranger <- performance(prediction_ranger, "tpr", "fpr")
tpr_ranger  <- perf_ranger@y.values[[1]]
fpr_ranger  <- perf_ranger@x.values[[1]]

# Create a data frame with TPR and FPR values
roc_data <- data.frame(FPR = fpr_ranger, TPR = tpr_ranger)

# Create the fitting curve plot using ggplot
ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "ROC Curve - Ranger Random Forest", x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()


##8. BOOSTING

boost <- xgboost(data = tr_mx, label = tr_y, max.depth = 5, eta = 0.04, nrounds = 500,  objective = "binary:logistic", verbosity = 0, verbose = 0)

#Estimate predictions in the validation set
preds_boost <- predict(boost, va_mx)

#Compute the performance of the model using the prediction classes and the true labels
prediction_boost <- prediction(preds_boost, va_y)

#Compute the AUC for boosting model
auc_boost <- performance(prediction_boost, measure = "auc")@y.values[[1]]
auc_boost

### ROC curves for Boosting Model


# Create the roc object
roc_boost <- performance(prediction_boost, "tpr", "fpr")

# Extract TPR and FPR values
tpr_boost <- roc_boost@y.values[[1]]
fpr_boost <- roc_boost@x.values[[1]]

# Create the data frame for plotting
roc_data <- data.frame(fpr = fpr_boost, tpr = tpr_boost)

# Create the ROC curve plot
roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
  geom_line(color = "blue", size = 1.5) +
  labs(x = "False Positive Rate", y = "True Positive Rate", title = "ROC Curve -Boosting Model") +
  theme_minimal()

roc_plot

### Boosting Learning Curve

#define the training set sizes
train_sizes = c(250,500,1000,1500,3000,5000,10000,20000,40000,50000,60000)
auc_boosting = c(0,0,0,0,0,0,0,0,0,0,0)

# Repeat num_draws times
for (i in c(1:11)){
  
  tr_mx_s <- tr_mx[1:train_sizes[i],]
  tr_y_s <- tr_y[1:train_sizes[i]]
  
  boosting <- xgboost(data = tr_mx_s, label = tr_y_s, max.depth = 1, eta = 1, nrounds = 500,  objective = "binary:logistic", verbosity = 0, verbose = 0)
  
  #Estimate predictions in the validation set
  preds_boosting <- predict(boosting, va_mx)
  
  #Compute the performance of the model using the prediction classes and the true labels
  prediction_boosting <- prediction(preds_boosting, va_y)
  
  #Compute the AUC
  auc_boosting[i] <- performance(prediction_boosting, measure = "auc")@y.values[[1]]
  auc_boosting[i]
  
  
}

# Create a data frame for the learning curve
learning_curve_bst <- data.frame(train_sizes = train_sizes, auc_boosting = auc_boosting)

# Plot the learning curve
ggplot(data = learning_curve_bst, aes(x = log(train_sizes), y = auc_boosting)) +
  geom_line(color="red") +
  geom_point() +
  labs(x = "Training Data Size", y = "Performance") +
  ggtitle("Learning Curve for Boosting Model") +
  theme_classic()


## BOOSTING HYPER PARAMETER TUNING

grid_search <- function(){
  
  #three hyperparameters can possibly really change predictive performance of xgboost (although maybe not)
  depth_choose <- c(1,2,3,4,5)
  nrounds_choose <- c(500)
  eta_choose <- c(0.8,0.9,1)
  
  #nested loops to tune these three parameters
  print('depth, nrounds, eta, auc')
  for(i in c(1:length(depth_choose))){
    for(j in c(1:length(nrounds_choose))){
      for(k in c(1:length(eta_choose))){
        thisdepth <- depth_choose[i]
        thisnrounds <- nrounds_choose[j]
        thiseta <- eta_choose[k]
        
        inner_bst <- xgboost(data = tr_mx, label = tr_y, max.depth = thisdepth, eta = thiseta, nrounds = thisnrounds,  objective = "binary:logistic", verbosity = 0, verbose = 0)
        
        inner_bst_pred <- predict(inner_bst, va_mx)
        
        # Calculate validation AUC
        bst_full <- prediction(inner_bst_pred, va_y)
        bst_auc <- performance(bst_full, measure = "auc")@y.values[[1]] 
        
        #print the performance for every combination
        print(paste(thisdepth, thisnrounds, thiseta, bst_auc, sep = ", "))
        
      }
    }
  }
}


grid_search()

## Boosting Fitting Curve
auc_boost_x =c(0.873996052351919,0.884671773338708,0.875357751127626, 0.866515603624871, 0.861333926310494)
fitting_curve_data_bst <- data.frame(depth_choose = c(1,2,3,4,5), Auc = auc_boost_x)

# Plotting the fitting curve
ggplot(fitting_curve_data_bst , aes(x = depth_choose, y = Auc)) +
  geom_line(color="red") +
  geom_point() +
  labs(title = "Fitting Curve - Boosting Model",
       x = "depth_choose",
       y = "Validation AUC")



### WE WILL BE CHOOSING BOOSTING MODEL FOR CONTEST PREDICTIONS FURTHER BELOW


####### RUBRIC TASKS
##### TASK 1 - USING LOGISTIC MODEL

#Incorporate an external data source (beyond the Airbnb data that I have provided) to 
#create at least one additional feature variable. Provide evidence in your report that
#the new feature variable(s) result in a performance improvement.

airbnb_dummy_mx_df <- subset(airbnb_dummy_matrix_df, select = -c(population))

airbnb_dummy_matrix_train <- airbnb_dummy_mx_df[1:99981,]
airbnb_dummy_matrix_test <- airbnb_dummy_mx_df[99982:nrow(airbnb_dummy_matrix_df),]

# adding target variable to all train data
airbnb_dummy_matrix_train$high_booking_rate <- tr_y_num

## Splitting

set.seed(1)
train_matrix <- sample(nrow(airbnb_dummy_matrix_train),.7*nrow(airbnb_dummy_matrix_train))

# split the data into training and validation data using the dummy variables dataframe
tr_x <- airbnb_dummy_matrix_train[train_matrix,]
va_x <- airbnb_dummy_matrix_train[-train_matrix,]


tr_y <- tr_y_num[train_matrix]
va_y <- tr_y_num[-train_matrix]

#########

logistic_model <- glm(high_booking_rate~., data= tr_x, family = "binomial")

# display summary
summary(logistic_model)

# logistic- predict values in validation data
preds_logistic <- predict(logistic_model, newdata = va_x, type = "response")

# Compute the performance of the model using the prediction classes and the true labels
perf_logistic <- prediction(preds_logistic, va_y)

# Compute the AUC of the logistic model without external dataset feature-population
auc_logistic <- performance(perf_logistic, "auc")@y.values[[1]]
auc_logistic

# Note: LOGISTIC AUC: 0.8433801

##### TASK 2 - USING BOOSTING MODEL

#Use at least one unstructured text field to create new features. 
#Provide evidence in your report that the features derived resulting performance improvement.

amenities_mx <- as.matrix(dtm_train)
hv_mx <- as.matrix(dtm_train_hv)
transit_mx <- as.matrix(dtm_train_transit)
rules_mx <- as.matrix(dtm_train_rules)
interactions_mx <- as.matrix(dtm_train_interac_df)
license_mx <- as.matrix(dtm_train_license)
neighbour_mx <- as.matrix(dtm_train_neighbour_df)
jurisdiction_mx <- as.matrix(dtm_train_jurisdiction_df)
hneighbour_mx <- as.matrix(dtm_train_hneighbourhood_df)
description_mx <- as.matrix(dtm_train_description_df)

airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix,amenities_mx)
airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix_txt,hv_mx)
airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix_txt,transit_mx)
airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix_txt,rules_mx)
airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix_txt,interactions_mx)
airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix_txt, license_mx)
airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix_txt, neighbour_mx)
airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix_txt, jurisdiction_mx)
airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix_txt, hneighbour_mx)
airbnb_dummy_matrix_txt <- cbind(airbnb_dummy_matrix_txt, description_mx)

airbnb_dummy_mx_df <- as.data.frame(airbnb_dummy_matrix_txt)

airbnb_dummy_matrix_train <- airbnb_dummy_mx_df[1:99981,]
airbnb_dummy_matrix_test <- airbnb_dummy_mx_df[99982:nrow(airbnb_dummy_matrix_df),]

# adding target variable to all train data
airbnb_dummy_matrix_train$high_booking_rate <- tr_y_num

## Splitting

set.seed(1)
train_matrix <- sample(nrow(airbnb_dummy_matrix_train),.7*nrow(airbnb_dummy_matrix_train))

# split the data into training and validation data using the dummy variables dataframe
tr_x <- airbnb_dummy_matrix_train[train_matrix,]
va_x <- airbnb_dummy_matrix_train[-train_matrix,]

tr_y <- tr_y_num[train_matrix]
va_y <- tr_y_num[-train_matrix]

tr_sub <- tr_x
va_sub <- va_x

tr_sub$high_booking_rate <- NULL
va_sub$high_booking_rate <- NULL

tr_mx <- as.matrix(tr_sub)
va_mx <- as.matrix(va_sub)

#CONTEST MODEL
## THIS IS GOING TO TAKE A LONG TIME - BE PATIENT

inner_bst <- xgboost(data = tr_mx, label = tr_y, max.depth = 15, eta = 0.04, nrounds = 1000,
                     objective = "binary:logistic",nthread = num_cores, verbosity = 0, verbose = 0)

#Estimate predictions in the validation set
preds_boost <- predict(boost, va_mx)

#Compute the performance of the model using the prediction classes and the true labels
prediction_boost <- prediction(preds_boost, va_y)

#Compute the AUC
auc_boost <- performance(prediction_boost, measure = "auc")@y.values[[1]]
auc_boost

###### GRAPHS AND TABLES

library(ggplot2)

#1. Distribution of Airbnb listings by state

# Assuming your dataset is named 'airbnb' and the city variable contains city names
ggplot(airbnb, aes(x = state)) +
  geom_bar(fill = "skyblue", color = "white") +
  labs(x = "City", y = "Count") +
  ggtitle("Distribution of Listings by State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#2. Distribution of Airbnb Listings by Cancellation Policy
# Assuming your dataset is named 'airbnb' and the cancellation_policy variable represents cancellation policies
cancel_counts <- table(airbnb$cancellation_policy)

#Converting to data frame
cancel_counts_df <- as.data.frame(cancel_counts)

#Ordering frequency in descending order
cancel_counts_df$Var1 <- factor(cancel_counts_df$Var1, levels = cancel_counts_df$Var1[order(cancel_counts_df$Freq, decreasing = TRUE)])

#Plotting Frequency distribution of cancellation policy
ggplot(data = cancel_counts_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", width = 0.8, fill = "#009E73") +
  labs(x = "Cancellation Policy", y = "Frequency") +
  ggtitle("Cancellation Policy Distribution") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))


#3. Distribution of Property Type Listings on Airbnb

property_counts <- table(airbnb$property_category)

#Converting to data frame
df_property <- as.data.frame(property_counts)

#Ordering frequency in ascending order
df_property$Var1 <- factor(df_property$Var1, levels = df_property$Var1[order(df_property$Freq, decreasing = FALSE)])

#Distribution of Property Type Listings on Airbnb
ggplot(df_property, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "#E69F00") +
  labs(x = "Property Type", y = "Frequency") +
  ggtitle("Distribution of Property Types") +
  coord_flip()

#4. Distribution of Room Types in Each Property Category on Airbnb

# Creating a table containing the 'property_category' and 'room_type' variables
room_prop_counts <- table(airbnb$property_category, airbnb$room_type)

# Compute the proportion of each room type within each property type
room_prop_counts_perc <- prop.table(room_prop_counts, margin = 1)

#Converting to data frame
room_prop_counts_perc <- as.data.frame(room_prop_counts_perc)

# Convert the proportion data frame into long format
df_room_prop_long <- reshape2::melt(room_prop_counts_perc)

# Create a stacked bar chart
ggplot(df_room_prop_long, aes(x = Var1, y = value, fill = Var2)) +
  geom_bar(stat = "identity") +
  labs(x = "Property Type", y = "Proportion", fill = "Room Type") +
  ggtitle("Proportion of Room Types within Property Types") +
  theme_minimal() +
  theme(legend.position = "top")


#5. Analyzing the relationship between number of accommodates and pricing of listing
ggplot(airbnb, aes(x = accommodates, y = price)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Number of Accommodates", y = "Price") +
  ggtitle("Relationship between Accommodates and Price") +
  theme_minimal()


#6. Distribution of Average Price across Markets on Airbnb
avg_price_by_market <- aggregate(price ~ market, data = airbnb, FUN = mean)

exclude_markets <- c("OTHER")  # Specify the markets to be excluded
avg_price_by_market <- subset(avg_price_by_market, !market %in% exclude_markets)

#Ordering the markets in ascending order of average price
avg_price_by_market <- avg_price_by_market[order(avg_price_by_market$price), , drop = FALSE]
avg_price_by_market$market <- factor(avg_price_by_market$market, levels = avg_price_by_market$market)

#Plotting the distribution of average price per market
ggplot(avg_price_by_market, aes(x = market, y = price)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Market", y = "Average Price") +
  ggtitle("Average Price across Markets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


#7. Price Distribution by Bed Category on Airbnb
ggplot(airbnb, aes(x = bed_category, y = price)) +
  geom_boxplot(fill = "skyblue", color = "black", width = 0.8) +
  labs(x = "Bed Type", y = "Price") +
  ggtitle("Price Distribution by Bed Category") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 2000), breaks = seq(0, 2000, 200))


#8. Analyzing the Relationship between the number of beds and privacy ratio of customers on Airbnb
library(plotly)

# Create the scatter plot to show the relationship
bed_privacy <- plot_ly(airbnb, x = ~beds, y = ~privacy, mode = "markers", type = "scatter", text = ~paste("Beds:", beds, "Privacy Ratio:", privacy)) %>%
  add_trace(data = airbnb, x = ~beds, y = fitted(lm(privacy ~ beds, data = airbnb)), 
            mode = "lines", type = "scatter") %>%
  layout(xaxis = list(title = "Number of Beds"), yaxis = list(title = "Privacy Ratio"), 
         title = "Scatter Plot: Number of Beds vs Privacy",
         showlegend = FALSE)

#Display Plot
bed_privacy


#9. Analyzing Host Acceptance vs Cancellation Policy
accept_cancel_counts <- table(airbnb$host_acceptance, airbnb$cancellation_policy)

accept_cancel_counts <- as.data.frame(accept_cancel_counts)

# Exclude specific values from the dataset
accept_cancel_counts_filtered <- subset(accept_cancel_counts, !(Var1 %in% c("MISSING")))
accept_cancel_counts_filtered <- subset(accept_cancel_counts_filtered, !(Var2 %in% c("no_refunds")) )

# Create the clustered bar chart
accept_cancel <- ggplot(accept_cancel_counts_filtered, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "Cancellation Policy by Host Acceptance",
       x = "Host Acceptance",
       y = "Frequency", 
       fill = "Host Acceptance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convert the ggplot object to plotly
plotly_chart <- ggplotly(accept_cancel)


# Display the interactive plot
plotly_chart

#10. Analyzing overpriced listings in each market on Airbnb

# Create the table
table_data <- table(airbnb$market, airbnb$overpriced_market)

# Convert the table to a data frame
df <- as.data.frame(table_data)

# Exclude the non-overpriced listings 
excluded_values <- c("NO")  # Specify the values to exclude
table_data_excluded <- subset(df, !Var2 %in% excluded_values)

# Sort the data frame by number of overpriced listings in descending order
sorted_df <- arrange(table_data_excluded, desc(Freq))
sorted_df <- sorted_df[, c(-2)]
# Rename the columns
new_column_names <- c("Market", "Total Overpriced Listings")  # Specify the new column names
dimnames(sorted_df)[[2]] <- new_column_names

#Display sorted 
print(sorted_df)

