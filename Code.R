setwd("G:/Data Visualisation/Final Project")

library(tidyverse)
library(highcharter) 
library(lubridate)
library(stringr)
library(xts)

options(scipen=20000)

playstore <- read.csv("googleplaystore.csv")

sentiments <- read.csv("googleplaystore_user_reviews.csv")

#########################################################################

data_clean <- playstore %>%
  mutate(
    # Eliminate some characters to transform Installs to numeric
    Installs = gsub("\\+", "", as.character(Installs)),
    Installs = as.numeric(gsub(",", "", Installs)),
    # Eliminate M to transform Size to numeric
    Size = gsub("M", "", Size),
    # Replace cells with k to 0 since it is < 1MB
    Size = ifelse(grepl("k", Size), 0, as.numeric(Size)),
    # Transform reviews to numeric
    Reviews = as.numeric(Reviews),
    # Remove currency symbol from Price, change it to numeric
    Price = as.numeric(gsub("\\$", "", as.character(Price))),
    # Last Updated to date format
    Last_Updated = mdy(Last_Updated),
    # Replace "Varies with device" to NA since it is unknown
    Android_Version = gsub("Varies with device", NA, Android_Version),
    # Keep only version number to 1 decimal
    Android_Version = as.numeric(substr(Android_Version, start = 1, stop = 3)),
    # Drop old Android version column
    #Android_Version = NULL
  ) %>%
  filter(
    # Two apps had type as 0 or NA, they will be removed 
    Type %in% c("Free", "Paid")
  )

str(data_clean)

##############################################################################

data_clean <- data_clean %>% distinct()

sentiments_clean <- sentiments %>% distinct()

##############################################################################



library(hablar)
data_clean1 <- na.omit(data_clean)
sentiments_clean1 <- na.omit(sentiments_clean)


write.csv(data_clean1,"G:\\Data Visualisation\\Final Project\\googleplaystore_new.csv", row.names = TRUE)
write.csv(sentiments_clean1,"G:\\Data Visualisation\\Final Project\\googleplaystore_sentimentsnew.csv", row.names = TRUE)

data_clean1 %>% 
  group_by(Category, Type) %>%
  summarize(
    n = n()
  ) %>%
  mutate(perc = round((n /sum(n))*100)) %>%
  hchart('bar', hcaes(x = 'Category', y = 'perc', group = 'Type')) %>%
  hc_plotOptions(series=list(stacking='normal')) %>%
  hc_title(text="Percentage of Free vs Paid by Category") %>%
  hc_add_theme(hc_theme_ffx())

data_clean2 <- as_tibble(data_clean1)
tmp <- data_clean2 %>%
  count(Type) %>%
  mutate(perc = round((n /sum(n))*100)) %>%
  arrange(desc(perc))

hciconarray(tmp$Type, tmp$perc, icons = "android", size = 2) %>%
  hc_title(text="Percentage of paid vs free apps")


data_clean1 %>%
 filter(Android_Version > 0, Type %in% c("Free", "Paid")
   ) %>%
 group_by(as.factor(Android_Version), Type) %>%
 rename(Android_Ver = "as.factor(Android_Version)") %>%
 summarize(Total_Installs = sum(Installs)) %>%
 hchart(
   type = "heatmap", hcaes(x = Android_Ver, y = Type, value = Total_Installs)
   ) %>%
 hc_title(text = "Minimum Android Version (by number of installs)")



cor_matrix = cor(data_clean1)
corrplot(cor_matrix, ,method = "color",order = "AOE",addCoef.col = "grey")

library(ggplot2)
library(hrbrthemes)
p <- ggplot(data_clean1, aes(x=Price, y=Rating)) + ggtitle("Rating Vs Price Comparisons") +
  geom_point(color = "orange", size = 2) +
  theme_bw()
p + theme(plot.title = element_text(hjust = 1))
p


library(ggplot2)
library(hrbrthemes)
q <- ggplot(playstore, aes(x=Reviews, y=Rating)) + ggtitle("Rating Vs Review Comparisons") +
  geom_point(color = "orange", size = 2)  +
  theme_bw()
q + theme(plot.title = element_text(hjust = 1))
q

data_clean1 %>%
  filter(Type == "Paid") %>%
  group_by(Category) %>%
  summarize(
    Price = median(Price)
  ) %>%
  arrange(-Price) %>%
  hchart('treemap', hcaes(x = 'Category', value = 'Price')) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_title(text="Median price per category")


data_clean1 %>%
  filter(Type == "Paid") %>%
  mutate(
    Total.Paid = Price * Installs
  ) %>%
  group_by(Category) %>%
  summarize(USD.Paid = sum(Total.Paid)) %>%
  arrange(-USD.Paid) %>%
  hchart('treemap', hcaes(x = 'Category', value = 'USD.Paid', color = 'USD')) %>%
  hc_add_theme(hc_theme_elementary()) %>%
  hc_title(text="Total amount spent by category (installs * price)")

#########################################################################################################

library(readr)
library(dplyr)
library(e1071)
library(mlbench)

#Text mining packages
library(tm)
library(SnowballC)
library("wordcloud")
library("RColorBrewer")

# Create corpus
corpus = Corpus(VectorSource(sentiments_clean1$Translated_Review))
# Look at corpus
corpus[[1]][1]

#Conversion to Lowercase
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, tolower)

#Removing Punctuation
corpus = tm_map(corpus, removePunctuation)

#Remove stopwords
corpus = tm_map(corpus, removeWords, c("cloth", stopwords("english")))

# Stemming
corpus = tm_map(corpus, stemDocument)

# Eliminate white spaces
corpus = tm_map(corpus, stripWhitespace)
corpus[[1]][1] 

DTM <- TermDocumentMatrix(corpus)
mat <- as.matrix(DTM)
f <- sort(rowSums(mat),decreasing=TRUE)
dat <- data.frame(word = names(f),freq=f)
head(dat, 5)




sentiments_clean1 %>%
  filter(Sentiment %in% c("Positive", "Negative")
  ) %>%
  group_by(as.factor(Sentiment)) %>%

  summarize(Sum = sum(App)) %>%
  hchart(
    type = "barplot", hcaes(x = Sum, y = App_Name, value = Sum)
  ) %>%
  hc_title(text = "Minimum Android Version (by number of installs)")


library(streamgraph)


# Basic stream graph: just give the 3 arguments
pp <- streamgraph(data_clean1, key="App", value="Last_Updated", date="Last_Updated", height="300px", width="1000px")
pp 

plot(data_clean1$Rating, data_clean1$Reviews,
     xlim=c(0,250) , ylim=c(0,250), 
     pch=18, 
     cex=2, 
     col="#69b3a2",
     xlab="value of X", ylab="value of Y",
     main="A simple scatterplot"
)

hcboxplot(x = sentiments_clean1$Sentiment_Polarity, var = sentiments_clean1$Sentiment, outliers = TRUE, color = "#fb4901", fillColor = "lightblue") %>%
  hc_chart(type = "column") %>%
  hc_add_theme(hc_theme_ffx()) %>%
  hc_title(text = "Application size range (in MB) by Application Type")


dfcorr <- subset(data_clean1, select = c(Rating, Size, Reviews, Installs, Price))

##  different color series
col1 <- colorRampPalette(c("orange", "orange", "#FF7F00", "yellow", "white",
                           "cyan", "#007FFF", "skyblue","#00007F"))
col2 <- colorRampPalette(c("orange", "skyblue"))


cor_matrix = cor(dfcorr)
library(corrplot)
corrplot(cor_matrix,method = "color",order = "AOE",addCoef.col = "grey", col = col2(20))

library(plotly)
plot_ly(z = dfcorr, type = "heatmap")
