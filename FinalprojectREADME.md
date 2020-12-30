# AU588081_Leidinger_Simone

# I activate tidyverse
library(tidyverse)

# I read the original csv-file from url, which allows me to directly download the file
data <- read_csv("https://raw.githubusercontent.com/aarhusstadsarkiv/datasets/master/citizenship/1740-1862/citizenship-records-1740-1862-original.csv")

# I want to see the data I use view(data) - the column I am interested in is called "hovederhverv"
view(data)

# I can see that the data are very messy, I clean the columns oprindelsessted (place of origin) and hovederhverv (mainoccupation) with open refine
# Then I import the cleaned dataset from OpenRefine to RStudio

# I have to rename the cleaned dataset citizenship.records.1740.1862.original.csv.2.kopi.csv
newdata <- citizenship.records.1740.1862.original.csv.2.kopi.csv

# Newdata looks fine now.
view(newdata)

# I select column V15 which is oprindelsessted1 (place of origin)
oprindelsessted1 <- select(newdata, V15)

# I select column V16 which is oprindelsessted2 (place of origin)
oprindelsessted2 <- select(newdata, V16)

# To find the most frequent places where people were coming from I make a vector. The code I found in the URL:
oprindelsessted2nr <- sort(table(oprindelsessted2),decreasing=TRUE)[1:15]

#I have a look at the vector:
view(oprindelsessted2nr)

# To choose only the rows from a special year (aar - column V(8) I use the filter-function
# In the year 1750 21 persons gained citizenship, most of them are merchants and skippers
filter(newdata, V8 == 1750)

# In the year 1815 29 persons gained citizenship, there are many merchants and skippers, but also many craftsmen. 
filter(newdata, V8 == 1815)

# In the year 1860 45 persons gained citizenship, there are really many innkeepers, some merchants and some craftsmen. No skippers. 
filter(newdata, V8 == 1860)

# In the year of 1814, when DK lost Norway, really many Norwegians gained citizenship.
# Out of 45 new citizenships are 22 Norwegian.
filter(newdata, V8 == 1814)

# To find out multiple conditions about the Norwegian citizenships in 1814 I make a new command.
# As expected I got 22 choices, but I can see that 18 out of the 22 choices are skippers, 4 are merchants. 
filter(newdata, V8 == "1814", V17 == "Norge")

# I make a new smaller dataframe only with the informations from 41 by using a pipe: 
newdata1814norge <- newdata %>%
  filter(V8 == "1814", V17 == "Norge") %>%
  select(V17:V19)

# Now I look at the column V20 (hovederhverv1 = main occupation):
hovederhverv1 <- select(newdata, V20)
view(hovederhverv1)

# I sort out the twenty occupations that are most frequent and make a vector. I use again the code from the  URL
# I get the same list I already could see on OpenRefine. 
hovederhverv1nr <- sort(table(hovederhverv1),decreasing=TRUE)[1:20]
view(hovederhverv1nr)

# Now I want to see if there is a change in the number of main occupations in the three chosen years 1750, 1815 and 1860:

hovederhverv1750 <- newdata %>%
  filter(V8 == "1750") %>%
  select(V20)

hovederhverv1815 <- newdata %>%
  filter(V8 == "1815") %>%
  select(V20)

hovederhverv1860 <- newdata %>%
  filter(V8 == "1860") %>%
  select(V20)

# Then I make vectors with the main occupations in 1750, 1815 and 1860 and get the 20 most frequent occupations. 
#The three vectors give me a good overview:

hovederhverv1750nr <- sort(table(hovederhverv1750),decreasing=TRUE)[1:20]

hovederhverv1815nr <- sort(table(hovederhverv1815),decreasing=TRUE)[1:20]

hovederhverv1860nr <- sort(table(hovederhverv1860),decreasing=TRUE)[1:20]


# Instead of making the vectors (line 63 to 84) I can use ggplot which is faster.
# PLOTTING: First I want to make a barplot from the most frequent countries the new burghers came from: 

newdata_plotting <- newdata
newdata_plotting  %>%
  count(V16, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = V16, y = n)) +
  geom_col()

# Then I try to get simple barplots from the main occupations in 1750, 1815 and 1860:

newdata_plotting <- newdata
newdata_plotting  %>%
  filter(V8==1750) %>%
  count(V20, sort = TRUE) %>%
  ggplot(aes(x = V20, y = n)) +
  geom_col()


newdata_plotting <- newdata
newdata_plotting  %>%
  filter(V8==1815) %>%
  count(V20, sort = TRUE) %>%
  ggplot(aes(x = V20, y = n)) +
  geom_col()

newdata_plotting <- newdata
newdata_plotting  %>%
  filter(V8==1860) %>%
  count(V20, sort = TRUE) %>%
  ggplot(aes(x = V20, y = n)) +
  geom_col()



