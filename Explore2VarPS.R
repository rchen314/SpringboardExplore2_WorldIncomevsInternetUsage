library(ggplot2)

# Create a scatterplot of price vs X using ggplot
ggplot(aes(x = x, y = price), data = diamonds) + geom_point()

# Find the correlation between price and x/y/z
cor.test(diamonds$x, diamonds$price)    
cor.test(diamonds$y, diamonds$price)
cor.test(diamonds$z, diamonds$price)

# Create a simple scatter plot of price vs depth
ggplot(aes(x = depth, y = price), data = diamonds) + geom_point()

# Make transparency 1/100 and mark x-axis every 2 units
range(diamonds$depth)
ggplot(aes(x = depth, y = price), data = diamonds) + 
  geom_point(alpha = .01) +
  scale_x_continuous(lim = c(42, 80), breaks = seq(42, 80, 2)) 

# Find correlation between depth and price
cor.test(diamonds$depth, diamonds$price)

# Make a scatter plot of price vs carat and omit the top 1% of price and 
# carat values
quantile(diamonds$price,.99)
quantile(diamonds$carat,.99)
ggplot(aes(x = carat, y = price), 
       data = subset(diamonds, carat < 2.18, price < 17378.22)) +
  geom_point()

# Create a new variable for volume (x*y*z) and make a scatter plot of 
# price vs volume 
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
ggplot(aes(x = volume, y = price), data = diamonds) + geom_point()

# Find the correlation between price and volume, excluding anything where
# volume = 0 or volume > 800
d2 <- subset(diamonds, volume > 0)
d3 <- subset(d2,       volume < 800)
cor.test(d3$price, d3$volume)

# Adjust the transparency and add a linear model to the plot
ggplot(aes(x = volume, y = price), data = d3) +
  geom_point(alpha = .05) +
  geom_smooth()

# Use dplyr to create a new data frame diamondsByClarity w/ mean_price,
# median_price, min_price, max_price, n
library(dplyr)
diamondsByClarity <- diamonds %>% 
  group_by(clarity) %>%
  summarise(mean_price   = mean(price),
            median_price = median(price),
            min_price    = min(price),
            max_price    = max(price),
            n            = n())
head(diamondsByClarity)

# Create various summary data frames and create 2 bar plots
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))
diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))
d1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) + 
  geom_bar(stat="identity")
d2 <- ggplot(aes(x = color,   y = mean_price), data = diamonds_mp_by_color)   + 
  geom_bar(stat="identity") 
library(gridExtra)
grid.arrange(d1, d2, ncol = 1) 


#######################################################
# Look at income and internet use from Gapminder site #
#######################################################

# Read in files
internet <- read.csv("Internet user per 100.csv", header = TRUE)
income   <- read.csv("indicator gapminder gdp_per_capita_ppp.csv", header = TRUE)

# Change name of column 1
colnames(internet)[1] = "Country"
colnames(income)[1]   = "Country"

# Select relevant fields -- Use 2011 because that is the most 
# recent value for "Internet"
internet2011 <- internet %>% select(Country, X2011)
income2011   <- income   %>% select(Country, X2011)

# Remove any "NAs" 
internet2011 <- internet2011 %>% filter(!is.na(X2011))
income2011   <- income2011   %>% filter(!is.na(X2011))

# Remove 2011 column to prevent conflict
colnames(internet2011)[2] = "InternetUse"
colnames(income2011)[2]   = "Income"

# Join the data
all2011 <- inner_join(internet2011, income2011)

# Make a scatterplot
ggplot(aes(x = Income, y = InternetUse), data = all2011) + geom_point()

# Check for overplotting by adding alpha
ggplot(aes(x = Income, y = InternetUse), data = all2011) + 
  geom_point(alpha = 0.25)

# Add correlation line
ggplot(aes(x = Income, y = InternetUse), data = all2011) + 
  geom_point() +
  geom_smooth()

# Find correlation
cor.test(all2011$Income, all2011$InternetUse)

# List those above 85% to find unusually high countries
all2011 %>% filter(InternetUse > 85) %>% arrange(desc(InternetUse))

# Zoom in on 50-60%
  ggplot(aes(x = Income, y = InternetUse), data = all2011) + 
  geom_point() +
  ylim(50, 60)

# Find the 5 countries that are unusually high in Internet use 
# in the 56 - 60% range
all2011 %>% filter(InternetUse > 56, InternetUse < 60 ) %>% arrange(desc(InternetUse))

# Find the countries where usage is lower than expected
all2011 %>% filter(Income > 50000, InternetUse < 75 ) %>% arrange(desc(InternetUse))


   