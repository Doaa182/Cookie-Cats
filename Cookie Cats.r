# To read the dataset into a data frame, run this cell
#https://drive.google.com/file/d/1kSAaGB0jU2X_6Q4hoRf7klDTOSQMCd_a/view

system("gdown --id 1kSAaGB0jU2X_6Q4hoRf7klDTOSQMCd_a")
cookie_cats = read.csv("cookie_cats.csv")

# To learn more about the structure and variables of the dataset, use str() and head()

head(cookie_cats)
str(cookie_cats)

install.packages("naniar")
library(naniar)

# First, let's check if the data contains any missing values
is.na(cookie_cats)

# Let's check if there are any duplicates using duplicated()
duplicated(cookie_cats)


# Next, let's find the number of players who came back to play after 1 day or 7 days of installing using sum()
# Note: retention_1 and retention_7 are character variables not bool, find the appropriate function to convert them

cookie_cats$  retention_1 <- as.logical(cookie_cats$  retention_1)
message("sum of retention_1: ", sum(cookie_cats$  retention_1))

cookie_cats$  retention_7 <- as.logical(cookie_cats$  retention_7)
message("sum of retention_7: ",sum(cookie_cats$  retention_7))

# Let's now display retention_1 and retention_7 separately using a barplot
library(ggplot2)

ggplot(cookie_cats, aes(retention_1, fill = retention_7)) +
  geom_bar(position = "dodge")

# Finally, let's use summary() to find summaries regarding each variable
summary(cookie_cats)

# Counting the number of players in each A/B group.(gate30 / gate40)
library(dplyr)

cookie_cats %>% count(version)

# The distribution of game rounds
# Plotting the distribution of players that played 0 to 100 game rounds
library(ggplot2)

cookie_cats_plot <- cookie_cats %>%
filter(sum_gamerounds <= 100) 
bind_shadow(cookie_cats_plot) %>%

ggplot(aes(x = retention_1 , color = retention_7)) +
  geom_density()

# The % of users that came back the day(retention_1) after they installed
cookie_cats %>%
summarise(mean(retention_1))

# The % of users that came back a week(retention_7) after they installed
cookie_cats %>%
summarise(mean(retention_7))

#Calculating the percentage of 1-day retention for each A/B group
cookie_cats %>%
group_by(version) %>%
summarise(mean(retention_1 == "TRUE"))

#Calculating the percentage of 7-days retention for each A/B group
cookie_cats %>%
group_by(version)%>%
summarise(mean(retention_7 == "TRUE"))

# Creating an list with bootstrapped means(NULL distribution) for group ( retention_1)
install.packages("infer")
library(infer)

cookie_cats$  retention_1 <- as.character(cookie_cats$  retention_1)
NULL_distribution <- cookie_cats%>%

specify(retention_1 ~ version, success = "TRUE")%>%
hypothesize(null = "independence")%>%
generate(reps = 500, type = "permute")%>%
calculate(stat = "diff in props", order = c("gate_30", "gate_40"))

#calculate the observed statistics (actual statistics)
cookie_cats%>%
group_by(version)%>%
summarise(prop = mean(retention_1 == "TRUE"))%>%
summarise(diff(prop))

#plot the null distrbiution (the list with bootstrapped props)
#then plot a line to represent the observed differnce 
#Note: if you used the function shape_p_value(),set the value for the argument direction to "right"
library(ggplot2)

ggplot(NULL_distribution, aes(x = stat))+
  geom_histogram()+
  geom_vline(aes(xintercept = -0.00590517), color = "blue")

#calculate the p_value 
#Note: if you used the function get_p_value(),set the value for the argument direction to "right"
library(infer)

NULL_distribution%>%
get_p_value(obs_stat = -0.00590517, direction = "right")

# Creating an list with bootstrapped props(NULL distribution) for group(retention_7)
library(infer)

cookie_cats$  retention_7 <- as.character(cookie_cats$  retention_7)
NULL_distribution2 <- cookie_cats%>%

specify(retention_7 ~ version, success = "TRUE")%>%
hypothesize(null = "independence")%>%
generate(reps = 500, type = "permute")%>%
calculate(stat = "diff in props", order = c("gate_30", "gate_40"))

#calculate the observed statistics (actual statistics)
cookie_cats%>%
group_by(version)%>%
summarise(prop2 = mean(retention_7 == "TRUE"))%>%
summarise(diff(prop2))

#plot the null distrbiution (the list with bootstrapped props)
#then plot a line to represent the observed differnce 
#Note: if you used the function shape_p_value(),set the value for the argument direction to "right"
library(ggplot2)

ggplot(NULL_distribution2, aes(x = stat))+
  geom_histogram()+
  geom_vline(aes(xintercept = -0.008201298), color = "blue")

#calculating the p_value for retention_7
#Note: if you used the function get_p_value(),set the value for the argument direction to "right"
library(infer)

NULL_distribution2%>%
get_p_value(obs_stat = -0.008201298, direction = "right")