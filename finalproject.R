library(data.table)
library(ggplot2)
library(stringr)
library(tidyverse)
library(rvest)
library(readxl)
library(httr)
library(rsconnect)
library(dplyr)
library(leaflet)
library(geojsonio)
library(readxl)
library(chron)


attendance.url <-"https://www.baseball-reference.com/leagues/MLB/misc.shtml"

attendance.table <- attendance.url %>%
  read_html()%>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table

ratings.table <- read_xlsx("/Users/dangause/Downloads/baseball.viewership.xlsx")
homeruns.table <- read_xlsx("/Users/dangause/Downloads/homerun.stats.xlsx")

attendance.table1 <- attendance.table
attendance.table1 <- attendance.table1[-c(1,26,52,78,104,130),]
attendance.table1$Attendance <- as.numeric(gsub(",", "", attendance.table1$Attendance))
attendance.table1$`Attend/G` <- as.numeric(gsub(",", "", attendance.table1$`Attend/G`))
attendance.table1$Year <- as.numeric(gsub(",", "", attendance.table1$Year))
attendance.table1$`R/G` <- as.numeric(gsub(",", "", attendance.table1$`R/G`))
attendance.table1$G <- as.numeric(attendance.table1$G)

table3<- attendance.table1 %>%
  filter(is.null(Time) == FALSE)%>%
  mutate(Time2 = as.numeric(substr(Time, 1,1))*60 + as.numeric(substr(Time,3,4)))
attendance.table1 <- table3

#avg. time of game per year
attendance.table1 %>%
  ggplot(aes(x = as.numeric(Year),
             y = Time2)) +
  geom_point()

population.url <- "https://en.wikipedia.org/wiki/Demography_of_the_United_States"
population.data <- population.url %>%
  read_html() %>%
  html_nodes("table") %>%
  .[[19]] %>%
  html_table
population.table <- population.data[,1:2]
colnames(population.table) <- c("Year", "Population")
population.table$Population <- as.numeric(gsub(",", "", population.table$Population))

attendance.table1 <-  left_join(attendance.table1, population.table)

attendance.table1 %>%
  ggplot(aes(x = Year,
             y = Attendance)) +
  geom_line()
attendance.table1 %>%
  ggplot(aes(x = Year,
             y = `Attend/G`)) +
  geom_point()
attendance.table1 %>%
  ggplot(aes(x = Year,
             y = `R/G`)) +
  geom_point()
           







# MLB game annual attendance
attendance.model <- lm(Attendance ~ Year,
                       data = attendance.table1)
attendance.AIC <- AIC(attendance.model)


attendance.table1 %>%
  ggplot(aes(y= Attendance,
             x = Year)) +
  geom_point() +
  geom_abline(slope = attendance.model$coefficients[2],
              intercept = attendance.model$coefficients[1],
              color = "red",
              size = 1) +
  labs(title = "MLB Game Annual Attendance")




# Attendance weighted by increasing population: found that attendance still increases linearly
attendance.table1 %>%
  ggplot(aes(x = Year,
             y = Attendance/Population)) +
  geom_point()

weighted.attendance.model <- lm(Attendance/Population ~ Year,
                                data = attendance.table1)

attendance.table1 %>%
  ggplot(aes(y= Attendance/Population,
             x = Year)) +
  geom_point() +
  geom_abline(slope = weighted.attendance.model$coefficients[2],
              intercept = weighted.attendance.model$coefficients[1],
              color = "red",
              size = 1) +
  ylab("Weighted Attendance") +
  ggtitle("Annual MLB Game Attendance weighted by Population")



weighted.attendance.model <- lm(Attendance/Population ~ Year,
                                data = attendance.table1)

weighted.attendance.AIC <- AIC(weighted.attendance.model)
# Here we see a 



# Homeruns per game
attendance.table1$G
hr.table <- NULL
hr.table <- data.frame(Year = c(1920:2018),
                       runsPerG = homeruns.table[20:118,3]/as.numeric(attendance.table1[1:99,3]))

p <- hr.table %>%
  ggplot(aes(x = Year,
             y = AL.NL)) + 
  geom_point() +
  ylab("Homeruns Per Game") +
  ggtitle("Homeruns per MLB Game") +
  geom_point(data=hr.table[68, ], aes(x=Year, y=AL.NL), colour="red", size=3)

set.seed(20)
q <- seq(from=1920, to=2018, by=1)
y <- 0.4 * (q-10)^2
function1 <- function(x) ((x-1919)/45)^2 + .3

p + stat_function(fun = function1)

# We see a nicely quadratic increase from ~.5 homeruns per game in the 1920s to ~5 in 2018.
# This increase in homeruns per game strengthens the excitement factor of absolute dingers.
# This homemade "best-fit" line has no real statistical weight, but is included to show an
# approximate quadratic trend to the data.


ratings.table$`Year / Series` <- as.numeric(ratings.table$`Year / Series`)
ratings.table$Viewers <- as.numeric(ratings.table$Viewers)
WSViewership.model <- lm(Viewers ~ `Year / Series`,
                                data = ratings.table)
ratings.table %>%
  ggplot(aes(x = `Year / Series`,
             y = Viewers)) +
  geom_point() + 
  geom_abline(slope = WSViewership.model$coefficients[2],
              intercept = WSViewership.model$coefficients[1],
              color = "red",
              size = 1) + 
  labs(title = "World Series Television Viewership",
       x = "Year",
       y = "Viewers") 


ratings.table %>%
  ggplot(aes(x = `Year / Series`,
             y = Rating)) +
  geom_point()












