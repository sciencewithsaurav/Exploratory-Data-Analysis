library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)
p <- ggplot(murders)

head(heights)

class(p)

# define ggplot object called p like in the previous exercise but using a pipe 
p <- heights %>% ggplot()

p <- murders %>% ggplot(aes(population, total, label=abb, color = region)) +
  geom_label()
p + scale_x_log10() + scale_y_log10() + ggtitle("Gun murder data")

p <- heights %>% ggplot(aes(height, fill = sex))
p + geom_density(alpha=0.2)

install.packages("tidyverse")
library(tidyverse)

s <- heights %>% 
  filter(sex == "Male") %>% 
  summarize(average=mean(height), standard_deviation = sd(height))
s

murders <- murders %>% mutate(murder_rate = total/population*100000)
head(murders)
summarise(murders, mean(murder_rate))

us_murder_rate <- murders %>% summarise(rate = sum(total)/sum(population)*100000)
us_murder_rate %>%  .$rate


#############################################

library(dplyr)
library(ggplot2)
library(dslabs)

data("gapminder")

gapminder %>% group_by(continent) %>% summarise(avg_mort=mean(infant_mortality, na.rm=TRUE))

ds_theme_set()
filter(gapminder, year %in% c(1962,2012)) %>% ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() + 
  facet_grid(.~year)

filter(gapminder, year %in% c(1962,1970, 1980, 1990, 2000, 2012)) %>% 
  ggplot(aes(fertility, life_expectancy, color=continent)) +
  geom_point() + 
  facet_wrap(~year)

filter(gapminder, country %in% c("Germany","India")) %>% 
  ggplot(aes(year, fertility, color=country)) +
  geom_line()

countries <- c("South Korea", "Germany")

# life expectancy time series - lines colored by country and labeled, no legend
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

#####transformers
gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)
head(gapminder)

past_year <- 1970
gapminder %>% filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1,color="black")

gapminder %>% filter(year == past_year & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1,color="black") +
  scale_x_continuous(trans = "log2")

levels(gapminder$region)

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2") + geom_point(show.legend = FALSE)

# define Western countries
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

present_year <- 2010

# facet by West vs devloping
gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)


# define countries that have data available in both years
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)
# make histogram including only countries with data available in both years
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%    # keep only selected countries
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)


p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")
p + geom_boxplot(aes(region, dollars_per_day, fill = continent)) +
  facet_grid(year ~ .)


# arrange matching boxplots next to each other, colored by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))



# smooth density plots - area under each curve adds to 1
gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()


# smooth density plots - variable counts on y-axis
p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)


# add group as a factor, grouping regions
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))

# reorder factor levels
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

# note you must redefine p with the new gapminder object first
p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")

# stacked density plot
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

# weighted stacked density plot
gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)


# add additional cases
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))


# define a data frame with group average income and average infant survival rate
surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

# plot infant survival versus income, with transformed axes
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 

####################################

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
str(gapminder)


z_theme <- function() {
  theme_bw(base_size=9) +
    #Background and Grid formatting
    theme(panel.background=element_rect(fill="#000000", color="#000000")) +
    theme(plot.background=element_rect(fill="#000000", color="#000000")) +
    theme(panel.border=element_rect(color="#252525")) +
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    #Legend formatting
    theme(legend.background = element_rect(fill="#000000")) +
    theme(legend.text = element_blank()) +
    theme(legend.title= element_blank())+
    theme(legend.position="none")+
    #Axis & Title Formatting
    theme(plot.title=element_text(color="#D9D9D9", size=20, vjust=1.25)) +
    theme(plot.subtitle=element_text(size=12,color="#BDBDBD", vjust=0)) +
    theme(plot.caption=element_text(size=12,color="#BDBDBD", vjust=0)) +
    theme(axis.ticks=element_blank()) +
    theme(axis.text.x=element_text(size=14,color="#BDBDBD")) +
    theme(axis.text.y=element_text(size=14,color="#BDBDBD")) +
    theme(axis.title.x=element_text(size=16,color="#BDBDBD", vjust=0)) +
    theme(axis.title.y=element_text(size=16,color="#BDBDBD", vjust=1.25))
}


#Exercise 2
gapminder %>% filter(year == 2012 & continent == "Africa" ) %>% 
  ggplot(aes(fertility, life_expectancy)) + 
  geom_point(shape=".", aes(color=region)) +
  geom_smooth(method=lm,formula=y~x+0,linetype=4,size=.5,se=F) +
  labs(title="AFrica 2012",
       subtitle="Fertility vs Life Expectency",
       x="Life expectency",
       y="Fertility",
       caption="created by Saurav Mitra")+
  z_theme()









head(gapminder)

#Exercise 3
df <- gapminder %>%   filter(year == 2012 & continent == "Africa" & fertility<=3 & life_expectancy>=70) %>% select( country, region)

#Exercise 4
tab <- gapminder %>% filter(year >= 1960 & year <= 2010 & country %in% c("Vietnam","United States"))

#Exercise 5
p <- tab %>% ggplot(aes(year, life_expectancy, color = country)) + geom_line()
p

#Exercise 6
gapminder %>% filter(year >= 1960 & year <= 2010 & country == "Cambodia") %>% 
  ggplot(aes(year, life_expectancy)) + geom_line()


#Exercise 9
daydollars %>% filter(year %in% c(1970, 2010)) %>% ggplot(aes(dollars_per_day)) + 
  geom_density() +
  scale_x_continuous(trans = "log2") + facet_grid(year~.)


daydollars <- gapminder %>% mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(dollars_per_day))

daydollars %>% ggplot(aes(dollars_per_day, fill=region)) + 
  geom_density(bw=0.5, position = "stack") +
  scale_x_continuous(trans = "log2") + facet_grid(year~.)

#Exercise 11
library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

gapminder_Africa_2010 <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year == 2010 & !is.na(dollars_per_day))

gapminder_Africa_2010 %>% ggplot(aes(dollars_per_day,infant_mortality, color=region)) +
  geom_point()

gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day,infant_mortality, color=region)) +
  geom_point() +
  scale_x_continuous(trans = "log2")

gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day,infant_mortality, color=region,label = country)) + geom_point() + scale_x_continuous(trans='log2') +
  geom_text()

#Exercise 15

gapminder_Africa_2010 <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>%
  filter(continent == "Africa" & year %in% c(1970, 2010) & !is.na(year) & !is.na(infant_mortality) & !is.na(dollars_per_day))

gapminder_Africa_2010

gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day,infant_mortality, color=region,label = country)) + geom_point() + scale_x_continuous(trans='log2') +
  geom_text() + facet_grid(year~.)



library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)

dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()


library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
murders %>% mutate(rate = total/population*100000) %>% 
  mutate(region = reorder(region, rate, FUN=median)) %>%
  ggplot(aes(region, rate)) + geom_boxplot() + geom_point()


# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

library(RColorBrewer)
display.brewer.all(type="div")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "red") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")




############################################
#######PROBABILITY##################
###################################

library(tidyverse)

beads <-  rep(c("red", "blue"), times= c(2, 3))
beads

sample(beads, 1)

event <- replicate(10000, sample(beads,1))

tab <- table(event)
prop.table(tab)


events <- sample(beads, 10000, replace = TRUE)
prop.table(table(events))
?permutations
library(gtools)
all_phone_numbers <- permutations(10,7, v=0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 12)
all_phone_numbers[index,]

library(gtools)

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck
deck <- paste(deck$number, deck$suit)
deck
?permutations
hands <- permutations(52,2, v = deck)
hands

hands[,1]





#Code: Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)    # open door with no prize that isn't chosen
  stick <- my_pick    # stick with original door
  stick == prize_door    # test whether the original door has the prize
})

mean(stick)    # probability of choosing prize door when sticking


#Code: Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))    # puts prizes in random order
  prize_door <- doors[prize == "car"]    # note which door has prize
  my_pick  <- sample(doors, 1)    # note which door is chosen first
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)    # open door with no prize that isn't chosen
  switch <- doors[!doors%in%c(my_pick, show)]    # switch to the door that wasn't chosen first or opened
  switch == prize_door    # test whether the switched door has the prize
})
mean(switch)    # probability of choosing prize door when switching



?sample
?sapply


######################################################

library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits

#3 Medals | 8 runners | 3 from Jamaica | 

#How many different ways can the 3 medals be distributed across 8 runners?
permutations(8, 3)

#How many different ways can the three medals be distributed among the 
#3 runners from Jamaica?
permutations(3,3)

#What is the probability that all 3 medals are won by Jamaica?
3/8*2/7*1/6

#Run a Monte Carlo simulation on this vector representing the countries 
#of the 8 runners in this race:
#
#runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
#
#For each iteration of the Monte Carlo simulation, within a replicate loop, 
#select 3 runners representing the 3 medalists and check whether they are 
#all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 
#before running the loop.
#
#Calculate the probability that all the runners are from Jamaica.
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", 
             "Ecuador", "Netherlands", "France", "South Africa")

set.seed(1)
#?sample
results <- replicate(10000, {
  runner <- sample(8, 3)
  
  all(runner[1] ==1, runner[2] == 2, runner[3] == 3)
})

mean(results)

#Question 2: Restaurant management
#1 entree, 2 sides, and 1 drink. 
#He currently offers a choice of 1 entree from a list of 6 options, 
#a choice of 2 sides from a list of 6 options, 
#a choice of 1 drink from a list of 2 options.
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(2,1))

nrow(combinations(6,1))*nrow(combinations(6,3))*nrow(combinations(3,1))

e <- 9

testfunc <- function(e){
  nrow(combinations(e,1))*nrow(combinations(6,2))*nrow(combinations(3,1))
}

sapply(seq(1,12), testfunc)
testfunc(9)


s <- 3

testfunc1 <- function(s){
  nrow(combinations(6,1))*nrow(combinations(s,2))*nrow(combinations(3,1))
}

sapply(2:12, testfunc1)
testfunc1(6)


library(tidyverse)
head(esoph)

high_alcgp <- esoph %>% filter(alcgp == max(esoph$alcgp))
head(high_alcgp)
(sum(high_alcgp$ncases))/(sum(esoph$ncases) + sum(esoph$ncontrols))
(sum(high_alcgp$ncontrols))/(sum(esoph$ncontrols) + sum(esoph$ncases))
.038/.057



high_tobgp <- esoph %>% filter(tobgp == max(esoph$tobgp))
head(high_tobgp)
(sum(high_tobgp$ncontrols))/(sum(esoph$ncontrols))

high_tob_alcgp <- esoph %>% filter(tobgp == max(esoph$tobgp) 
                                   | alcgp == max(esoph$alcgp))
head(high_tob_alcgp)
(sum(high_tob_alcgp$ncontrols))/(sum(esoph$ncontrols))
(sum(high_tob_alcgp$ncases))/(sum(esoph$ncases))

.33/.13



library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
x
F <- function(a) mean(x <= a)

1 - F(70)    # probability of male taller than 70 inches


########################################################

avg <- 20.9
sd <- 5.7

set.seed(16, sample.kind = "Rounding")

act_scores <- rnorm(10000, avg, sd)
head(act_scores)
hist(act_scores)

avg <- mean(act_scores)
sd <- sd(act_scores)

sum(act_scores<=10)/10000

x <- 1:36

f_x <- function(x){
  dnorm(avg, sd)  
}

length(f_x)
plot(dnorm, from = 1,to=36)

#####################################

head(act_scores)
avg
sd

z_scores <- (act_scores-avg)/sd
head(z_scores)

sum(z_scores > 2)/length(z_scores)

head(z_scores)

z_frame <- data.frame(z_scores)

head(z_frame)

a <- z_frame %>% filter(z_frame$z_scores >=2)

sort(a$z_scores)

2.000335*sd+ avg

?qnorm
qnorm(.975)


p <- seq(0.01, 0.99, 0.01)
head(p)

sample_quantiles <- qnorm(p, avg, sd)

#4d.
theoretical_quantiles <- qnorm(p, 20.9, 5.7)

qqplot(theoretical_quantiles, sample_quantiles)

###############################


# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)    # 1000 draws from urn, -1 if red, else +1
X[1:10]    # first 10 outcomes

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S


n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 roulette spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

library(tidyverse)

s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")



### Standard error
abs((17 - -1))*sqrt(p_green*p_not_green) / sqrt(n)


######

options(digits = 3)

#Incorrect    -0.25
#Correct      +1.0
#44 ques   5 ans each

#prob of guessing a question
prob_correct <- 0.25
prob_incorrect <- 0.75

44*0.25*1 + 0.75*0

abs((1 - -.25))*sqrt(prob_correct*prob_incorrect) / sqrt(44)

set.seed(21, sample.kind = "Rounding")
S<- replicate(10000, {
  X <- sample(c(1, -.25), 44, replace = TRUE, prob = c(prob_correct, prob_incorrect))
  sum(X)
})

mean(S>=8)

p <- seq(0.25, 0.95, 0.05)
head(p)
avg <- 44 * (1*p + 0*(1-p))
se <- sqrt(44) * (1 - 0)*sqrt(p*(1-p))

data.frame(1-pnorm(35, avg, se))
p[13]


#Q3a
p_win <- 5/38
p_win
p_loss <- (1-p_win)
p_loss
win <- 6
loss <- -1
n <- 500

#3a
avg <- (win*p_win + loss*p_loss)/500
avg

#3b
se <- sqrt(1) * (win - loss)*sqrt(p_win*(1-p_win))
se

#3c
(win*p_win + loss*p_loss)

S<- replicate(10000, {
  X <- sample(c(win, loss), n, replace = TRUE, prob = c(p_win, p_loss))
  mean(X)
})
S
mean(S)

#3d
sd(S)

#3e
avg <- 500*(win*p_win + loss*p_loss)

#3f
se <- sqrt(500) * (win - loss)*sqrt(p_win*(1-p_win))

#3g
pnorm(0, avg, se)



##############

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})
sum(losses)
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")


n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error


l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x    # required profit when loan is not a foreclosure
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money


############################

#The big short assesment
options(digits = 3)
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)


hist(death_prob$prob)

death_prob %>% ggplot(aes(age, prob)) + geom_line()




############################
############################
############################


#####Modelling


library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)

#Spread = 2p - 1

#Standard error of each sample average
#se <- sqrt(p*(1-p)/N)

p <- 0.8
N <- 100
B <- 10000

X_hat <- replicate(B, {
  X <- sample(c(0,1), N, replace = TRUE, prob=c(p,1-p))
  mean(X)
})

mean(X_hat)
sd(X_hat)

library(tidyverse)
library(gridExtra)

data.frame(X_hat = X_hat) %>% 
  ggplot(aes(X_hat)) +
  geom_histogram(binwidth = 0.005, color="red")


N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
se
data <- data.frame(N_size=N, se_list=se)
data
plot(data)
data 

qqnorm(se)
qqline(se)

?pnorm


qnorm(0.95)
pnorm(1.96)


(2*X_hat - 1) + c(-2, 2)*2*sqrt(X_hat*(1-X_hat)/N)
###########################

library(dplyr)
library(dslabs)
data("polls_us_election_2016")
str(polls_us_election_2016)

polls <- polls_us_election_2016 %>% 
  filter(enddate >= as.Date("2016-10-31") & state == "U.S.")

nrow(polls)

N <- polls$samplesize[1]
print(N)
X_hat <- polls$rawpoll_clinton[1]/100
print(X_hat)

se_hat <- sqrt(X_hat*(1-X_hat)/N)
print(se_hat)

ci <- X_hat + c(-1,1)*qnorm(0.975)*se_hat
ci

head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results <- polls %>% mutate(X_hat = polls$rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize),
                                     lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) %>%
  select(pollster, enddate, X_hat, se_hat, lower, upper)



# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- polls %>% mutate(X_hat = polls$rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize),
                            lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat, 
                            hit = lower<=0.482 & upper>=0.482) %>%
  select(pollster, enddate, X_hat, lower, upper, hit) %>%
  summarize(mean(hit))



