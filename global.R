library(dplyr)
library(tidyr)
library(haven)
library(foreign)
library(ggplot2)
library(stringr)
library(ggtext)
library(glue)
library(countrycode)
library(directlabels)
library(shinydashboard)


source('utils.R')

# Each of the following is a list of dataframes, indexed by survey
surveys.data <- utils.surveys_data(force=F)
surveys <- names(surveys.data)

# Removed from UI for the moment
questions.n <- 4
metric <- "area"

# data.labels <- utils.labels(data.raw)
# data.questions <- utils.questions(data.raw)
# data.answers.levels <- utils.answer_levels(data.raw, data.questions)
# data.long <- utils.pivot_longer(data.raw, data.questions)
# data.regions <- utils.regions(data.raw)


metrics <- list("Area between curves"="area",
                "Entropy-like"="entropy")

# Keywords to highlight in titles
keywords <- c("economy", "better off", "worse off", "democracy", "religion", "The United States",
              "China", "Russia", "The European Union", "The United Nations",
              "Germany","France","United Kingdom", "Brexit", "1989 / 1991",
              "culture", "rich", "poor", "well-paying jobs", "education system", "political system",
              "military", "believe in God", "Homosexuality", "diverse", "prayer", "god",
              "economic situation","neighboring countries", "traveled", "women", "Vladimir Putin",
              "Angela Merkel", "Muslims", "internet", "jobs", "Kim Jong-Un", "Donald Trump",
              "North Korean", "benefit of all the people", "state",
              "freedom of speech", "immigrants", "immigration", "trade", "wages",
              "politicians are corrupt", "major global problems", "U\\.S\\.", "scenarios","crime")
rm("data.raw")
