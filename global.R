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
library(shinyWidgets)


source('utils.R')

# Each of the following is a list of dataframes, indexed by survey
surveys.data <- utils.surveys_data(force=F)
surveys <- names(surveys.data)

# Removed from UI for the moment
metric <- "area"
questions.n <- 6

# data.labels <- utils.labels(data.raw)
# data.questions <- utils.questions(data.raw)
# data.answers.levels <- utils.answer_levels(data.raw, data.questions)
# data.long <- utils.pivot_longer(data.raw, data.questions)
# data.regions <- utils.regions(data.raw)


metrics <- list("Neutral"="area",
                "Sensitive to rare answers"="entropy")

# Keywords to highlight in titles
keywords <- c("economy", "better off", "worse off", "democracy", "religion", "United States",
              "China", "Russia", "European Union", "United Nations", "North Atlantic Treaty Organization",
              "Germany","France","United Kingdom", "Brexit", "1989 / 1991", "Roma", "foreign companies",
              "culture", "rich", "poor", "well-paying jobs", "education system", "political system",
              "military", "believe in God", "Homosexuality", "diverse", "prayer", "god",
              "economic situation","neighboring countries", "traveled", "women", "Vladimir Putin",
              "Angela Merkel", "Muslims", "internet", "jobs", "Kim Jong-Un", "Donald Trump",
              "North Korean", "benefit of all the people", "state", "government", "Emmanuel Macron",
              "extreme left", "extreme right", "elected officials",
              "freedom of speech", "immigrants", "immigration", "trade", "wages",
              "politicians are corrupt", "major global problems", "U\\.S\\.", "scenarios","crime")

