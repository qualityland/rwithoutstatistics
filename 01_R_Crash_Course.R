# base R
population_data <- read.csv("data/population-by-state.csv")

head(population_data)
tail(population_data)

str(population_data)


# tidyverse
library(tidyverse)

population_data_2 <- read_csv("https://data.rwithoutstatistics.com/population-by-state.csv")

# mean population of a state
summarize(.data = population_data_2,
          mean_population = mean(Pop))
# as pipe
population_data_2 |> 
  summarize(mean_population = mean(Pop))

# pipe to filter
population_data_2 |> 
  filter(State == "Hawaii")

# Calculate the mean population of the five largest states
population_data_2 |> 
  filter(rank <= 5) |> 
  summarize(top5_mean_population = mean(Pop))
