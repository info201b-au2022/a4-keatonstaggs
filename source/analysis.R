library(tidyverse)
library(ggplot2)
library(usmap)


## Load data frame ---- 
a4_data <- read.csv("~/Documents/info201/assignments/a4-keatonstaggs/source/incarceration_trends.csv")
# View(a4_data)

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Creating a function to analyze white and black population in 2018
pop_sum <- function() {
  first_variable <- a4_data %>%
    filter(str_detect(year, "2018")) %>%
    select(black_pop_15to64, white_pop_15to64) 
  
  
  black_pop_sum <- first_variable %>%
    select(black_pop_15to64) %>%
    colSums(na.rm=TRUE)
  
  
  white_pop_sum <- first_variable %>%
    select(white_pop_15to64) %>%
    colSums(na.rm=TRUE)
  
  # Calculating the difference between both variables
  both_sums <- abs(white_pop_sum - black_pop_sum)
  return(both_sums)
}

# Creating a function to find the state and year in which the max black population occurred.
black_pop_high <- function() {
  black_pop_highest <- a4_data %>%
    filter(black_pop_15to64 == max(black_pop_15to64, na.rm=TRUE)) %>%
    select(state, year)
  return(black_pop_highest)
}
black_pop_high()

# Creating a function to find the state and year in which the max white population occurred.
white_pop_high <- function() {
  white_pop_highest <- a4_data %>%
    filter(white_pop_15to64 == max(white_pop_15to64, na.rm=TRUE)) %>%
    select(state, year)
  return(white_pop_highest)
}
white_pop_high()

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

# Creating a function for total jail population arranged by the year
get_year_jail_pop <- function() {
  filtered_df <- a4_data %>%
    select(year, total_jail_pop) %>%
    arrange(year) 
 # aggregate(filtered_df$total_pop, list(filtered_df$year), FUN=sum)
  return(filtered_df)   
}
get_year_jail_pop()

# Creating a bar chart by passing in the previous function and adding necessary labels
plot_jail_pop_for_us <- function()  {
  bar_chart <- ggplot(get_year_jail_pop(), aes(x= year, y = total_jail_pop)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = scales::comma) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") +
    labs(x = "Year", 
         y = "Total Jail Population",
         caption = "Figure 1. Increase of Jail Population in U.S. (1970-2018).
         This chart shows that the total jail population has been steadily
         increasing from 1970 to 2008, then fluctuates from decreasing to increasing up to 2018."
         )
  return(bar_chart)   
} 
plot_jail_pop_for_us()
#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# Creating a function that selects specific states to analyze for total jail population
get_jail_pop_by_states <- function(states) {
  states <- c("WA", "AZ", "MD", "AL")
  state_filter <- a4_data %>%
    filter(state %in% states) %>%
    select(year, state, total_jail_pop)
  aggregate_state <- aggregate(state_filter$total_jail_pop, list(state_filter$state, state_filter$year), FUN=sum)
  
  return(aggregate_state)
}
get_jail_pop_by_states(states)

# Creating a line chart to display each state and the population with labels
plot_jail_pop_by_states <- function(states) {
  line_chart <- ggplot(get_jail_pop_by_states(states), aes(x = Group.2, y = x, color = Group.1)) +
    geom_line(size = 0.5) +
    ggtitle("Growth of Prison Population by State") +
    labs(x = "Year", 
         y = "Total Jail Population",
         color = "State",
         caption = "Figure 2. Growth of Prison Population by State.
         This chart shows a comparison between Arizona, Maryland, Alabama, and Washington.
         Each state is represented by a colored line that displays the jail population each year."
    )
    
   
  
  return(line_chart)
}
plot_jail_pop_by_states(states)
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Growth of Black vs. White Prison Population by Year

# Function that filters black population data according to year
plot_wrangle <- function(){
  filter <- a4_data %>%
    filter(year >= 1990) %>%
    select(year, black_pop_15to64) 
  aggregation <- aggregate(filter$black_pop_15to64, list(filter$year), FUN=sum)
    
  return(aggregation)
}

# Plotting a scatter plot of the wrangled data from the previous function
plot_scatter <- function() {
  scatterplot <- ggplot(plot_wrangle(), aes(x = Group.1, y = x)) +
    geom_point() +
    ggtitle("Growth of Black Prison Population by Year") +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Year", 
         y = "Black Prison Population",
         caption = "Figure 3. Growth of Black Prison Population by Year.
         This chart displays the growth of the population of imprisoned black people.
         The x-axis represents increasing year and the y-axis represents population."
    )
  
  return(scatterplot)
}
plot_scatter()

# Creating a function that filters white population by year
plot_wrangle_white <- function(){
  filter_white <- a4_data %>%
    filter(year >= 1990) %>%
    select(year, white_pop_15to64) 
  aggregation_white <- aggregate(filter_white$white_pop_15to64, list(filter_white$year), FUN=sum)
  
  return(aggregation_white)
}

# Plotting a scatter plot of the white population data
plot_scatter_white <- function() {
  scatterplot_white <- ggplot(plot_wrangle_white(), aes(x = Group.1, y = x)) +
    geom_point() +
    ggtitle("Growth of White Prison Population by Year") +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Year", 
         y = "White Prison Population",
         caption = "Figure 4. Growth of White Prison Population by Year.
         This chart displays the growth and decline of the population of imprisoned white people.
         The x-axis represents increasing year and the y-axis represents population."
    )
  
  return(scatterplot_white)
}
plot_scatter_white()
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# Black Prison Population by State in 2018

# Creating a filtering function to group black population data by state
get_map_filters <- function() {
  black_pop_filter <- a4_data %>%
    filter(str_detect(year, "2018")) %>%
    select(state, black_pop_15to64) %>%
    group_by(state) %>%
    summarize(state_total = sum(black_pop_15to64))
  return(black_pop_filter)
}

# Creating a mapping function to display the black prison population for each state
plot_map <- function() {
  map <- plot_usmap(
    regions = "states",
    data = get_map_filters(),
    values = "state_total") +
    scale_fill_continuous(
      label = scales::comma, 
      name = "Black Prison Population") +
    theme(legend.position = "right") +
    ggtitle("Black Prison Population by State in 2018") +
    labs(caption = "Figure 5. Black Prison Population by State in 2018.
         This United States map displays a comparison of the black prison population for each state in 2018.
         Lighter tones of blue indicate a higher population.")
    
  return(map)
}
plot_map()
#----------------------------------------------------------------------------#


