# install packages
install.packages("plotly")

# declare libraries
library(abd)
library(Hmisc)
library(lattice)
library(mosaic)
library(dplyr)
library(psych) 
library(plotly)
library(ggplot2)
library(magrittr)


setwd('C:/Users/dylan/OneDrive/School/GT/CourseMaterials/fall2023/ECON 4803/Problem_Set_1')
getwd()

#### Problem 1 ####

# read turbine data into a data frame
turbine_df <- read.csv('uswtdb_v6_0_20230531.csv')
cols <- c('p_year', 'p_cap', 't_manu', 't_model', 't_cap', 't_hh', 't_rd', 'xlong', 'ylat')

# remove observations with missing important values
turbine_df <- turbine_df[rowSums(is.na(select(turbine_df, cols))) == 0, ]

# remove projects that started before 2001
turbine_df <- turbine_df[turbine_df$p_year >= 2001, ]

# remove observations from Alaksa and Hawaii
turbine_df <- turbine_df[!(turbine_df$t_state %in% c('AK', 'HI')), ]

# create summary table
n <- nrow(turbine_df)
vars <- c('t_cap', 't_hh', 't_rd')

# establish column names
sum_table <- data.frame(var = character(), mean = numeric(), sd = numeric(), min = numeric(), max = numeric())

# get stats for each variable
for (var in vars) {
  mean <- mean(turbine_df[, var])
  sd <- sd(turbine_df[, var])
  min <- min(turbine_df[, var])
  max <- max(turbine_df[, var])
  # convert turbine capacity to MW
  if (var == 't_cap') {
    sum_table[nrow(sum_table) + 1, ] <- c(var, round(mean/1000, 2), round(sd/1000, 2), round(min/1000, 2), round(max/1000, 2))
  } else { 
    sum_table[nrow(sum_table) + 1, ] <- c(var, round(mean, 2), round(sd, 2), round(min, 2), round(max, 2))
  }
}
  
sprintf('N: %s', n)

# aggregate data
agg_df <- turbine_df[!is.na(turbine_df$eia_id), ] # only consider data with an eia_id

# get appropriate aggregations for each variable
agg_df <- group_by(agg_df, eia_id) %>% dplyr::summarize(
  mean_p_cap = mean(p_cap),
  year = first(p_year), 
  num_t = n(), 
  m_t_cap = round(mean(t_cap)/1000, 2),
  m_t_hh = mean(t_hh),
  m_t_rd = mean(t_rd),
  pred_t_manu = names(which.max(table(t_manu))),
  pred_t_model = names(which.max(table(t_model))),
  pred_t_state = names(which.max(table(t_state))),
  pred_t_county = names(which.max(table(t_county))),
  )

# get summary table with statistics for each aggregate
proj_sum_table <- dplyr::summarize(agg_df,
  mean_of_p_cap = mean(mean_p_cap),
  mean_of_num_t = mean(num_t),
  mean_of_hh = mean(m_t_hh),
  mean_of_rd = mean(m_t_rd),
  sd_p_cap = sd(mean_p_cap),
  sd_num_t = sd(num_t),
  sd_hh = sd(m_t_hh),
  sd_rd = sd(m_t_hh),
  min_p_cap = min(mean_p_cap),
  min_num_t = min(num_t),
  min_hh = min(m_t_hh),
  min_rd = min(m_t_rd),
  max_p_cap = max(mean_p_cap),
  max_num_t = max(num_t),
  max_hh = max(m_t_hh),
  max_rd = max(m_t_rd),
)

# get values for the max of each variable since max values did not fit in the console
max_cols <- c('max_p_cap', 'max_num_t', 'max_hh', 'max_rd')
select(proj_sum_table, all_of(max_cols))

# get data for plot
plot_data <- group_by(agg_df, year) %>% dplyr::summarize(
  t_hh = mean(m_t_hh),
  t_rd = mean(m_t_rd)
)

# plot evolution of average hub height and rotor diameter at the project level
p <-    
  ggplot(data=plot_data, aes(x=year)) + 
  geom_line(aes(y=t_hh, colour='Hub-Height')) + 
  geom_point(aes(y=t_hh, colour='Hub-Height')) +
  geom_line(aes(y=t_rd, colour='Rotor Diameter')) + 
  geom_point(aes(y=t_rd, colour='Rotor Diameter')) +
  ylab('meters') +
  ggtitle('Evolution of hub height and rotor diameter') +
  scale_colour_manual("", 
                      breaks = c('Hub-Height', 'Rotor Diameter'),
                      values = c('Hub-Height' = 'blue', 'Rotor Diameter' = 'red'))



plotly::ggplotly(p)

# create a table containing the top five states with the most wind turbines, highest capacity for
# wind projects, and most technically advanced wind turbines

# aggregate data to state level
state_df <- group_by(turbine_df, t_state) %>% dplyr::summarize(
  num_t = n(),
  cap = sum(p_cap),
  tech = mean(t_hh+t_rd)
)

# get data for table
Most_Turbines = state_df[order(-state_df$num_t), ][1:5, ]$t_state
Highest_Capacity = state_df[order(-state_df$cap), ][1:5, ]$t_state
US_Cap = round(sum(state_df$cap) / 1000, 2)
State_Cap = round(state_df[order(-state_df$cap), ][1:5, ]$cap / 1000, 2)
Highest_Tech = state_df[order(-state_df$tech), ][1:5, ]$t_state

# create summary table
top_five_df <- data.frame(
  'Most Turbines' = Most_Turbines, 
  'Highest Capacity' = Highest_Capacity,
  'State Capacity' = State_Cap,
  'US Capacity' = US_Cap,
  'Highest Tech' = Highest_Tech)

perc_top_five = round(sum(top_five_df$State.Capacity)/ top_five_df$US.Capacity[1], 2)

#### Problem 2 ####

# bring in wind ordinance data
ordinance_df = read.csv('wind_ordinance_main.csv')

# remove observations before 2001
ordinance_df = ordinance_df[ordinance_df$ordinance_year >= 2001, ]

# find the state with the most wind ordinances
most_ordinances <- group_by(ordinance_df, State) %>% dplyr::summarise(
  num_o = n()
)
total_ordinances <- sum(most_ordinances$num_o)
state_most_o <- most_ordinances[order(-most_ordinances$num_o), ][1:5,]
state_most_o

# collapse data to year level and get the number of new ordinances as well as the
# cumulative number of ordinances for each year
year_df <- group_by(ordinance_df, ordinance_year) %>% dplyr::summarise(
  new_o = n(),
  cum_o = count(ordinance_df[ordinance_df$ordinance_year <= ordinance_year, ])
)
year_df

# plot yearly ordinance data
p <-    
  ggplot(data=year_df, aes(x=ordinance_year)) + 
  geom_line(aes(y=new_o, colour='New Ordinances')) + 
  geom_point(aes(y=new_o, colour='New Ordinances')) +
  geom_line(aes(y=cum_o$n, colour='Cumulative Ordinances')) + 
  geom_point(aes(y=cum_o$n, colour='Cumulative Ordinances')) +
  ylab('Number of Ordinances') +
  xlab('Year') +
  ggtitle('Ordinances by year') +
  scale_colour_manual("", 
                      breaks = c('New Ordinances', 'Cumulative Ordinances'),
                      values = c('New Ordinances' = 'blue', 'Cumulative Ordinances' = 'red'))



plotly::ggplotly(p)

#### Problem 3 ####

# bring the wind resource data into R
quality_df <- read.csv('wtk_site_metadata.csv')

# remove off shore observations
quality_df <- quality_df[!quality_df$power_curve == 'offshore', ]

# remove observation in Alaska and Hawaii
quality_df <- quality_df[!quality_df$State %in% c('Alaska', 'Hawaii'), ]
colnames(quality_df)

# create a summary table for wind speed, capacity factor, and fraction of usable area
vars <- c('wind_speed', 'capacity_factor', 'fraction_of_usable_area')

sum_table <- data.frame(var=character(), mean=numeric(), sd=numeric(), min=numeric(), max=numeric())
# add data
for (var in vars) {
  mean <- mean(quality_df[, var])
  sd <- sd(quality_df[, var])
  min <- min(quality_df[, var])
  max <- max(quality_df[, var])
  
  sum_table[nrow(sum_table) + 1, ] <- c(var, round(mean, 2), round(sd, 2), min, max)
}

# clean up row labels
var_names <- c('Wind Speed', 'Capacity Factor', 'Fraction of Usable Area')
sum_table$var <- var_names

# get the top five states with the highest wind speeds
top_five <- group_by(quality_df, State) %>% dplyr::summarise(
  ws = mean(wind_speed),
  cf = mean(capacity_factor)
) 
top_five_ws <- top_five[order(-top_five$ws), ][1:5, ]

# get the top five states with the highest capacity factors
top_five_cf <- top_five[order(-top_five$cf), ][1:5, ]


#### Problem 4: Merging the data ####

# read in mapping of state abbreviations
conversion_df <- read.csv('states.csv')
conversion_df
# convert State column in ordinance_df to abbreviations
ordinance_df$State <- factor(ordinance_df$State,
                             levels = conversion_df$State,
                             labels = conversion_df$Abbreviation)
# get rid of extra index column created by the mapping
ordinance_df <- select(ordinance_df, -X)

# get rid of ' county' from the end of the t_county column in turbine_df
turbine_df$t_county <- gsub(' County', '', turbine_df$t_county)

# merge the wind turbine and wind ordinance data 
joined_df <- left_join(turbine_df, ordinance_df,
                        by = join_by(t_state == State,
                                     t_county == County), 
                       keep=TRUE
)
# joined_df[is.na(joined_df$ordinance_year), ]

# create a binary variable to track if an observation has an ordinance in the given year.
# a value of 1 indicates the presence of an ordinance, while a value of 0 indicates no ordinance
joined_df$has_ordinance <- ifelse(is.na(joined_df$ordinance_year), 0, 1)

# collapse data to county level
county_df <- joined_df %>% group_by(t_county, p_year) %>% dplyr::summarise(
  state = first(t_state),
  cap = round(sum(t_cap) / 1000, 2),
  pre_manu = names(which.max(table(t_manu))),
  pre_model = names(which.max(table(t_model))),
  avg_hh = mean(t_hh),
  avg_rd = mean(t_rd),
  has_ordinance = min(has_ordinance)
)

# get average wind capacity (MW) over the years for counties with ordinances vs those without
plot_df <- county_df %>% group_by(has_ordinance, p_year) %>% dplyr::summarise(
  cap = mean(cap)
)
plot_df
p <-    
  ggplot() + 
  geom_line(data=plot_df[plot_df$has_ordinance==1, ],  aes(x=p_year, y=cap, colour='With Ordinance')) + 
  geom_point(data=plot_df[plot_df$has_ordinance==1, ],  aes(x=p_year, y=cap, colour='With Ordinance')) +
  geom_line(data=plot_df[plot_df$has_ordinance==0, ],  aes(x=p_year, y=cap, colour='No Ordinance')) + 
  geom_point(data=plot_df[plot_df$has_ordinance==0, ],  aes(x=p_year, y=cap, colour='No Ordinance')) +
  ylab('Average Wind Capacity (MW)') +
  xlab('Year') +
  ggtitle('Wind capacity (MW) by presence of ordinance') +
  scale_colour_manual("", 
                      breaks = c('With Ordinance', 'No Ordinance'),
                      values = c('With Ordinance' = 'blue', 'No Ordinance' = 'red'))



plotly::ggplotly(p)
  
# collapse joined_df to the project level
project_df <- joined_df %>% group_by(eia_id, t_state, t_county, has_ordinance) %>% dplyr::summarise(
    cap = round(sum(t_cap) / 1000, 2),
    pre_manu = names(which.max(table(t_manu))),
    pre_model = names(which.max(table(t_model))),
    avg_hh = mean(t_hh),
    avg_rd = mean(t_rd)
  )

# create a summary table that displays the average project capacity and average
# rotor diameter for counties with ordinances vs counties without ordinances


# get data for sum_table
col1 <- c(mean(project_df[project_df$has_ordinance==1, ]$cap),
          sd(project_df[project_df$has_ordinance==1, ]$cap),
          min(project_df[project_df$has_ordinance==1, ]$cap),
          max(project_df[project_df$has_ordinance==1, ]$cap),
          mean(project_df[project_df$has_ordinance==1, ]$avg_rd),
          sd(project_df[project_df$has_ordinance==1, ]$avg_rd),
          min(project_df[project_df$has_ordinance==1, ]$avg_rd),
          max(project_df[project_df$has_ordinance==1, ]$avg_rd))
col2 <- c(mean(project_df[project_df$has_ordinance==0, ]$cap),
          sd(project_df[project_df$has_ordinance==0, ]$cap),
          min(project_df[project_df$has_ordinance==0, ]$cap),
          max(project_df[project_df$has_ordinance==0, ]$cap),
          mean(project_df[project_df$has_ordinance==0, ]$avg_rd),
          sd(project_df[project_df$has_ordinance==0, ]$avg_rd),
          min(project_df[project_df$has_ordinance==0, ]$avg_rd),
          max(project_df[project_df$has_ordinance==0, ]$avg_rd))

# add data to summary table
sum_table <- data.frame('Counties with ordinance'=col1,
                        'Counties without ordinance'=col2)
sum_table$difference <- sum_table$Counties.with.ordinance - sum_table$Counties.without.ordinance
# add difference columns

# set row labels
row.names(sum_table) <- c('avg_proj_cap',
                          'sd_proj_cap',
                          'min_proj_cap',
                          'max_proj_cap',
                          'avg_proj_rd',
                          'sd_proj_rd',
                          'min_proj_rd',
                          'max_proj_rd')


