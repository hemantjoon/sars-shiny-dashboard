## attaching database
df <- read.csv("random.csv")
df <- df %>% 
  mutate(Release_Date= as.Date(Release_Date, format= "%Y-%m-%d"))
totalCases <- nrow(df)

mdf <- as.data.frame(table(df$Release_Date))
colnames(mdf) <- c('date', 'cases')
mdf <- mdf %>% 
  mutate(date = as.Date(date, format= "%Y-%m-%d")) %>%
  mutate(cumulative_cases = cumsum(cases))


mdf_month <- mdf
mdf_month$date <- format(as.POSIXlt(mdf$date, format="%Y-%m-%d"), "%Y-%m")
mdf_month <- mdf_month %>% group_by(date) %>%
  summarise(cases = sum(cases))
mdf_month <- mdf_month %>% 
  mutate(cumulative_cases = cumsum(cases))


mdf_year <- mdf
mdf_year$date <- format(as.POSIXlt(mdf$date, format="%Y-%m-%d"), "%Y")
mdf_year <- mdf_year %>% group_by(date) %>%
  summarise(cases = sum(cases))
mdf_year <- mdf_year %>% 
  mutate(cumulative_cases = cumsum(cases))
  
  
## Map

#library(giscoR)
#library(sf)


## data germany

germany_df <- subset(df, df$Country == 'Germany')
germany_df <- as.data.frame(table(germany_df$Release_Date))
colnames(germany_df) <- c('date', 'cases')
germany_df <- germany_df %>% 
  mutate(date = as.Date(date, format= "%Y-%m-%d")) %>%
  mutate(cumulative_cases = cumsum(cases))


germany_df_month <- germany_df
germany_df_month$date <- format(as.POSIXlt(germany_df$date, format="%Y-%m-%d"), "%Y-%m")
germany_df_month <- germany_df_month %>% group_by(date) %>%
  summarise(cases = sum(cases))
germany_df_month <- germany_df_month %>% 
  mutate(cumulative_cases = cumsum(cases))


germany_df_year <- germany_df
germany_df_year$date <- format(as.POSIXlt(germany_df$date, format="%Y-%m-%d"), "%Y")
germany_df_year <- germany_df_year %>% group_by(date) %>%
  summarise(cases = sum(cases))
germany_df_year <- germany_df_year %>% 
  mutate(cumulative_cases = cumsum(cases))




countries_all <- gisco_get_countries()
colnames(countries_all)[4] <- 'country'
countries_all$country <- replace(countries_all$country, countries_all$country == 'Russian Federation', 'Russia')
countries_all$country <- replace(countries_all$country, countries_all$country == 'United States', 'USA')
countries_all$country <- replace(countries_all$country, countries_all$country == 'Myanmar/Burma', 'Myanmar')
countries_all_centre <- suppressWarnings({st_centroid(countries_all, of_largest_polygon = TRUE)})
countries_all$position <- countries_all_centre$geometry 


country_temp <- as.data.frame(table(df$Country))
colnames(country_temp) <- c('country', 'cases')
country_temp$country <- as.character(country_temp$country)
country_temp$country <- replace(country_temp$country, country_temp$country == 'Viet Nam', 'Vietnam')

country_merge <- merge(country_temp, countries_all, by = 'country')  


## Germany Map

df_germany <- subset(df, df$Country == 'Germany')
df_germany_count <- as.data.frame(table(df_germany$Geo_Location))

colnames(df_germany_count) <- c('state', 'cases')
df_germany_count$state <- as.character(df_germany_count$state)

total_cases_germany <- nrow(df_germany)
ok <- sapply(df_germany_count$state, function(x) grepl('Germany:Europe/Germany/', x))
df_germany_count_state <- df_germany_count[ok,]
df_germany_count_state$state <- gsub('Germany:Europe/Germany/', '', df_germany_count_state$state)

germany_all <-  gisco_get_nuts(nuts_level = 1, country = "Germany")

colnames(germany_all)[6] <- 'state'
germany_all$state <- sapply(germany_all$state, function(x) str_to_title(x))

germany_all$state <- replace(germany_all$state, germany_all$state == 'Baden-Württemberg', 'Baden-Wurttemberg')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Bayern', 'Bavaria')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Hessen', 'Hesse')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Mecklenburg-Vorpommern', 'Mecklenburg-Western Pomerania')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Niedersachsen', 'Lower Saxony')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Nordrhein-Westfalen', 'North Rhine-Westphalia')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Rheinland-Pfalz', 'Rhineland-Palatinate')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Sachsen', 'Saxony')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Sachsen-Anhalt', 'Saxony-Anhalt')
germany_all$state <- replace(germany_all$state, germany_all$state == 'Thüringen', 'Thuringia')

germany_all_centre <- suppressWarnings({st_centroid(germany_all, of_largest_polygon = TRUE)})
germany_all$position <- germany_all_centre$geometry 

germany_merge <- merge(df_germany_count_state, germany_all, by = 'state')




#####################  Lineages ##########################

host_df <-df[,c('Release_Date', 'Host', 'Length')]
host_df_table <- as.data.frame(table(host_df$Host))
colnames(host_df_table) <- c('host', 'counts')
host_df_table$host <- as.character(host_df_table$host)

unknown_host <- function(x){
  if(x=="")
    return('Unknown')
  else 
    return(x)
}

host_df_table$host <- sapply(host_df_table$host, unknown_host)


## Pie Chart NucleotideCompletness

df_partial <- as.data.frame(table(df$Nuc_Completeness))


## Line Chart


#lineages_df <- df[,c('Release_Date', 'Pangolin')]
#all_dates <- unique(lineages_df$Release_Date)
#all_dates <- as.character(all_dates[order(all_dates)])

voc_list <- c('BA.2', 'BA.4', 'BA.5', 'XBB.1.5', 'BA.2.86', 'BA.2.87.1', 'XAK', 'B.1.617.2', 'P.1', 'B.1.1.7')
#lineages_df_count <- as.data.frame(all_dates)
#colnames(lineages_df_count) <- c('Release_Date')

#for (j in voc_list){
#  my_list <- c()
#  lineages_df_count[,j] <- NA
#  for(i in all_dates){
#    ok <- lineages_df %>% filter(Release_Date == i) %>% filter(grepl(j, Pangolin))
#    ok_count <- nrow(ok)
#    my_list <- append(my_list, ok_count)
#  }
#  lineages_df_count[,j] <- my_list
#}
#write.csv(lineages_df_count, 'lineages_df_count.csv', row.names = FALSE)

lineages_df_count <- read.csv('lineages_df_count.csv')
lineages_df_count$Release_Date <- as.Date(lineages_df_count$Release_Date, format= "%Y-%m-%d")



#plot_ly(lineages_df_count, type = 'scatter', mode = 'lines')%>%
#  add_trace(x = ~Release_Date, y = ~BA.2, name = 'BA.2')%>%
#  add_trace(x = ~Release_Date, y = ~BA.4, name = 'BA.4')%>%
#  add_trace(x = ~Release_Date, y = ~BA.5, name = 'BA.5')%>%
#  add_trace(x = ~Release_Date, y = ~XBB.1.5, name = 'XBB.1.5')%>%
#  add_trace(x = ~Release_Date, y = ~BA.2.86, name = 'BA.2.86')%>%
#  add_trace(x = ~Release_Date, y = ~BA.2.87.1, name = 'BA.2.87.1')%>%
#  add_trace(x = ~Release_Date, y = ~XAK, name = 'XAK')%>%
#  add_trace(x = ~Release_Date, y = ~B.1.617.2, name = 'BA.2')%>%
#  add_trace(x = ~Release_Date, y = ~P.1, name = 'P.1')%>%
#  add_trace(x = ~Release_Date, y = ~B.1.1.7, name = 'B.1.1.7')%>%
#  layout(showlegend = T, plot_bgcolor='#e5ecf6',
#         xaxis = list(zerolinecolor = '#ffff',
#                      zerolinewidth = 2,
#                      gridcolor = 'ffff'),
#         yaxis = list(zerolinecolor = '#ffff',
#                      zerolinewidth = 2,
#                      gridcolor = 'ffff',title = 'Counts'))




