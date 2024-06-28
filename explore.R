library(dplyr)
library(covidcast)
library(ggplot2)

drunkest_states = readRDS('dat/drunkest_states.rds')
df = read.csv('dat/scrubbed.csv')
mh = read.csv('dat/mh_ranks.csv')

mh = mh[,c(1,5)]
colnames(mh) = c("State", "mh_rank")

drunk_states_df = data.frame(drunkest_states = drunkest_states,
                         drunk_rank = 1:50)

bars_df = read.csv("dat/bars.csv")
bars_df = bars_df[,1:2]

pops = state_census %>% select(NAME, POPESTIMATE2019, ABBR)

bars_df = left_join(bars_df, pops, join_by('State' == 'NAME')) %>%
  arrange(desc(Bars))

df_us = df %>% filter(country == 'us') %>%
  mutate(state = toupper(state))

counts_us = data.frame(table(df_us$state)) %>%
  left_join(bars_df, join_by('Var1' == 'ABBR')) %>%
  mutate(scaled_aliens = Freq / POPESTIMATE2019,
         scaled_bars = Bars / POPESTIMATE2019)

drunkest_df = counts_us %>% select(Var1, scaled_bars) %>% arrange(desc(scaled_bars))
drunkest_df$bars_rank = 1:nrow(drunkest_df)
drunkest_df = drunkest_df[,c(1,3)]

counts_us = left_join(counts_us, drunkest_df, by = 'Var1')

counts_us = left_join(counts_us, drunk_states_df, join_by("State" == "drunkest_states"))
counts_us = left_join(counts_us, mh, by = "State")


counts_us_top = counts_us %>%
  arrange(desc(scaled_aliens))
counts_us_top = counts_us_top[1:15,]

mean_mh = mean(counts_us_top$mh_rank)

ggplot(counts_us_top, aes(x = Var1, y = scaled_aliens, fill = State)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = mh_rank), vjust = -0.5) +
  annotate("text", x = Inf, y = Inf, label = paste("Mean Rank:", round(mean_mh, 2)),
           hjust = 3, vjust = 2, size = 5, color = "black") +
  labs(x = 'State', y = 'Count', title = "Alien Sightings (scaled by population)", subtitle = "Mental Health Rank Shown")

length(which(counts_us_top$mh_rank >= 25))


 