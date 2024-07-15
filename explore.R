library(dplyr)
library(covidcast)
library(ggplot2)
library(lubridate)

drunkest_states = readRDS('dat/drunkest_states.rds')
df = read.csv('dat/scrubbed.csv')
mh = read.csv('dat/mh_ranks.csv')
drug_df = readRDS('dat/drug_use.rds')
religion_df = readRDS('dat/religion_df.rds')
mb_df = readRDS('dat/mb_df.rds') %>% na.omit()

mb_df$military_rank = 50:1
religion_df$religion_rank = 50:1

df$datetime <- mdy_hm(df$datetime)
df = df %>% filter(datetime >= as.Date("2010-01-01"))


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
counts_us = left_join(counts_us, drug_df, join_by("State" == "state_names"))
counts_us = left_join(counts_us, religion_df, join_by("State" == "state"))
counts_us = left_join(counts_us, mb_df, join_by("State" == "states"))




counts_us_top = counts_us %>%
  arrange(desc(scaled_aliens))
counts_us_top = counts_us_top[1:15,]

mean_val = mean(counts_us_top$drunk_rank)

# Fit the linear model
fit_drunk <- lm(scaled_aliens ~ drunk_rank, data = counts_us)
fit_religion <- lm(scaled_aliens ~ religion_rank, data = counts_us)
fit_drug <- lm(scaled_aliens ~ drug_rank, data = counts_us)

# Get the coefficients (intercept and slope)
coefficients <- coef(fit)

summary(fit_drug)$r.squared


abs(coef(fit_religion)[2]) / abs(coef(fit_drunk)[2])


ggplot(data = counts_us) +
  # geom_point(aes(x = drunk_rank, y = scaled_aliens), shape = 16, color = 'blue', size = 2.5) +
  # geom_point(aes(x = drug_rank, y = scaled_aliens), shape = 17, color = 'red', size = 2.5) +
  geom_point(aes(x = religion_rank, y = scaled_aliens), shape = 18, color = 'purple', size = 4) +
  geom_smooth(aes(x = drunk_rank, y = scaled_aliens, color = 'Drunk'), linetype = 17,  method = 'lm', se = FALSE) +
  geom_smooth(aes(x = drug_rank, y = scaled_aliens, color = "Drug"), linetype = 17, method = 'lm', se = FALSE, color = 'red') +
  geom_smooth(aes(x = military_rank, y = scaled_aliens, color = "Drug"), linetype = 17, method = 'lm', se = FALSE, color = 'green') +
  geom_smooth(aes(x = religion_rank, y = scaled_aliens, color = "Religion"), method = 'lm', size = 2.5, se = FALSE, color = 'purple') +
  labs(title = "Alien sightings vs Religion Rank",
       subtitle = "scaled by population of each state",
       color = 'Rank Type') +
  xlab("Rank") +
  ylab("Scaled Alien Sightings") +
  scale_color_manual(values = c("Drunk" = "blue", "Drug" = "red", "Religion" = "purple"))

ggplot(counts_us_top, aes(x = Var1, y = scaled_aliens, fill = State)) +
  geom_bar(stat = 'identity') +
  geom_text(aes(label = mh_rank), vjust = -0.5) +
  annotate("text", x = Inf, y = Inf, label = paste("Mean Rank:", round(mean_val, 2)),
           hjust = 3, vjust = 2, size = 5, color = "black") +
  labs(x = 'State', y = 'Sightings / State Pop', title = "Alien Sightings (scaled by population)", subtitle = "Mental Health Rank Shown")

length(which(counts_us_top$drunk_rank >= 25))


 