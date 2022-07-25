maddison_project_df <- read_excel("~/Google Drive/3 - Misc. Data Research/Misc. Data/Penn World Tables/Maddison Project Database 2020.xlsx", 
                                             sheet = "Full data")
table(maddison_project_df$country)

maddison_project_df %>%
  group_by(countrycode) %>%
  filter(year %in% c(1917:1991)) %>%
  mutate(per_capita_GDP_growth = pct_change_function(gdppc),
         per_capita_GDP_growth_5yr_MA = five_yr_moving_avg_func(per_capita_GDP_growth),
         countrycode = if_else((countrycode == "SUN" & year <= 1991), "USSR", countrycode)) %>%
  filter(countrycode %in% c("USA", "GBR", "CAN", "JPN", "FRA", "DEU", "USSR", "CHN")) %>%
  select(countrycode, year,
         per_capita_GDP_growth_5yr_MA) %>%
  ungroup(countrycode) %>%
  gather(key = "Legend", value = "value", -countrycode, -year) %>%
  ggplot(aes(x = year, y = value)) +
  geom_line(aes(linetype = `Legend`)) +
  theme_bw() +
  labs(y = "",
       x = "Year",) + # this creates space between x-axis and the caption
  theme(text = element_text(face = 'bold'),
        panel.background = element_rect(colour = "black"),
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        plot.caption = element_text(hjust = 0, face = "bold.italic"),
        plot.caption.position = "plot",
        legend.position="right",
        strip.text.x = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  scale_x_continuous(limits = c(1910, 1991), breaks = seq(1910, 1991, by = 10)) +
  scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, by = 5)) +
  geom_hline(yintercept = 4, linetype="dashed", color = "red") +
  geom_vline(xintercept = 1939, linetype="dashed", color = "blue") +
  facet_wrap(vars(countrycode))

