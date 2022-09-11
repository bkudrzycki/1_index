## ---- fig-infunemp --------

unemp_r <- read_csv("../data/raw/unemployment_rate_sex_age_ilostat.csv", col_types = cols()) %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(ref_area.label) %>%
  top_n(1, time) %>% dplyr::select(ref_area.label, unemp_r = obs_value)

inform_r <- informal %>% dplyr::select(ref_area.label, inform_r = "Sex: Total") %>% filter(ref_area.label != "Moldova, Republic of",
                                                                                           ref_area.label != "Ukraine",
                                                                                           ref_area.label != "Mozambique")

pop <- read.csv("../data/raw/population_age_ilostat.csv") %>%
  filter(time == 2019,
         sex.label == "Sex: Total") %>% 
  pivot_wider(names_from = classif1.label, values_from = obs_value)

pop <- pop %>% 
  rename("total_pop" = `Age (Aggregate bands): Total`,
         "youth_pop" = `Age (Youth, adults): 15-24`,
         "adult_pop" = `Age (Youth, adults): 25+`)

df <- left_join(all_countries, unemp_r, by ="ref_area.label") %>% left_join(., inform_r, by ="ref_area.label") %>% left_join(., pop, by ="ref_area.label")

ggplot(df, aes(x = inform_r, y = unemp_r)) +
  geom_point(aes(shape = inc_level, color = inc_level)) +
  xlab("Youth informality rate") +
  ylab("Youth unemployment rate") +
  theme_minimal() +
  geom_smooth(method = "lm", aes(color = inc_level, linetype = inc_level), se = F, size=0.5) +
  scale_linetype_manual(name = "World Bank Income Classification", labels = c("LIC/LMIC", "HIC/UMIC"),
                        values = c("LIC/LMIC" = 1, "HIC/UMIC" = 2)) +
  scale_colour_manual(name = "World Bank Income Classification", labels = c("LIC/LMIC", "HIC/UMIC"), values = c("gray", "black")) +
  scale_shape_manual(name = "World Bank Income Classification", labels = c("LIC/LMIC", "HIC/UMIC"), values=c(17, 16)) +
  #geom_text_repel(aes(label=country_code),size = 3)
  labs(linetype="World Bank Income Classification") +
  theme(legend.position="top")
