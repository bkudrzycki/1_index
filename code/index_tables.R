## ---- tbl-macrocorr --------

wb_recode <- function(data) {
  data$`Country.Name` <- data$`Country.Name` %>% 
    recode("Congo, Dem. Rep." = "Congo, Democratic Republic of the",
           "Yemen, Rep." = "Yemen",
           "Gambia, The" = "Gambia",
           "Egypt, Arab Rep." = "Egypt",
           "Lao PDR" = "Lao People's Democratic Republic",
           "Congo, Rep." = "Congo",
           "Cote d'Ivoire" = "Côte d'Ivoire",
           "Tanzania" = "Tanzania, United Republic of",
           "Kyrgyz Republic" = "Kyrgyzstan",
           "Iran, Islamic Rep." = "Iran, Islamic Republic of",
           "Moldova" = "Moldova, Republic of",
           "Slovak Republic" = "Slovakia",
           "Vietnam" = "Viet Nam",
           "Czech Republic" = "Czechia",
           "Macao SAR, China" = "Macau, China",
           "Hong Kong SAR, China" = "Hong Kong, China",
           "Korea, Rep." = "Korea, Republic of",
           "Macedonia, FYR" = "North Macedonia",
           "West Bank and Gaza" = "Occupied Palestinian Territory",
           "Micronesia, Fed. Sts." = "Micronesia, Federated States of",
           "Korea, Dem. People’s Rep." = "Korea, Democratic People's Republic of",
           "Cabo Verde" = "Cape Verde")
  data %>% rename(country = Country.Name)
}

# load gdp

gdp <- read.csv("../data/raw/gdp_PPP_percap_worldbank.csv") %>%
  wb_recode() %>% 
  mutate(gdp = X2018) %>% 
  dplyr::select(country, gdp)

# load productivity growth

prod_growth <- read.csv("../data/raw/productivity_growth_ilostat.csv") %>% 
  rename(country_code = ref_area,
         prod_growth19 = obs_value) %>% 
  filter(time == 2019)

# load youth population

pop <- read.csv("../data/raw/population_age_ilostat.csv") %>%
  filter(time == 2019,
         sex.label == "Sex: Total") %>% 
  pivot_wider(names_from = classif1.label, values_from = obs_value)

pop <- pop %>% 
  mutate(youth_ratio = `Age (Youth, adults): 15-24` / `Age (Aggregate bands): Total`) %>%
  rename("country" = "ref_area.label",
         "total_pop" = `Age (Aggregate bands): Total`,
         "youth_pop" = `Age (Youth, adults): 15-24`,
         "adult_pop" = `Age (Youth, adults): 25+`)

# load population growth

pop_growth <- read.csv("../data/raw/population_growth_wb.csv") %>%
  wb_recode() %>%
  rename("pop_growth_2005" = X2005,
         "pop_growth_2019" = X2019) %>% 
  dplyr::select(c(country, pop_growth_2005, pop_growth_2019))

## load minimum wage

mw_usd <- minimum_wage <- read.csv("../data/raw/minimum_wage_ilostat.csv") %>%
  pivot_wider(names_from = classif1.label, values_from = obs_value) %>% 
  rename("country" = "ref_area.label",
         "mw_usd" = `Currency: U.S. dollars`) %>% 
  filter(!is.na(`mw_usd`)) %>% 
  group_by(country) %>% 
  top_n(1, time) %>% 
  dplyr::select(c(country, mw_usd, time))

mw_ppp <- minimum_wage <- read.csv("../data/raw/minimum_wage_ilostat.csv") %>%
  pivot_wider(names_from = classif1.label, values_from = obs_value) %>% 
  rename("country" = "ref_area.label",
         "mw_ppp" = `Currency: 2017 PPP $`) %>% 
  filter(!is.na(`mw_ppp`)) %>% 
  group_by(country) %>% 
  top_n(1, time) %>% 
  dplyr::select(c(country, mw_ppp, time))

## load fertility rate

fertility <- read.csv("../data/raw/fertility_worldbank.csv") %>%
  wb_recode() %>% 
  pivot_longer(cols = c(5:65), names_to = "time", names_prefix = "X", values_to = "obs_value") %>% 
  filter(time > 2009,
         !is.na(obs_value)) %>%
  group_by(country) %>% 
  top_n(1, time) %>% 
  dplyr::select(c(country, fertility_rate = obs_value, time))

## load unemployment_rate 

youth_unemp_rate <- unemployment_rate %>% 
  rename(country = ref_area.label) %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(country) %>%
  top_n(1, time) %>% 
  dplyr::select(country, youth_unemp_rate = obs_value, time)


## load savings rate

savings_rate <- read.csv("../data/raw/savings_rate_worldbank.csv") %>%
  wb_recode() %>% 
  pivot_longer(cols = c(5:65), names_to = "time", names_prefix = "X", values_to = "obs_value") %>% 
  filter(time > 2009,
         !is.na(obs_value)) %>%
  group_by(country) %>% 
  top_n(1, time) %>% 
  dplyr::select(c(country, savings_rate = obs_value, time))


## load hdi

hdi <- read.xlsx("../data/raw/hdi.xlsx") %>% dplyr::select(c(country, hdi =`2019`)) %>% 
  mutate(hdi = hdi*100,
         country = substr(country,2,str_length(country)))

hdi$country <- hdi$country %>% 
  recode("Congo (Democratic Republic of the)" = "Congo, Democratic Republic of the",
         "Moldova (Republic of)" = "Moldova, Republic of",
         "Tanzania (United Republic of)" = "Tanzania, United Republic of",
         "Iran (Islamic Republic of)" = "Iran, Islamic Republic of",
         "Palestine, State of" = "Occupied Palestinian Territory")


## load remaining correlates
correlates <- read.xlsx("../data/raw/correlates_wb.xlsx") %>% 
  pivot_longer(c("2016.[YR2016]":"2020.[YR2020]"), names_to = "time") %>% 
  mutate(time = as.numeric(substr(time, 1,4))) %>% 
  rename(series = `Series.Name`) %>% 
  wb_recode()


y <- levels(factor(correlates$series))

count = 0
# dplyr::select most recent observation for each variable
for (i in y) {
  x <- correlates %>% 
    filter(series == i) %>% 
    group_by(country) %>% 
    filter(value != "..") %>% 
    top_n(1, time) %>% 
    dplyr::select(country, value) %>% 
    mutate(value = as.numeric(value))
  assign(paste0("corr", count), x)
  count = count + 1
}

df <- rank %>% 
  dplyr::select(country, country_code, transition_mean, working_conditions_mean, education_mean, index_mean) %>% 
  left_join(male, by = "country") %>% 
  left_join(female, by = "country")

df <- df %>%
  left_join(gdp, by = "country") %>% 
  left_join(youth_unemp_rate, by = "country") %>% 
  left_join(pop_growth, by = "country") %>% 
  left_join(fertility, by = "country") %>% 
  left_join(minimum_wage, by = "country") %>% 
  left_join(savings_rate, by = "country") %>% 
  left_join(pop, by = "country") %>% 
  left_join(hdi, by = "country") %>% 
  left_join(corr0, by = "country") %>% 
  rename("access_to_elec" = value) %>% 
  left_join(corr1, by = "country") %>% 
  rename("agriculture" = value) %>% 
  left_join(corr4, by = "country") %>% 
  rename("doing_business" = value) %>% 
  left_join(corr5, by = "country") %>% 
  rename("export" = value) %>% 
  left_join(corr6, by = "country") %>% 
  rename("fdi" = value) %>% 
  left_join(corr9, by = "country") %>% 
  rename("manu_value" = value) %>% 
  left_join(corr11, by = "country") %>% 
  rename("urban_pop" = value) %>% 
  mutate(youth_ratio = youth_ratio * 100,
         log_gdp = log(gdp))

options(scipen=4, digits=4)

m1 <- lm(data = df, index_mean ~ youth_unemp_rate)
m2 <- lm(data = df, index_mean ~ hdi)
m3 <- lm(data = df, index_mean ~ log_gdp)
m4 <- lm(data = df, index_mean ~ youth_ratio)
m5 <- lm(data = df, index_mean ~ fertility_rate)
m6 <- lm(data = df, index_mean ~ log_gdp + youth_ratio + fertility_rate)
m7 <- lm(data = df, index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)
m8 <- lm(data = df, index_mean ~ youth_ratio + fertility_rate + agriculture + manu_value + export + fdi + savings_rate + doing_business + urban_pop + access_to_elec)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, omit.stat = c("f", "adj.rsq", "ser"), column.sep.width = "-5pt", dep.var.caption = "\\textit{Dependent variable:} YLILI Score", dep.var.labels = FALSE, omit = "Constant", font.size= "footnotesize", column.labels = NULL, no.space=TRUE, table.placement = "H", digits = 3, header = F,
          covariate.labels = c("Youth unemployment rate", 
                               "HDI Score ($\\times$ 100)", 
                               "log(GDP)", 
                               "Youth pop. ratio ($\\times$ 100)", 
                               "Fertility rate", "Agriculture (\\% of GDP)", 
                               "Manufacturing (\\% of GDP)", "Exports (\\% of GDP)",
                               "FDI (\\% of GDP)", "Savings rate (\\% of GDP)",
                               "Ease of Doing Business",
                               "Urbanization rate",
                               "Access to Electricity"), 
        notes = "Standard errors shown in parentheses. YLILI score is on a scale of 0-100.", notes.align = "r", notes.append = TRUE, title = "Macro correlates of YLILI score", label = "tab:tbl-macrocorr")

## ---- tbl-regwide --------

df <- left_join(df, (male %>% select(country, male_index_mean = index_mean)), by = "country")
df <- left_join(df, (female %>% dplyr::select(country, female_index_mean = index_mean)), by = "country")

options(scipen=4, digits=4)

m9 <- lm(data = df, index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m10 <- lm(data = df, male_index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m11 <- lm(data = df, female_index_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m12 <- lm(data = df, transition_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m13 <- lm(data = df, working_conditions_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

m14 <- lm(data = df, education_mean ~ log_gdp + youth_ratio + fertility_rate + agriculture + manu_value + export)

stargazer(m9, m10, m11, m12, m13, m14, omit.stat = c("f", "adj.rsq", "ser"), column.sep.width = "-2pt", column.labels  = c("Overall", "Male", "Female", "Transition", "Work. cond.", "Education"), dep.var.labels = c("", "\\textit{\\underline{YLILI score}}", "", "", "\\textit{\\underline{Dimension}}", ""), header = F, digits = 3,
          covariate.labels = c("log(GDP)",
                               "Youth pop. ratio ($\\times$ 100)",
                               "Fertility rate", "Agriculture (\\% of GDP)",
                               "Manufacturing (\\% of GDP)",
                               "Exports (\\% of GDP)"),
          omit = "Constant", font.size= "footnotesize", model.numbers = FALSE, float = FALSE, notes = "Standard errors shown in parentheses. YLILI score is on a scale of 0-100.", title = "Macro correlates, gender-specific YLILI and YLILI dimensions", label = "tab:tbl-regwide")

## ---- tbl-gender --------

## Table of index outcomes by gender

options(scipen=2, digits=4)

male <- male %>% 
  filter(!is.na(index_mean)) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select("YLILI score" = index_mean,
         "Transition" = transition_mean,
         "Share of youth NEET" = neet,
         "Youth skills mismatch rate" = mismatch,
         "Relative working conditions ratio" = relative_wc,
         "Working conditions" = working_conditions_mean,
         "Youth working poverty rate" = workingpov,
         "Youth TR underemployment rate" = underemp,
         "Share of youth in informal employment" = informal,
         "Share of youth in elementary occup." = elementary,
         "Education" = education_mean,
         "Share of youth with no secondary educ." = nosecondary,
         "Youth illiteracy rate" = literacy,
         "Harmonized test scores" = test_scores) %>% t()

female <- female %>% 
  filter(!is.na(index_mean)) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  select("YLILI score" = index_mean,
         "Transition" = transition_mean,
         "Share of youth NEET" = neet,
         "Youth skills mismatch rate" = mismatch,
         "Relative working conditions ratio" = relative_wc,
         "Working conditions" = working_conditions_mean,
         "Youth working poverty rate" = workingpov,
         "Youth TR underemployment rate" = underemp,
         "Share of youth in informal employment" = informal,
         "Share of youth in elementary occup." = elementary,
         "Education" = education_mean,
         "Share of youth with no secondary educ." = nosecondary,
         "Youth illiteracy rate" = literacy,
         "Harmonized test scores" = test_scores) %>% t()

gender_comp <- as.data.frame(cbind(male, female)) %>% mutate("$\\Delta$" = V1-V2) %>% rename("Male" = V1, "Female" = V2)

kbl(gender_comp, col.names=c('Male', 'Female', '$\\Delta$'), align = 'l', 'latex', booktabs = T, linesep = "", escape = F, caption = "Mean YLILI dimension and indicator scores by gender", digits = 2) %>% 
  kableExtra::add_indent(positions = c(2:4)) %>% 
  kableExtra::add_indent(positions = c(6:10)) %>% 
  kableExtra::add_indent(positions = c(12:14)) %>% 
  kableExtra::row_spec(1, bold = TRUE) %>% 
  footnote(general = "\\\\textit{Notes:} Most recent observations, dating back no further than 2010. Rescaled indicator scores shown—higher values always correspond to better labor market outcomes.",
           threeparttable = T,
           escape = F,
           fixed_small_size = T,
           general_title = "")

