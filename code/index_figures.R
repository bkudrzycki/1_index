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


## ---- fig-gendercorr --------

comp <- full_join(male, female, by = c("country", "country_code"), suffix = c("_male", "_female")) %>% 
  full_join(., rank, by = c("country", "country_code")) %>% 
  mutate(index_diff = index_mean_male-index_mean_female) %>% 
  left_join(regions, by = "country") %>% 
  filter(`Region` != "Oceania")

p1 <- ggplot(comp, aes(x = index_mean_male, y = index_mean_female, label = country_code)) +
  geom_point(aes(color = `Region`)) +
  geom_abline(slope = 1) +
  annotate("text", x=80, y=98, label= "Overall", size = 4.5) +
  xlab("Male Score") +
  ylab("") +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x=element_text(hjust=.575)) +
  geom_text_repel(aes(label=country_code),size = 3) +
  xlim(50,100) +
  ylim(50,100) 

p2 <-ggplot(comp, aes(x = transition_mean_male, y = transition_mean_female, label = country_code)) +
  geom_point(aes(color = `Region`), size = .5) +
  geom_abline(slope = 1) +
  ggtitle("Transition") +
  xlab("") +
  ylab("") +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  theme_minimal() +
  guides(color="none") +
  #geom_text_repel(aes(label=country_code),size = 3) +
  xlim(20,100) +
  ylim(20,100)

p3 <-ggplot(comp, aes(x = working_conditions_mean_male, y = working_conditions_mean_female, label = country_code)) +
  geom_point(aes(color = `Region`), size = .5) +
  geom_abline(slope = 1) +
  ggtitle("Working conditions") +
  xlab("") +
  ylab("") +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  theme_minimal() +
  guides(color="none") +
  #geom_text_repel(aes(label=country_code),size = 3) +
  xlim(20,100) +
  ylim(20,100)

p4 <-ggplot(comp, aes(x = education_mean_male, y = education_mean_female, label = country_code)) +
  geom_point(aes(color = `Region`), size = .5) +
  geom_abline(slope = 1) +
  ggtitle("Education") +
  xlab("") +
  ylab("") +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  theme_minimal() +
  guides(color="none") +
  #geom_text_repel(aes(label=country_code),size = 3) +
  xlim(20,100) +
  ylim(20,100)

yaxis = textGrob("Female Score", rot = 90, vjust = 0.5, hjust = -.15)

grid.arrange(yaxis, p1, p2, p3, p4, heights = c(3,2,2,2), widths = c(.05,1,1,1),
             layout_matrix = rbind(c(1,3,4,5), c(1,2,2,2), c(1,2,2,2), c(1,2,2,2)))

## ---- fig-cormat --------

cormat_indices <- rank %>% 
  rename("work cond. ratio" = relative_wc,
         "test scores" = test_scores,
         "dim: transition" = transition_mean,
         "dim: work cond." = working_conditions_mean,
         "dim: education" = education_mean,
         "YLILI" = index_mean) %>% 
  dplyr::select(c(3:15,19)) %>% 
  filter(!is.na(`YLILI`)) %>% 
  as.matrix()

cormat_indices <- cormat_indices %>% 
  cor()

cormat_indices_spearman <- cormat_indices %>% 
  cor(method ="pearson")

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(cormat_indices)

p1 <- ggcorrplot(cormat_indices, type = "upper",
                 outline.col = "white",
                 ggtheme = ggplot2::theme_minimal,
                 colors = c("#000000", "#7E7E7E", "#FFFFFF"),
                 show.diag = TRUE,
                 lab = TRUE,
                 lab_size = 2)

p2 <- ggcorrplot(cormat_indices, type = "upper",
                 outline.col = "white",
                 ggtheme = ggplot2::theme_minimal,
                 colors = c("#000000", "#7E7E7E", "#FFFFFF"),
                 p.mat = p.mat,
                 show.diag = TRUE,
                 sig.level = .05)

grid.arrange(p1, p2)

## ---- fig-indexgdp --------

gdp <- read.csv("../data/raw/gdp_PPP_percap_worldbank.csv") %>%
  rename("country" = Country.Name) %>%
  dplyr::select(country, "gdp"=X2018)

gdp$country <- gdp$country %>%
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

df <- rank %>% left_join(gdp) %>%
  left_join(regions, by = "country") %>% 
  filter(`Region` != "Oceania")

ggplot(df, aes(x = log(gdp), y = index_mean, label = country_code)) +
  geom_point(size=2, aes(color = `Region`)) +
  #ggtitle("Per capita GDP vs YLILI") +
  xlab("log GDP per capita (PPP, current international $)") +
  ylab("YLILI score") +
  theme_minimal() +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  geom_text_repel(aes(label=country_code),size = 3)

## ---- fig-indexunemp --------

unemp_r <- read_csv("../data/raw/unemployment_rate_sex_age_ilostat.csv", col_types = cols()) %>% 
  filter(sex.label == "Sex: Total",
         classif1.label == "Age (Youth, adults): 15-24") %>% 
  group_by(ref_area.label) %>%
  top_n(1, time) %>% dplyr::select(ref_area.label, unemp_r = obs_value)

df <- rank %>% left_join(unemp_r, by = c("country" = "ref_area.label")) %>% 
  left_join(regions, by = "country") %>% 
  filter(`Region` != "Oceania")

ggplot(df, aes(x = unemp_r, y = index_mean, label = country_code)) +
  geom_point(size = 2, aes(color = `Region`)) +
  xlab("Youth unemployment rate") +
  ylab("YLILI score") +
  theme_minimal() +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  geom_text_repel(aes(label=country_code),size = 3)

## ---- fig-spider --------

# generating function

spider <- function(x, region) {
  x <- x %>% 
    filter(region2 == region)
  
  x <- x[,-1]
  
  x <- rbind(rep(100,5) , rep(0,5) , x)
  
  radarchart( x , axistype=6,
              maxmin = TRUE,
              #custom polygon
              pcol="black" , pfcol=gray(.5, .5), plwd=4 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey",
              #custom labels
              vlcex=0.8 ,
              vlabels = addline_format(c("Overall YLILI", "NEET", "Working Conditions Ratio", "Mismatch", "Working Poverty", "Under- employment",  "Informality",  "Elementary", "No Secondary", "Literacy", "Test Scores")),
              title = region)
}

df <- left_join(rank, regions, by = c("country")) %>% 
  filter(!is.na(index_mean))

df <- df %>% 
  mutate(region2 = ifelse(`Region Name` == "Asia", "Asia", `Sub-region Name`))

x <- df %>% 
  dplyr::select(country, region2, "Overall YLILI" = index_mean, "NEET" = neet, "Working\nConditions\nRatio" = relative_wc, "Mismatch" = mismatch, "Working\nPoverty" = workingpov, "Under-\nemployment" = underemp, "Informality" = informal, "Elementary\nOccupations" = elementary, "No\nSecondary\nSchooling" = nosecondary, "Literacy" = literacy, "Test Scores" = test_scores) %>% 
  group_by(region2) %>% 
  summarise_at(vars(-country), ~ mean(., na.rm = TRUE)) %>% 
  as.data.frame()

p1 <- spider(x, "Asia")
p2 <- spider(x, "Eastern Africa")
p3 <- spider(x, "Middle Africa")
p4 <- spider(x, "Southern Africa")
p5 <- spider(x, "Western Africa")

grid.arrange(p1, p2, p3, p4, p5)

## ---- fig-arithgeom --------

df <- rank %>% 
  left_join(regions, by = "country") %>% 
  filter(`Region` != "Oceania")

raw <- rank_generator(impute = FALSE) %>% dplyr::select(country, "index_mean_raw" = index_mean)

df <- left_join(df, raw, by = "country")

ggplot(df, aes(x = index_mean, y = index_mean_raw, label = country_code)) +
  geom_point(aes(color = `Region`)) +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  xlab("YLILI score - imputed data") +
  ylab("YLILI score - raw data") +
  theme_minimal() +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  geom_text_repel(aes(label=country_code),size = 3)

## ---- fig-imputedraw --------

df <- rank %>% 
  left_join(regions, by = "country") %>% 
  filter(`Region` != "Oceania")

raw <- rank_generator(impute = FALSE) %>% dplyr::select(country, "index_mean_raw" = index_mean)

df <- left_join(df, raw, by = "country")

ggplot(df, aes(x = index_mean, y = index_geom, label = country_code)) +
  geom_point(aes(color = `Region`)) +
  geom_smooth(method = "lm", se = F, size=0.5, colour = "black") +
  xlab("YLILI score - arithmetic mean") +
  ylab("YLILI score - geometric mean") +
  theme_minimal() +
  scale_color_viridis_d(option = "inferno", end = 0.7) +
  geom_text_repel(aes(label=country_code),size = 3)



