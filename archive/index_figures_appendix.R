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