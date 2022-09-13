packages <- c("kableExtra", "xtable", "data.table", "reshape2", "tidyverse", "lamadex")

options(repos = c(CRAN = "http://cran.rstudio.com"))

# load the lamadex
devtools::install_github("bkudrzycki/youth-lmi/lamadex", quiet = TRUE, upgrade = "always")

# install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], silent = TRUE)
}

# load packages
invisible(lapply(packages, library, character.only = TRUE))

rm(packages, installed_packages)

options(scipen=2, digits=3)

# Table 2: Descriptive statistics, indicators of the YLILI
rank <- rank_generator(bygender = "Total", countries = "dev",  years = c(2010, 2020), impute = TRUE)
rank_no_impute <- rank_generator(bygender = "Total", countries = "dev",  years = c(2010, 2020), impute = FALSE)

x <- rank %>% select(transition_mean, neet, mismatch, relative_wc, relative_wc, working_conditions_mean, workingpov, underemp, informal, elementary, education_mean, nosecondary, literacy, test_scores) %>% 
  summarise(across(where(is.numeric), list(~ mean(., na.rm = T), ~ sd(., na.rm = T), ~ min(., na.rm = T), ~ max(., na.rm = T)))) %>%
  pivot_longer(everything(),
               names_to = c("Indicator", "stat"),
               names_pattern = "(.*)_(.*)",
               values_to = "Value"
  ) %>% 
  pivot_wider(
    names_from = "stat", 
    names_prefix = "stat_", 
    values_from = "Value"
  ) %>% rename("Mean" = stat_1, "Std.Dev" = stat_2, "Min." = stat_3, "Max." = stat_4)

y <- rank_no_impute %>% select(transition_mean, neet, mismatch, relative_wc, relative_wc, working_conditions_mean, workingpov, underemp, informal, elementary, education_mean, nosecondary, literacy, test_scores) %>% 
  summarise(across(where(is.numeric), list(~ sum(!is.na(.))))) %>% 
  pivot_longer(everything(),
               names_to = c("Indicator", "stat"),
               names_pattern = "(.*)_(.*)",
               values_to = "Value"
  ) %>% 
  pivot_wider(
    names_from = "stat", 
    names_prefix = "stat_", 
    values_from = "Value"
  ) %>% rename("Obs." = stat_1)

z <- left_join(x,y, by = "Indicator") %>% mutate(Indicator = recode(Indicator,
                                                                    transition_mean = "Transition",
                                                                    neet = "Share of youth NEET",
                                                                    mismatch = "Youth skills mismatch rate",
                                                                    relative_wc = "Youth relative working conditions ratio",
                                                                    working_conditions_mean = "Working Conditions",
                                                                    workingpov = "Youth working poverty rate",
                                                                    underemp = "Youth TR underemployment rate",
                                                                    informal = "Share of youth in informal employment",
                                                                    elementary = "Share of youth in elementary occup.",
                                                                    education_mean = "Education",
                                                                    nosecondary = "Share of youth with no secondary educ.",
                                                                    literacy = "Youth illiteracy rate",
                                                                    test_scores = "Harmonized test scores")) 

z %>% xtable() %>% kable() %>% kable_classic() %>% kableExtra::add_indent(c(2:4), level_of_indent = 1) %>% kableExtra::add_indent(c(6:9), level_of_indent = 1) %>% kableExtra::add_indent(c(11:13), level_of_indent = 1)


# Table 3: YLILI by country, last available year

rank <- rank_generator(bygender = "Total", countries = "dev",  years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean)) %>% 
  mutate(rank_ylili = rank(-index_mean,na.last = "keep"),
         rank_transition = rank(-transition_mean,na.last = "keep"),
         rank_working_conditions = rank(-working_conditions_mean,na.last = "keep"),
         rank_education = rank(-education_mean,na.last = "keep")) %>% 
  filter(!is.na(index_mean)) %>% 
  select(country, country_code, contains("mean"), contains("rank")) %>% 
  mutate("Mean Rank" = rowMeans(select(., c("rank_transition", "rank_working_conditions", "rank_education")))) %>% 
  rename("Country" = country,
         "Country code" = country_code,
         YLILI = rank_ylili,
         Transition = rank_transition,
         "Work. cond" = rank_working_conditions,
         Education = rank_education,
         "YLILI " = index_mean,
         "Transition " = transition_mean,
         "Work. cond " = working_conditions_mean,
         "Education " = education_mean)

rank[c(1,2,6,3,4,5,7,8,9,10,11)] %>% xtable() %>% kable() %>% kable_classic() %>% add_header_above(c(" " = 2, "Mean Score" = 4, "Rank" = 5))

# Table 4: alternative specifications

raw <- rank_generator(bygender = "Total", countries = "dev",  years = c(2010, 2020), impute = FALSE) %>%
  arrange(desc(index_mean)) %>%
  filter(!is.na(index_mean)) %>% 
  select(country,
         "raw" = index_mean)

rank <- rank_generator(bygender = "Total", countries = "dev",  years = c(2010, 2020), impute = TRUE) %>%
  arrange(desc(index_mean)) %>% 
  mutate(rank = rank(-index_mean,na.last = "keep")) %>% 
  filter(!is.na(index_mean))

#remove one indicator at a time, recompile

#transition dimension

new_trans <- function (x, i) {
  varname <- paste0("no_", i)
  x %>% select(-i) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:4]))<2, rowMeans(.[3:4], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[5:8]))<3, rowMeans(.[5:8], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[9:11]))<2, rowMeans(.[9:11], na.rm = TRUE),NA)) %>% 
    mutate(!!varname := ifelse(rowSums(is.na(.[12:14]))==0, rowMeans(.[12:14], na.rm = TRUE),NA)) %>% 
    select(c(!!varname))
}

df <- cbind(rank, new_trans(rank, "neet"))
df <- cbind(df, new_trans(rank, "relative_wc"))
df <- cbind(df, new_trans(rank, "mismatch"))

new_wc <- function (x, i) {
  varname <- paste0("no_", i)
  x %>% select(-i) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:5]))<2, rowMeans(.[3:5], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[6:8]))<3, rowMeans(.[6:8], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[9:11]))<2, rowMeans(.[9:11], na.rm = TRUE),NA)) %>% 
    mutate(!!varname := ifelse(rowSums(is.na(.[12:14]))==0, rowMeans(.[12:14], na.rm = TRUE),NA)) %>%
    select(c(!!varname))
}

df <- cbind(df, new_trans(rank, "workingpov"))
df <- cbind(df, new_trans(rank, "underemp"))
df <- cbind(df, new_trans(rank, "informal"))
df <- cbind(df, new_trans(rank, "elementary"))

new_educ <- function (x, i) {
  varname <- paste0("no_", i)
  x %>% select(-i) %>%
    mutate(transition_mean = ifelse(rowSums(is.na(.[3:5]))<2, rowMeans(.[3:5], na.rm = TRUE),NA)) %>%
    mutate(working_conditions_mean = ifelse(rowSums(is.na(.[6:9]))<3, rowMeans(.[6:9], na.rm = TRUE),NA)) %>%
    mutate(education_mean = ifelse(rowSums(is.na(.[10:11]))<2, rowMeans(.[10:11], na.rm = TRUE),NA)) %>% 
    mutate(!!varname := ifelse(rowSums(is.na(.[12:14]))==0, rowMeans(.[12:14], na.rm = TRUE),NA)) %>% 
    select(c(!!varname))
}

df <- cbind(df, new_educ(rank, "nosecondary"))
df <- cbind(df, new_educ(rank, "literacy"))
df <- cbind(df, new_educ(rank, "test_scores"))

df <- df %>% select(-index_geom, index_geom)
df <- left_join(df, raw, by = "country")

#score correlations

scores <- df %>% 
  select(c("country", "index_mean", starts_with("no_"), "index_geom", "raw"))

pearson <- cor(as.matrix(scores[,-1]))[,1]
spearman <- cor(as.matrix(scores[,-1]), method = "spearman")[,1]

score_diffs <- abs(scores[2:ncol(scores)]-scores[,2]) %>%
  summarise_if(is.numeric, mean)

score_sd <- scores[2:ncol(scores)]-scores[,2]

score_sd <- score_sd %>% 
  summarise_if(is.numeric, sd)

##differences in rankings
to_rank <- function(x) {rank(-x)}

ranks <- df %>% 
  select(c("country", "index_mean", starts_with("no_"), "index_geom", "raw")) %>% 
  mutate_if(is.numeric, to_rank)

rank_diffs <- abs(ranks[2:ncol(ranks)]-ranks[,2]) %>% 
  summarise_if(is.numeric, mean)

max_rank_diffs <- abs(ranks[2:ncol(ranks)]-ranks[,2]) %>%
  summarise_if(is.numeric, max)

table <- rbind(pearson, score_sd, spearman, rank_diffs, max_rank_diffs)[,-1] %>% rename(geometric = "index_geom")

rownames(table) <- c("Pearson's $r$", "Std dev. of score diff.", "Spearman's $\\rho$", "Mean rank diff.", "Max rank diff.")

table %>% xtable() %>% kable() %>% kable_classic()

# Table 6: Number of available indicators by year

dfList <- compute_indicators()

compress <- function (x) {
  x %>%
    filter(ref_area.label %in% countryLists()[[3]][[1]],
           sex.label == "Sex: Total",
           !is.na(obs_value)) %>%
    summarise(time, ref_area.label) %>% 
    mutate(dummy = 1,) %>% 
    acast(ref_area.label ~ time, value.var = "dummy") %>% 
    t() %>% as.data.frame() %>% rownames_to_column(var ="Year") %>% 
    rename("Cote d'Ivoire" = "Côte d'Ivoire")
}

years_list <- lapply(dfList, compress) %>% 
  rbindlist(fill = TRUE) %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  t() %>% as.data.frame()

names(years_list) = years_list[1,]
years_list = years_list[-1,]

df <- years_list %>% count("num" = years_list$`2000`) %>% 
  rename("2000" = n) %>% 
  full_join(years_list %>% count("num" = years_list$`2001`) %>% rename("2001" = n), by = "num") %>% 
  full_join(years_list %>% count("num" = years_list$`2002`) %>% rename("2002" = n), by = "num") %>% 
  full_join(years_list %>% count("num" = years_list$`2003`) %>% rename("2003" = n), by = "num") %>% 
  full_join(years_list %>% count("num" = years_list$`2004`) %>% rename("2004" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2005`) %>% rename("2005" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2006`) %>% rename("2006" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2007`) %>% rename("2007" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2008`) %>% rename("2008" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2009`) %>% rename("2009" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2010`) %>% rename("2010" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2011`) %>% rename("2011" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2012`) %>% rename("2012" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2013`) %>% rename("2013" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2014`) %>% rename("2014" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2015`) %>% rename("2015" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2016`) %>% rename("2016" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2017`) %>% rename("2017" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2018`) %>% rename("2018" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2019`) %>% rename("2019" = n), by = "num") %>%
  full_join(years_list %>% count("num" = years_list$`2020`) %>% rename("2020" = n), by = "num") %>% 
  t() %>% as.data.frame()

names(df) = df[1,]
df = df[-1,] %>% rownames_to_column(var ="Year") %>% arrange(desc(Year)) %>% mutate(`10` = NA)
df[is.na(df)] <- 0
df <- mutate_all(df, function(x) as.numeric(as.character(x))) %>% mutate("Total number of countries" = rowSums(.[2:11]))

df %>% xtable() %>% kable(caption = "<center>Number of available indicators</center>") %>% kable_classic() %>% column_spec(13, width = "6em")


# Table 7: Coverage of indicators (%) by year, last available year
options(scipen=2, digits=3)

compress2 <- function (x, years) {
  varname <- deparse(substitute(x))
  x <- as_tibble(x) %>%
    filter(ref_area.label %in% countryLists()[[3]][[1]],
           sex.label == "Sex: Total",
           between(time, years[1], years[2]),
           !is.na(obs_value)) %>%
    group_by(ref_area.label) %>%
    top_n(1, time) %>% ## include only a single observation per country per year
    dplyr::select(ref_area.label, time)
}

last_year <- lapply(dfList, compress2, years = c(2010, 2020)) %>%
  reduce(full_join, by = "ref_area.label", accumulate == TRUE) %>%
  filter(ref_area.label %in% country_list[[1]]) %>% as.data.frame()

colnames(last_year) <- c("ref_area.label", "neet", "relative_wc", "mismatch", "workingpov", "underemp", "informal", "elementary", "nosecondary", "literacy", "test_scores")

x <- as.data.frame(table(last_year$neet)) %>% 
  left_join(as.data.frame(table(last_year$relative_wc)), by = "Var1") %>% 
  left_join(as.data.frame(table(last_year$mismatch)), by = "Var1") %>% 
  left_join(as.data.frame(table(last_year$workingpov)), by = "Var1") %>% 
  left_join(as.data.frame(table(last_year$underemp)), by = "Var1") %>% 
  left_join(as.data.frame(table(last_year$informal)), by = "Var1") %>% 
  left_join(as.data.frame(table(last_year$elementary)), by = "Var1") %>% 
  left_join(as.data.frame(table(last_year$nosecondary)), by = "Var1") %>% 
  left_join(as.data.frame(table(last_year$literacy)), by = "Var1") %>% 
  left_join(as.data.frame(table(last_year$test_scores)), by = "Var1")

colnames(x) <- c("year", "neet", "relative_wc", "mismatch", "workingpov", "underemp", "informal", "elementary", "nosecondary", "literacy", "test_scores")
x[is.na(x)] <- 0
x[, 2] <- x[, 2]/sum(!is.na(last_year$neet))*100
x[, 3] <- x[, 3]/sum(!is.na(last_year$relative_wc))*100
x[, 4] <-x[, 4]/sum(!is.na(last_year$mismatch))*100
x[, 5] <-x[, 5]/sum(!is.na(last_year$workingpov))*100
x[, 6] <-x[, 6]/sum(!is.na(last_year$underemp))*100
x[, 7] <-x[, 7]/sum(!is.na(last_year$informal))*100
x[, 8] <-x[, 8]/sum(!is.na(last_year$elementary))*100
x[, 9] <-x[, 9]/sum(!is.na(last_year$nosecondary))*100
x[, 10] <-x[, 10]/sum(!is.na(last_year$literacy))*100
x[, 11] <-x[, 11]/sum(!is.na(last_year$test_scores))*100

x %>% kable() %>% kable_classic()

# Table 8: Availability of indicators, last available year

last_year2 <- lapply(dfList, compress2, years = c(2010, 2020)) %>%
  reduce(full_join, by = "ref_area.label", accumulate == TRUE) %>%
  filter(ref_area.label %in% country_list[[1]]) %>% as.data.frame()

colnames(last_year2) <- c("Country Name", "neet", "relative_wc", "mismatch", "workingpov", "underemp", "informal", "elementary", "nosecondary", "literacy", "test_scores")

last_year2[,2:11][!is.na(last_year2[,2:11])] <- 1
last_year2[is.na(last_year2)] <- 0

last_year2 <- last_year2 %>% mutate("Transition (# out of 3)" = rowSums(.[2:4]),
                                    "Working cond. (# out of 4)" = rowSums(.[5:8],),
                                    "Education (# out of 3)" = rowSums(.[9:11]),
                                    "Total" = rowSums(.[2:11]),
                                    "YLILI Computed?" = ifelse(rowSums(.[2:4]) >= 2 & rowSums(.[5:8]) >= 3 & rowSums(.[9:11]) >= 2, "YES", "NO")) %>% 
  select("Country Name", "Transition (# out of 3)", "Working cond. (# out of 4)", "Education (# out of 3)", "Total", "YLILI Computed?") %>% arrange(desc(Total))

last_year2 %>% kable() %>% kable_classic() %>% column_spec(1, width = "15em") %>% column_spec(2, width = "4em") %>% column_spec(3, width = "4em") %>% column_spec(4, width = "4em") %>% column_spec(5, width = "4em") %>%  column_spec(6, width = "4em")



# Table 10: YLILI by country and gender, last available year

male <- rank_generator(bygender = "Male", years = c(2010, 2020), impute = TRUE) %>% arrange(desc(index_mean)) %>% 
  mutate(rank_male = rank(-index_mean,na.last = "keep"))

female <- rank_generator(bygender = "Female", years = c(2010, 2020), impute = TRUE) %>% 
  mutate(rank_female = rank(-index_mean,na.last = "keep")) %>% 
  select(country, index_mean, rank_female)

rank <- left_join(male, female, by = "country") %>% mutate(delta = index_mean.x-index_mean.y)
rank <- rank[c(1,2,19,22,24,21,23)] %>% 
  rename("Country" = country,
         "Country code" = country_code,
         Male = index_mean.x,
         Female = index_mean.y,
         "$\\Delta$" = delta,
         "Rank (Male)" = rank_male,
         "Rank (Female)" = rank_female)

rank %>% xtable() %>% kable() %>% kable_classic()

# Table 11: YLILI by country, dimension and gender, last available year

rank <- rank_generator(bygender = "Total", years = c(2010, 2020), impute = TRUE) %>% arrange(desc(index_mean)) %>% select("country")

male <- rank_generator(bygender = "Male", years = c(2010, 2020), impute = TRUE) %>% arrange(desc(index_mean)) %>% 
  mutate(trans_male = transition_mean,
         wc_male = working_conditions_mean,
         educ_male = education_mean,
         rank_trans_male = rank(-transition_mean,na.last = "keep"),
         rank_wc_male = rank(-working_conditions_mean,na.last = "keep"),
         rank_educ_male = rank(-education_mean,na.last = "keep"))

female <- rank_generator(bygender = "Female", years = c(2010, 2020), impute = TRUE) %>% 
  mutate(trans_female = transition_mean,
         wc_female = working_conditions_mean,
         educ_female = education_mean,
         rank_trans_female = rank(-transition_mean,na.last = "keep"),
         rank_wc_female = rank(-working_conditions_mean,na.last = "keep"),
         rank_educ_female = rank(-education_mean,na.last = "keep")) %>% 
  select(country, index_mean, trans_female, wc_female, educ_female, rank_trans_female, rank_wc_female, rank_educ_female)

rank <- rank %>% left_join(male, by = "country") %>% left_join(female, by = "country") %>% filter(!is.na(trans_male) | !is.na(trans_female) | !is.na(wc_male) | !is.na(wc_female) | !is.na(educ_male) | !is.na(educ_female)) %>% mutate(trans_delta = trans_male-trans_female,
                                                                                                                                                                                                                                        wc_delta = wc_male-wc_female,
                                                                                                                                                                                                                                        educ_delta = educ_male-educ_female,
                                                                                                                                                                                                                                        trans_male = paste0(round(trans_male,2), " (", rank_trans_male, ")"),
                                                                                                                                                                                                                                        trans_female = paste0(round(trans_female,2), " (", rank_trans_female, ")"),
                                                                                                                                                                                                                                        wc_male = paste0(round(wc_male,2), " (", rank_wc_male, ")"),
                                                                                                                                                                                                                                        wc_female = paste0(round(wc_female,2), " (", rank_wc_female, ")"),
                                                                                                                                                                                                                                        educ_male = paste0(round(educ_male,2), " (", rank_educ_male, ")"),
                                                                                                                                                                                                                                        educ_female = paste0(round(educ_female,2), " (", rank_educ_female, ")"))

rank$country <- rank$country %>% 
  recode("Moldova, Republic of" = "Moldova",
         "Occupied Palestinian Territory" = "Occ. Palestine",
         "Lao People's Democratic Republic" = "Lao PDR",
         "Congo, Democratic Republic of the" = "Congo DR",
         "Tanzania, United Republic of" = "Tanzania",
         "Micronesia, Federated States of" = "Micronesia")

rank2 <- rank[c(1,2,21,28,34,22,29,35,23,30,36)] %>% rename("Country" = country,
                                                            " " = country_code,
                                                            "M" = trans_male,
                                                            "F" = trans_female,
                                                            "$\\Delta$" = trans_delta,
                                                            " M " = wc_male,
                                                            " F " = wc_female,
                                                            " $\\Delta$ " = wc_delta,
                                                            "  M  " = educ_male,
                                                            "  F  " = educ_female,
                                                            "  $\\Delta$  " = educ_delta)


kbl(rank2, align = 'l', booktabs = T, longtable = T, linesep = "", escape = F, digits = 2) %>%
  add_header_above(c(" " = 2,"Transition" = 3, "Working conditions" = 3, "Education" = 3)) %>% 
  footnote(general = "\\\\textit{Notes:} Gender-specific ranks shown in parentheses.",
           threeparttable = T,
           escape = F,
           fixed_small_size = T,
           general_title = "") %>% kable_classic()
