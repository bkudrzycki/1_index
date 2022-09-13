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


