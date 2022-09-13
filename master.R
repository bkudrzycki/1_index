## Master Script ##

# set user path: e.g. /Volumes/nadel/research/Data/PhDs/Bart 2022/Paper 3 - CQP

path <- "~/polybox/Youth Employment/1b Index/Paper"

setwd(path)

# to generate the paper in PDF format, open "markdown/index.Rmd" and knit file (shift + cmd + K) or run the command below:
rmarkdown::render("markdown/index.Rmd", envir = new.env())

# Tables 2, 3, 4, B2, B3, B4, B6, and B7 were formatted before being imported into the document. To generate these tables, run the command below:
source("code/index_tables_for_replication.R")
