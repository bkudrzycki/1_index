# README
This folder was made to facilitate replication of the results and PDF of the paper "Youth Labor Index for Low Income Countries".

# Prerequisites
R (preferably RStudio) are necessary to fully reproduce the paper.

# Replication
The paper can be replicated in RStudio in two ways:

  -1 by running "master.R" in the root folder, or
  -2 by knitting the file "index.Rmd" in the /markdown folder.

Either method generates a PDF called "cnb_apprenticeship.pdf" in the /markdown folder. This is the full, replicated paper.

The default path "/Volumes/nadel/research/Data/PhDs/Bart 2022/Paper 3 - CQP" is used in master.R. If the project folder is copied into a different directory, this path name needs to be adjusted.

As mentioned in the file "master.R", certain latex tables needed additional formatting and could not be generated directly in markdown. To replicate these tables, simply run the script "code/index_tables_for_replication.R" in the /code folder.

If you have any questions, please see the comments in "master.R" or contact the author at bartlomiej.kudrzycki[at]nadel.ethz.ch
