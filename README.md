# README
This folder was made to facilitate replication of the results and PDF of the paper "Youth Labor Index for Low Income Countries".

# Prerequisites
R (preferably RStudio) are necessary to fully reproduce the paper.

The following default path is used in the scripts below: 

"/Volumes/nadel/research/Data/PhDs/Bart 2022/Paper 3 - CQP"

If data is copied into a different directory, path names have to be adjusted where indicated below.

# Replication

The paper can be replicated by running "master.R" in the root folder, or by knitting the file index.Rmd, found the /markdown folder, in RStudio.

Either method generates a PDF called "cnb_apprenticeship.pdf" in the "markdown" folder. This is the replicated paper.

As mentioned in the file "master.R", certain tables needed additional formatting and could not be generated directly in the markdown file. To replicate these tables, simply run the script "code/index_tables_for_replication.R" in the /code folder.

If you have any questions about replicating this paper, please see comments in the scripts above or contact the author at bartlomiej.kudrzycki[at]nadel.ethz.ch
