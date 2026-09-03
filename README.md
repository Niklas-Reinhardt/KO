README for the Replication Code for "Educating All Retail Investors in a Billion-Euro Market" by Alexander Klos and Niklas Reinhardt (as of August 2026)

Link to the paper:
https://papers.ssrn.com/sol3/papers.cfm?abstract_id=7345658

How to replicate the results:
Running the R scripts in "Code" sequentially from 01 through 10 replicates the figures, tables, and numbers in our paper.
The figures and tables are stored in "Output".
Individual numbers (such as the back-of-the-envelope calculation of the retail losses saved due to the intervention) are printed to the console.
The code contains relative file paths, which can be used when the scripts are executed through "KO.Rproj".

How to obtain the data:
Data are not provided in this repository.
Data for Frankfurt and Gettex are downloaded by scripts 01 through 03.
Data for SIX and BSW can be downloaded from the websites linked in scripts 06 and 08, respectively.
In case of incomplete downloads, scripts 02 and 03 can be executed repeatedly to complete the downloads.
Note that script 03 also attempts to download data for holidays, for which "No data available." is printed to the console.

Main results and additional results:
To replicate our main results, only scripts 01 through 07 have to be run and it is not necessary to download the BSW data.

R version: 4.6.1

Required R packages (package versions as of August 26, 2026 are controlled using groundhog):
"groundhog", "httr2", "stringr", "curl", "data.table", "lubridate", "zoo", "readxl", "ggplot2", "gridExtra", "sandwich", "lmtest", "kableExtra", "pdftools"

License:
The code in this repository is licensed under the MIT License. 
The underlying data are subject to the terms and conditions of the respective data providers and applicable statutory rights and restrictions. 
Users are responsible for obtaining and using the data in accordance with the applicable terms and conditions.
