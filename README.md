# DTrsiv <img src="img/DTrsiv_hexsticker-ai.png" align="right" height="140" />  

![GitHub repo size](https://img.shields.io/github/repo-size/YoannPa/DTrsiv)
![GitHub issues](https://img.shields.io/github/issues-raw/YoannPa/DTrsiv)
![GitHub closed issues](https://img.shields.io/github/issues-closed-raw/YoannPa/DTrsiv)  

_**DTrsiv** is a R package containing a collection of R data.table functions available to quickly and easily clean your data._  
**Everyone who wants is welcome to contribute!**

**Author: PAGEAUD Y.<sup>1</sup>**  
**Contributors:** Everyone who wants is welcome to contribute!  
**1-** [**DKFZ - Division of Applied Bioinformatics, Germany.**](https://www.dkfz.de/en/applied-bioinformatics/index.php)  

![GitHub R package version](https://img.shields.io/github/r-package/v/YoannPa/DTrsiv?label=Package%20version&logo=RStudio&logoColor=white&style=for-the-badge)  
<img src="https://img.shields.io/static/v1?label=compatibility&message=4.2.0%20(2022-04-22)%20--%20%22Vigorous%20Calisthenics%22&color=blue&logo=R&logoColor=white&style=for-the-badge" />  
![GitHub last commit](https://img.shields.io/github/last-commit/YoannPa/Dtrsiv?logo=git&style=for-the-badge)  
![GitHub](https://img.shields.io/github/license/YoannPa/DTrsiv?color=brightgreen&style=for-the-badge)  

## Prerequisites
Install devtools and data.table packages:  
```R
install.packages(pkgs = c("devtools", "data.table"))
```

## Install
```R
devtools::install_github("YoannPa/DTrsiv")
```

## Content
`dt_fun.R` script contains functions related to R data.table formating:  
* `dt.sub()` for pattern matching and substitution applied on data.table object column-wise. It first identifies the columns containing any occurence matching the pattern and then applies the substitution considering only columns where the pattern matched, thus shortening execution time on data.table with many columns. It supports columns of type list.  
* `dt.ls2c()` converts data.table columns of type list to a type vector.  
* `dt.rm.dup()` removes duplicated columns based on their content (not on their names).  
* `dt.rm.allNA()` removes columns exclusively containing NAs from a data.table.  
* `dt.int64tochar()` converts columns of 'double.integer64' type into 'character' type.  
* `dt.combine()` combines values of partially duplicated columns from a data.table into new columns.  

`dt_chk.R` script contains functions related to checking a R data.table content:  
* `allNA.col()` checks if any column contains exclusively NAs and returns their names if any with a warning.  
* `best.merged.dt()` looks for the best merging operation(s) between two data.tables trying a set of columns from the second one.  

## Problems ? / I need help !
For any questions **Not related to bugs or development** you can write me at [y.pageaud@dkfz.de](y.pageaud@dkfz.de).
 
## Technical questions / Development / Feature request
If you encounters issues or a feature you would expect is not part of DTrsiv functions available, please go to the [DTrsiv Github repository](https://github.com/YoannPa/DTrsiv) click on the tab **Issues** and create an issue.  

## References
1. Introduction to data.table: [https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html](https://cran.r-project.org/web/packages/data.table/vignettes/datatable-intro.html)  
2. Official R data.table Github repository: [https://github.com/Rdatatable/data.table](https://github.com/Rdatatable/data.table)  
3. [_By-Group Processing, the R data.table and the Power of Open Source_ (22.02.2011) - Steve Miller](https://www.information-management.com/opinion/by-group-processing-the-r-datatable-and-the-power-of-open-source)  

