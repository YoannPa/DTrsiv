# DTrsiv
## Collection of R data.table functions to easily clean data   

### Install

```bash
git clone https://github.com/YoannPa/DTrsiv.git
```

Go to the directory and and open the file 'DTrsiv.Rproj' in RStudio.  
Install devtools and data.table packages:  
```R
install.packages(pkgs = c("devtools", "data.table"))
```

Install DTrsiv:  
```R
devtools::install()
```

Load data.table and DTrsiv packages:
```R
library(DTrsiv)
```

The DTrsiv functions should be available in your environment.

### Content
`dt_fun.R` is a script containing functions related to R data.table:  
* `dt.sub()` for pattern matching and substitution applied on data.table object column-wise. It first identifies the columns containing any occurence matching the pattern and then applies the substitution considering only columns where the pattern matched, thus shortening execution time on data.table with many columns. It supports columns of type list.  
* `dt.ls2c()` converts data.table columns of type list to a type vector.  
* `dt.rm.dup()` removes duplicated columns based on their content (not on their names).  
* `dt.int64tochar()` converts columns of 'double.integer64' type into 'character' type.  
* `dt.combine()` combines information using duplicated colnames of a data.table resulting from merge().  

### Problems ? / I need help !
For any questions **not related to bugs or development** you can write me at [y.pageaud@dkfz.de](y.pageaud@dkfz.de).
 
### Technical question / Development / Feature request
If you encounters issues or a feature you would expect is not part of DTrsiv functions available, please go to the [DTrsiv Github repository](https://github.com/YoannPa/DTrsiv) click on the tab **Issues** and create an issue.  

