# r-data-wrangling
Functions that I use across all prrojects, to call, <br />
```r
source("https://raw.githubusercontent.com/hkwilliamchiu/r-data-wrangling/main/functions.R")
```
## Tips on making your code run faster
-  Use `data.table` instead of `data.frame`

-  Use `lapply` instead of for loops (or any kinds of loops)

-  Ues parallelization, in particular, if `lapply` works, `parallel::parLapply` probably also works (but there are overheads associated with utilizing multiple cores, so determining the optimal number of cores to use may not be straight forward, also, look at cores not threads)
