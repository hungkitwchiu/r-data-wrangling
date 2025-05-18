# r-data-wrangling
Functions that I use across all prrojects, to call, <br />
```r
source("https://raw.githubusercontent.com/hungkitwchiu/r-data-wrangling/main/functions.R")
```
## Faster, Faster, FASTER
-  Use `data.table` instead of `data.frame`

-  Use `lapply` instead of for loops (or any kinds of loops)

-  Use parallelization, in particular, if `lapply` works, `parallel::parLapplyLB` probably also works (be aware of overheads)

-  When parallelizing inside a function, export the necessary variables via `clusterExport`; be aware that each variable exported will add to the overheads

-  Use `distinct`, `unique` and `factor` whenever appropriate, never repeat anything if once is enough
