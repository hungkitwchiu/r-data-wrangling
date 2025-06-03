# r-data-wrangling
Functions that I use across all prrojects, to call, <br />
```r
source("https://raw.githubusercontent.com/hungkitwchiu/r-data-wrangling/main/functions.R")
```
## Write faster code
-  Both Python and R are primarily single-core language, shop for CPUs that have strong single core performance

-  Use `data.table` instead of `data.frame`

-  Use `lapply` instead of for loops (or any kinds of loops)

-  Use parallelization, for example, if `lapply` works, `parallel::parLapplyLB` probably also works (be aware of overheads)

-  When parallelizing inside a function, export the necessary variables via `clusterExport`; be aware that each variable exported will add to the overheads

-  Use `distinct`, `unique` and `factor` whenever appropriate, never repeat anything if once is enough

-  Use `Rcpp` to embed `C++` code

## Parallelization

| Package/Approach         | Syntax Style     | Backend           | Notes                       |
| ------------------------ | ---------------- | ----------------- | --------------------------- |
| `furrr`                  | `map()` style    | `future`          | Best for tidyverse users    |
| `foreach` + `doParallel` | for-loop style   | `parallel`/`snow` | Good control, verbose       |
| `parallel::parLapply`    | `lapply()` style | `parallel`        | Built into base R           |
| `future.apply`           | `apply()` style  | `future`          | Drop-in for `apply()` funcs |


-  `furrr`: built on top of future, supports multisession, multicore, clusters, etc.

```{r}
library(furrr)
plan(multisession)  # use multisession on Windows
result <- future_map(my_list, slow_function)
```

-  `parallel::parLapply`: base `R` only, no other dependencies

```{r}
cl <- makeCluster(4)
result <- parLapply(cl, my_list, slow_function)
stopCluster(cl)
```
