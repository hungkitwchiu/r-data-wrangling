# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
pacman::p_load("readxl","data.table","purrr","stringr","dplyr","tidyverse")

# ------------------------------------------------------------------------------
# Read file using pattern or exact path in directory recursively, default fread
# pattern can be a character vector with multiple elements
# return list if multiple files are read, otherwise return single data.table
# if you want to change default na.strings of fread, consider using formals
# also check column names and print groups if not all same or all different
# if force64 = TRUE, all integer64 columns are coerced into character
# ------------------------------------------------------------------------------
wdread <- function(pattern, func = "fread", bind = TRUE, force64 = FALSE){
  if (is_scalar_character(pattern)){ # character string case
    files <- unlist(lapply(pattern, function(x){list.files(pattern = x, recursive = TRUE)}))
  }else if(is.character(pattern)){files = pattern} # character vector case
  
  data <- lapply(files, function(x){
  cat("Reading...", x, "\n")
  if (str_detect(x, ".RData$")){ get(load(x)) }else{ get(func)(x) }
  })
  
  # return if only one data is read
  if (length(data) == 1){return(data[[1]])}
  
  # get column class
  cols <- lapply(data, function(x){
    temp.col <- lapply(x, class)
    temp.col <- temp.col[order(names(temp.col))]
  })
  
  # get column names and unique names
  cols.names <- lapply(cols, names)
  cols.unique.names <- unique(cols.names)
  # get column types
  cols.types <- lapply(cols, unname)
  
  if (between(length(cols.unique.names), 1, length(cols), incbounds = FALSE)){
    cat("\n", length(cols.unique.names), "sets of column names among", length(cols) , 
        "files found. Showing groups... \n")
    
    group <- lapply(cols.unique.names, function(x) lapply(cols.names, function(y) identical(x,y)))
    print(lapply(group, function(x) files[unlist(x)]))
    
  }else if(length(cols.unique.names) == length(cols) & length(cols) > 1){
    cat("No data sets share common columns. \n")}
  
  if (force64 == TRUE){
    cat("Forcing int64 columns to be chr \n")
    lapply(1:length(data), function(i){
      int64 <- names(which(cols[[i]] == "integer64"))
      data[[i]] <<- data[[i]] %>%
        mutate_at(int64, ~as.character(.x))
    })
  }
  
  # bind if names are the same
  if (length(cols.unique.names) == 1 & bind == TRUE){
    tryCatch(
      {data <- rbindlist(data, use.names = TRUE)
      }, error = function(e){cat(e, "Check column types. Perhaps set force64 = TRUE")}
    )}
  return(data)
}
                 
# ------------------------------------------------------------------------------
# Coalesce join
# ------------------------------------------------------------------------------
coalesce.join <- function(data.list, id, arrange.col = NULL, co.names = NULL, 
                          everything = TRUE, join = dplyr::full_join){
  names <- unlist(lapply(data.list, function(x){colnames(x)}))
  duplicates <- names[which(duplicated(names))][names[which(duplicated(names))] != id]
  
  merged.data <- data.list %>% purrr::reduce(join, by = id)
  
  # coalesce duplicated columns, with suffix ".x", ".y", ".x.x", ".y.y" and so on
  if (!is_empty(duplicates)){ 
    for (i in duplicates){
      suffix <- na.omit(str_extract(colnames(merged.data), paste0("(?<=", i, ")", ".*\\.[a-z]$")))
      co.temp <- unlist(lapply(suffix, function(x){paste0(i, x)}))
      merged.data <- merged.data %>%
        dplyr::mutate(!!i := coalesce(!!!select(., any_of(co.temp)))) %>%
        select(-any_of(co.temp))
    }
  }
  
  if(!is.null(co.names)){ # further coalesce specific columns passed in co.names
    for (i in colnames(co.names)){
      co.temp <- as.vector(co.names[, i])[!is.na(as.vector(co.names[, i]))]
      skip.to.next <- FALSE
      tryCatch(
        merged.data <- merged.data %>%
          dplyr::mutate(!!i := coalesce(!!!select(., any_of(co.temp)))),
        error = function(e) {
          cat("ERROR:", conditionMessage(e), "\n")
          skip.to.next <<- TRUE
        }
      )
      if (skip.to.next){next}
    }
    # clean up columns that have been coalesced
    # all cells of co.names
    co.vector <- as.vector(as.matrix(co.names))[!is.na(as.vector(as.matrix(co.names)))]
    # exclude names of co.names (we want to keep those)
    co.vector <- co.vector[which(!co.vector %in% colnames(co.names))] 
    merged.data <- merged.data %>% 
      select(-any_of(co.vector)) %>%
      select(colnames(co.names), everything())
    if (!everything){merged.data <- merged.data %>% select(colnames(co.names))}
  }
  
  if(!is.null(arrange.col)){merged.data <- merged.data %>% dplyr::arrange(!!rlang::sym(arrange.col))}
  return(merged.data)
}

# ------------------------------------------------------------------------------
# mapview with separate data of interest and shape data
# ------------------------------------------------------------------------------
mapview.with.shape.data <- function(data.interest, data.shape, var.interest, 
                                    link.interest, link.shape, layer.name = NULL,
                                    return = FALSE){
  if (is.null(layer.name)){layer.name = var.interest}
  temp <- data.shape %>%
    right_join(data.interest %>% dplyr::select(eval(link.interest), eval(var.interest)), 
               by = join_by(!!link.shape == !!rlang::sym(link.interest)))
  m <- mapview(temp, zcol = var.interest, layer.name = layer.name)
  if (return){return(m)} else {m}
}

# ------------------------------------------------------------------------------
# did plot, currently with 1 treatment and 1 control
# ------------------------------------------------------------------------------
show.did.plot = function(gdat, x.name, y.name, t.name, vlines, show.means, pos.means = NULL){
  gdat <- gdat %>% dplyr::mutate(group = if_else(!!sym(t.name) == 0, "Control", "Treatment"))
  gg = ggplot(gdat, aes(y = get(y.name), x = get(x.name), color = group)) +
    geom_line() + 
    theme_bw() +
    labs(x = tools::toTitleCase(x.name), y = tools::toTitleCase(y.name), color = "Group")
  
  for (vline in vlines){gg = gg + geom_vline(xintercept = vline)}
  
  if(show.means) {
    exps <- sort(unique(gdat$exp))
    treats <- sort(unique(gdat$treat))
    for (exp in exps){
      for (treat in treats){
        y.label <- mean(unlist(filter(gdat, treat==!!treat, exp==!!exp)[, y.name])) %>% round(3)
        gg = gg + annotate("label", x=pos.means[exp+1], y=y.label*1.01, label=y.label)
      }
    }
  }
  gg
  return(gg)
}
