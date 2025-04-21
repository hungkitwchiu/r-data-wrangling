# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
pacman::p_load("readxl","data.table","purrr","stringr","dplyr","tidyverse")

# ------------------------------------------------------------------------------
# Read file using pattern in directory recursively, default fread
# pattern can be a character vector with multiple elements
# return list if multiple files are read, otherwise return single data.table
# if you want to change default na.strings of fread, consider using formals
# ------------------------------------------------------------------------------
wdread <- function(pattern, func = "fread"){
  files <- unlist(lapply(pattern, function(x){list.files(pattern = x, recursive = TRUE)}))
  data <- lapply(files, function(x){
    cat("Reading...", x, "\n")
    if (func == "load"){get(get(func)(x))}else{get(func)(x)}
  })
  if (length(data) == 1){data = data[[1]]}
  return(data)
}

# ------------------------------------------------------------------------------
# Coalesce join
# ------------------------------------------------------------------------------
coalesce.join <- function(data.list, id, arrange.col = NULL, co.names = NULL, everything = TRUE, join = dplyr::full_join){
  names <- unlist(lapply(data.list, function(x){colnames(x)}))
  duplicates <- names[which(duplicated(names))][names[which(duplicated(names))] != id] # check duplicates
  
  merged.data <- data.list %>% purrr::reduce(join, by = id)
  
  if (!is_empty(duplicates)){ # coalesce duplicated columns, with suffix ".x", ".y", ".x.x", ".y.y" and so on
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
    co.vector <- as.vector(as.matrix(co.names))[!is.na(as.vector(as.matrix(co.names)))] # all cells of co.names
    co.vector <- co.vector[which(!co.vector %in% colnames(co.names))] # exclude names of co.names (we want to keep those)
    merged.data <- merged.data %>% 
      select(-any_of(co.vector)) %>%
      select(colnames(co.names), everything())
    if (!everything){merged.data <- merged.data %>% select(colnames(co.names))}
  }
  
  if(!is.null(arrange.col)){merged.data <- merged.data %>% dplyr::arrange(!!rlang::sym(arrange.col))}
  return(merged.data)
}

# ============================================================================================================
mapview.with.shape.data <- function(data.interest, data.shape, var.interest, link.interest, link.shape){
   temp <- data.shape %>%
     right_join(data.interest %>% select(eval(link.interest), eval(var.interest)), 
                by = join_by(!!link.shape == !!rlang::sym(link.interest)))
   print(mapview(temp, zcol = var.interest, layer.name = var.interest))
 }

# ============================================================================================================
show.did.plot = function(gdat, x.name, y.name, t.name, vlines, show.means, pos.means = NULL){
  gdat <- gdat %>% dplyr::mutate(group = if_else(!!sym(t.name) == 0, "Control", "Treatment")) # supports only 1 treatment and 1 control
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

