# Date: Nov, 2024

# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
pacman::p_load("readxl","data.table","purrr","stringr","dplyr")

# ------------------------------------------------------------------------------
# Coalesce join
# ------------------------------------------------------------------------------
coalesce.join <- function(data.list, id, arrange.col = NULL, co.names = NULL, join = dplyr::full_join){
  names <- unlist(lapply(data.list, function(x){colnames(x)}))
  duplicates <- names[which(duplicated(names))][names[which(duplicated(names))] != id] # check duplicates
  
  merged.data <- data.list %>% purrr::reduce(join, by = id)
  
  if (!is_empty(duplicates)){ # coalesce duplicated columns, with suffix ".x", ".y", ".x.x", ".y.y" and so on
    for (i in duplicates){
      suffix <- na.omit(str_extract(colnames(merged.data), paste0("(?<=", i, ")", ".*\\.[a-z]$")))
      co.temp <- unlist(lapply(suffix, function(x){paste0(i, x)}))
        merged.data <- merged.data %>%
          mutate(!!i := coalesce(!!!select(., any_of(co.temp)))) %>%
          select(-any_of(co.temp))
    }
  }
  
  if(!is.null(co.names)){ # further coalesce specific columns passed in co.names
    for (i in colnames(co.names)){
      co.temp <- as.vector(co.names[, i])[!is.na(as.vector(co.names[, i]))]
      print(co.temp)
      skip.to.next <- FALSE
      tryCatch(
      merged.data <- merged.data %>%
        mutate(!!i := coalesce(!!!select(., any_of(co.temp)))),
      error = function(e) {
        cat("ERROR:", conditionMessage(e), "\n")
        skip.to.next <<- TRUE
        }
      )
      if (skip.to.next){next}
      
    }
    # clean up columns that have been coalesced
    co.vector <- as.vector(as.matrix(co.names))[!is.na(as.vector(as.matrix(co.names)))] # all cells of co.names
    co.vector[which(!co.vector %in% colnames(co.names))] # exclude names of co.names (we want to keep those)
    merged_data <- merged_data %>% 
      select(-any_of(co_vector))
  }
  
  if(!is.null(arrange.col)){merged.data <- merged.data %>% arrange(!! rlang::sym(arrange.col))}
  return(merged.data)
}

show.did.plot = function(gdat, x.name, y.name, t.name, vlines, show.means, pos.means = NULL){
  gdat <- gdat %>% mutate(group = if_else(!!sym(t.name) == 0, "Control", "Treatment")) # supports only 1 treatment and 1 control
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
}



  for (i in colnames(co)){
    co_temp <- as.vector(co[, i])[!is.na(as.vector(co[, i]))]
    merged_data <- merged_data %>%
      mutate(!!i := coalesce(!!!select(., any_of(co_temp))))
  }
  
  ncol_before <- ncol(merged_data)
  
  # clean up columns
  co_vector <- as.vector(as.matrix(co))[!is.na(as.vector(as.matrix(co)))]
  merged_data <- merged_data %>% 
    select(-any_of(co_vector)) %>%
    select(cl$identifier, colnames(co), everything())
  
  ncol_after <- ncol(merged_data)
  print(paste("Removed", ncol_before - ncol_after, "columns used in coalesce"))

  # coalesce specified items
  #for (i in colnames(co)){
  #  co_temp <- as.vector(co[, i])[!is.na(as.vector(co[, i]))]
  #  merged_data <- merged_data %>%
  #    mutate(!!i := coalesce(!!!select(., any_of(co_temp))))
  #}
  
 
#co <- data.frame(
#  company_name = c("Name", "Company Name", "EntityName", NA),
#  country = c("Country of Headquarters", "Country","cntry_of_incorporation", "HQ Address Country ISO"),
#  industry = c("Sub_Industry", "Subindustry", NA, NA)
#)
