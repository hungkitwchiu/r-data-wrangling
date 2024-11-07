# Date: Nov, 2024

# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
pacman::p_load("readxl","data.table","purrr","stringr","dplyr")

# ------------------------------------------------------------------------------
# Coalesce join, duplicate column names
# ------------------------------------------------------------------------------
coalesce.join <- function(data.list, by = NULL, suffix = c(".x", ".y"), join = dplyr::full_join){
  names <- unlist(lapply(temp, function(x){colnames(x)}))
  duplicates <- names[which(duplicated(names))][names[which(duplicated(names))] != by]
  merged.data <- data.list %>%
    purrr::reduce(join, by = by)

  if(!is.null(by)){merged.data <- merged.data %>% arrange(!! rlang::sym(by))}

  for (i in duplicates){
  co.temp <- unlist(lapply(suffix, function(x){paste0(i, x)}))
  merged.data <- merged.data %>%
    mutate(!!i := coalesce(!!!select(., any_of(co.temp)))) %>%
    select(-any_of(co.temp))
}
  return(merged.data)
}

mapview.with.shape.data <- function(data.interest, data.shape, var.interest, linkage, var.time, time){
  temp <- data.shape %>%
    right_join(data.interest[c(linkage, var.interest, var.time)], by = linkage) %>%
    filter(!!rlang::sym(var.time) == time)
  mapview(temp, zcol = var.interest)
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
        y.label <- mean(unlist(filter(gdat, treat==!!treat, exp==!!exp)[, y.name])) %>% round(1)
        gg = gg + annotate("label", x=pos.means[exp+1], y=y.label*1.01, label=y.label)
      }
    }
  }
  gg
}

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
