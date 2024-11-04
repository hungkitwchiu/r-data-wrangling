# Date: Nov, 2024

# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
pacman::p_load("readxl","data.table","purrr","stringr","dplyr")

# ------------------------------------------------------------------------------
# Coalesce join, duplicate column names
# ------------------------------------------------------------------------------
coalesce_join <- function(data.list, by = NULL, suffix = c(".x", ".y"), join = dplyr::full_join){
  names <- unlist(lapply(temp, function(x){colnames(x)}))
  duplicates <- names[which(duplicated(names))][names[which(duplicated(names))] != by]
  merged.data <- data.list %>%
    purrr:reduce(join, by = by) %>%
    arrange(!! rlang::sym(by))

  for (i in duplicates){
  co.temp <- unlist(lapply(suffix, function(x){paste0(i, x)}))
  merged.data <- merged.data %>%
    mutate(!!i := coalesce(!!!select(., any_of(co.temp)))) %>%
    select(-any_of(co.temp))
}
  return(merged.data)
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
