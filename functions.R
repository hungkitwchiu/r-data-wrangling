# Date: Nov, 2024

# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
pacman::p_load("readxl","data.table","purrr","stringr","dplyr")

# ------------------------------------------------------------------------------
# Coalesce join, duplicate column names
# ------------------------------------------------------------------------------
coalesce_join <- function(data.list, by = NULL, suffix = c(".x", ".y"), join = dplyr::full_join){
  merge.data <- data.list %>%
    purrr:reduce(join, by = by) %>%
    arrange(by)
}
  # coalesce specified items
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
  
  # save merged data to source file location
  # naming by counting number of copies
  #existing_count <- length(list.files(pattern='^merged_ESG_data.*.csv', recursive = FALSE))
  #if (existing_count == 0){file_name <- "merged_ESG_data.csv"
  #} else if (existing_count == 1) {file_name <- "merged_ESG_data copy.csv"
  #} else {file_name <- paste0("merged_ESG_data copy ", existing_count, ".csv")}
  
  # naming by pasting time
  file_name <- paste0("merged_ESG_data ", format(Sys.time(), "%Y-%m-%d %I.%M%p"), ".csv")
  write.csv(merged_data, file_name, na = "", row.names = FALSE)
  print(paste0("Exported: ", file_name, ". Check source file location"))
  
}

# -------------------------- development --------------------------
cl <- list(
  Bloomberg = 1,
  Refinitiv = 1,
  Sustainalytics = 0,
  SandP = 1,
  identifier = "isin" # ISIN recommended
)

co <- data.frame(
  company_name = c("Name", "Company Name", "EntityName", NA),
  country = c("Country of Headquarters", "Country","cntry_of_incorporation", "HQ Address Country ISO"),
  industry = c("Sub_Industry", "Subindustry", NA, NA)
)

re_ref <- read.csv("Refinitiv_Universe.csv")
re_ref <- re_ref %>%
  mutate(Instrument = gsub("\\..*", "", Instrument))
sus_ref <- read.csv("Sustainalytics data/Sustainalytics Reference data.csv")

unique_ticker <- unique(c(re_ref$Instrument, sus_ref$Ticker))
unique_ticker <- unique_ticker[unique_ticker != ""]
unique_name <- unique(c(re_ref$Company.Common.Name, sus_ref$EntityName))
unique_name <- unique_name[unique_name != ""]
unique_cusip <- unique(sus_ref$CUSIP)
unique_cusip <- unique_cusip[unique_cusip != ""]

table(ISS_company$ticker %in% unique_ticker | ISS_company$Name %in% unique_name | ISS_company$CUSIP %in% unique_cusip)
table(Refinitiv_13F$ticker %in% unique_ticker | Refinitiv_13F$stkname %in% unique_name | Refinitiv_13F$cusip %in% unique_cusip)
table(Refinitiv_S12$ticker %in% unique_ticker | Refinitiv_S12$stkname %in% unique_name | Refinitiv_S12$cusip %in% unique_cusip)

length(unique(c(sus_ref$Ticker, re_ref$Instrument)))
