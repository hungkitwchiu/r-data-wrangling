# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
pacman::p_load("readxl","data.table","purrr","stringr","dplyr","tidyverse")

# ------------------------------------------------------------------------------
# load r data file into an object
# ------------------------------------------------------------------------------
load.RData <- function(fileName){
  #loads an RData file, and returns it
  load(list.files(pattern = fileName, recursive = TRUE))
  get(ls()[ls() != "fileName"])
}

# ------------------------------------------------------------------------------
# Read file using pattern or exact path in directory recursively, default fread
# pattern can be a character vector with multiple elements
# return list if multiple files are read, otherwise return single data.table
# if you want to change default na.strings of fread, consider using formals
# also check column names and print groups if not all same or all different
# if force64 = TRUE, all integer64 columns are coerced into character
# ------------------------------------------------------------------------------

# to do: modify function to accept RData of one or MORE objects

wdread <- function(pattern, func = "fread", bind = TRUE, force64 = FALSE, 
                   forceDate = FALSE){
  if (is_scalar_character(pattern)){ # character string case, find files using pattern
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
  cols.unique.types <- unique(cols.types)
  #print(deparse(substitute(cols.unique.names)))
  
  for (check in list(list(cols.names, cols.unique.names, "names"), 
                     list(cols.types, cols.unique.types, "column types"))){
    if (between(length(check[[2]]), 1, length(cols), incbounds = FALSE)){
      cat("\n", length(check[[2]]), "sets of unique", check[[3]], "among", 
          length(cols), "files found. Showing groups... \n")
      group <- lapply(check[[2]], function(x) lapply(check[[1]], function(y) identical(x,y)))
      print(lapply(group, function(x) files[unlist(x)]))
    }else if(length(check[[2]]) == length(cols) & length(cols) > 1){
      cat("No data sets share common column", check[[3]], ". \n",
          "If seeing different column types, check if force64 = TRUE will help \n")}
  }

  if (force64 == TRUE){
    cat("Forcing int64 columns to be chr \n")
    lapply(1:length(data), function(i){
      int64 <- names(which(cols[[i]] == "integer64"))
      data[[i]] <<- data[[i]] %>%
        mutate_at(int64, ~as.character(.x))
    })
  }

  # use with CAUTION
  if (forceDate == TRUE){
    lapply(1:length(data), function(i){
      date.cols <- names(which(sapply(cols[[i]], function(x){
        any(c("IDate", "Date", "POSIXct", "POSIXt") %in% x)
      })))
      if (length(date.cols > 1)){
        cat("Forcing", date.cols, "to be chr \n")
        data[[i]] <<- data[[i]] %>%
          mutate_at(date.cols, ~as.character(.x))
      }
    })
  }
  
  # bind if names are the same
  if (length(cols.unique.names) == 1 & bind == TRUE){
    tryCatch(
      {data <- rbindlist(data, use.names = TRUE)
      }, error = function(e){message(conditionMessage(e))}
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
# Difference-in-difference functions
# ------------------------------------------------------------------------------

get.SA <- function(yname, tname, idname, gname, data, cluster = NULL){
  formula <- as.formula(paste0(
    yname, " ~ ", "sunab(", gname, ", ", tname, ") | ", idname, " + ", tname))

  SA <- feols(formula, data = data, cluster = cluster)
  return(SA)
}

# did by Callaway and Sant'Ana
get.CSA <- function(yname, tname, idname, gname, data, clustervars = NULL){
  set.seed(123)
  CSA <- att_gt(yname = yname,
             tname= tname,
             idname = idname,
             gname = gname,
             clustervars = clustervars,
             data = data,
             control_group = c("nevertreated", "notyettreated"),
             cores = detectCores()-2)

  return(CSA)
}

get.CD <- function(df, outcome, group, time, treatment, effects, placebo, cluster = NULL){
  CD <- did_multiplegt_dyn(
      df = df,
      outcome = outcome,
      group = group,
      time = time,
      treatment = "treat",
      effects = effects,
      placebo = placebo,
      cluster = cluster,
      graph_off = TRUE
)
  return(CD)
}

get.BJS <- function(yname, tname, idname, gname, data, cluster_var = NULL){
  did_imp <- did_imputation(data = data, yname = yname, gname = gname,
                          tname = tname, idname = idname, 
                          cluster_var = cluster_var,
                          horizon = TRUE, pretrends = TRUE) 
  return(did_imp)
}

get.GAR <- function(yname, tname, idname, reltname, treatment, ref, data, cluster_var = NULL){
  # set up ref of relative year to Inf for event study
  GAR <- did2s(data = data, yname = yname, 
                   first_stage = as.formula(paste0("~ 0 | ", idname, " + ", tname)),
                   second_stage = as.formula(paste0("~ i(", reltname, ", ref =", ref, ")")), 
                   treatment = treatment, cluster_var = cluster_var)
  return(GAR)
}

plot.did <- function(did.list, pre = -5, post = 10, title.alt = NULL){
  if (is.null(title.alt)){title = "Event Time Estimates"
  }else{title = paste("Event Time Estimates -", title.alt)}
  
  # verify did.list
  if (!all(names(did.list) %in% c("CSA", "CD", "SA", "BJS", "GAR"))){
    stop(paste("Check naming of list of did objects \n", 
               "Must be one of CSA, CD, SA, BJS, GAR"))}
  
  coefs <- NULL
  
  # get CSA coefficients
  if (!is.null(did.list[["CSA"]])){
    CSA.plot <- aggte(did.list[["CSA"]], type = "dynamic", na.rm = TRUE) %>%
      tidy() %>%
      rename(t = event.time) %>%
      select(t, estimate, conf.low, conf.high) %>%
      mutate(method = "Callaway & Sant’Anna")
    coefs <- rbind(coefs, CSA.plot)
  }
  
  
  # get CD coefficients
  if (!is.null(did.list[["CD"]])){
    CD.placebos <- did.list[["CD"]][["results"]][["Placebos"]] %>% 
      cbind(t = seq(from = -1, to = -nrow(.), by = -1))
    CD.effects <- did.list[["CD"]][["results"]][["Effects"]] %>% 
      cbind(t = seq(from = 0, to = nrow(.)-1))
    CD.plot <- rbind(CD.placebos, CD.effects) %>%
      as.data.frame() %>%
      rename(c(estimate = Estimate, conf.low = `LB CI`, conf.high = `UB CI`)) %>%
      select(t, estimate, conf.low, conf.high) %>% 
      mutate(method = "Chaisemartin & D'Haultfoeuille") %>%
      arrange(t)
    row.names(CD.plot) <- NULL
    coefs <- rbind(coefs, CD.plot)
  }
  
  # get SA coefficients
  if (!is.null(did.list[["SA"]])){
    SA.plot <- broom::tidy(did.list[["SA"]]) %>%
      mutate(t =  as.double(gsub(".*::", "", term)),
             conf.low = estimate - (qnorm(0.975)*std.error),
             conf.high = estimate + (qnorm(0.975)*std.error)) %>%
      select(t, estimate, conf.low, conf.high) %>%
      mutate(method = "Sun & Abraham")
    coefs <- rbind(coefs, SA.plot)
  }
  
  # get BJS coefficients
  if (!is.null(did.list[["BJS"]])){
    BJS.plot <- did.list[["BJS"]] %>% 
      select(t = term, estimate, std.error) %>%
      mutate(conf.low = estimate - 1.96 * std.error,
             conf.high = estimate + 1.96 * std.error,
             t = as.numeric(t),
             method = "Borusyak") %>% 
      select(c(t, estimate, conf.low, conf.high, method))
    coefs <- rbind(coefs, BJS.plot)
  }
  
  # get GAR coefficients
  if (!is.null(did.list[["GAR"]])){
    GAR.plot <- broom::tidy(did.list[["GAR"]]) %>% 
      mutate(t =  as.double(gsub(".*::", "", term)),
             conf.low = estimate - (qnorm(0.975)*std.error),
             conf.high = estimate + (qnorm(0.975)*std.error)) %>% 
      select(t, estimate, conf.low, conf.high) %>% 
      mutate(method = "Gardner")
    coefs <- rbind(coefs, GAR.plot)
  }
  
  coefs <- coefs %>% filter(!is.na(estimate)) %>%
    filter(t >= pre & t <= post)
  
  plot <- coefs %>% 
    ggplot(aes(x = t, y = estimate, color = method)) + 
    geom_point(aes(x = t, y = estimate), position = position_dodge2(width = 0.8), size = 1) +
    geom_linerange(aes(x = t, ymin = conf.low, ymax = conf.high), position = position_dodge2(width = 0.8), size = 0.75) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = .25, alpha = 0.75) + 
    geom_vline(xintercept = -0.5, linetype = "dashed", linewidth = .25) +
    scale_color_manual(name="Method", values= met.brewer("Cross", 5, "discrete")) +
    theme_calc(base_size = 9) + 
    theme(legend.position='bottom', 
          plot.background = element_rect(fill = "transparent", colour = NA)) +
    labs(title = title, y = "ATT", x = "Relative Time") + 
    guides(col = guide_legend(nrow = ceiling(length(unique(coefs$method))/2))) 
  
  return(plot)
}
