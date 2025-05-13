new_extract_fun <- function(df, survey_id_c, variable_recode) {
  
  message(survey_id_c)
  
  if(!survey_id_c %in% variable_recode$survey_id) {
    stop(survey_id_c, ": No survey found in recode sheet")
  }
  
  
  surv_type <- str_sub(survey_id_c, 8, -5)
  
  # if(str_detect(survey_id_c, "PLACE")) {
  #   
  #   # surv_type <- "PLACE"
  #   
  #   custom_recode <- filter(variable_recode, survey_id == str_sub(survey_id_c, end = -5))
  #   
  #   variable_df <- variable_recode %>%
  #     
  #     distinct(variable) %>%
  #     left_join(custom_recode, by="variable") %>% 
  #     filter(!is.na(var_raw))
  #   
  #   
  #   opt_var <- filter(variable_df)$var_raw %>% setNames(filter(variable_df)$variable)
  #   
  #   
  #   df <- df %>% 
  #     select(any_of(opt_var))
  #   
  # }
  # else if (surv_type == "BBS" | surv_type == "ACA")
  # {
    custom_recode <- filter(variable_recode, survey_id == survey_id_c)
    
    variable_df <- variable_recode %>%
      
      distinct(variable) %>%
      left_join(custom_recode, by="variable") %>% 
      filter(!is.na(var_raw))
    
    
    opt_var <- filter(variable_df)$var_raw %>% setNames(filter(variable_df)$variable)
    
    
    df <- df %>% 
      mutate(survey_id = survey_id_c) %>%
      select(survey_id, any_of(opt_var)) %>% 
      ungroup() %>%
      mutate(subject_id = row_number()) %>% 
      select(-any_of("old_subject_id")) # this is a bodge to deal with NA's in subject_id # deal with that later
    
    
  # }
}

#### haven::zap_labels()

new_val_recode <- function(col, col_name, survey_id_c) {
  
  # surv_type <- str_sub(survey_id_c, 8, -5)
  # if(str_detect(survey_id_c, "PLACE")) {
  #   value_recode <- value_recode %>%
  #     filter(variable == col_name,
  #            survey_id == str_sub(survey_id_c, end = -5))
  #   
  #   if(!length(value_recode$value[!value_recode$value %in% c(0:9, NA)]))
  #     value_recode$value <- as.numeric(value_recode$value)
  #   
  #   vec <- value_recode$value
  #   names(vec) <- value_recode$val_raw
  #   
  #   recode(col, !!!vec)
  # }
  # 
  # else if (surv_type == "BBS" | surv_type == "ACA") {
  #   
    value_recode <- value_recode %>%
      filter(variable == col_name,
             survey_id == survey_id_c)
    
    if(!length(value_recode$value[!value_recode$value %in% c(0:9, NA)]))
      value_recode$value <- as.numeric(value_recode$value)
    
    vec <- value_recode$value
    names(vec) <- value_recode$val_raw
    
    unchanged_vec <- unique(col[!col %in% names(vec)])
    uv_no_na <-unchanged_vec[!is.na(unchanged_vec)]
    names(uv_no_na) <- uv_no_na
    
    
    # unchanged_vec_names <- unchanged_vec
    # unchanged_vec_names[is.na(unchanged_vec_names)] <- "none"
    # names(unchanged_vec) <- unchanged_vec_names
    
    vec <- c(vec, uv_no_na)
    
    recode(col, !!!vec)
    
  }


new_recode_survey_variables <- function(df, survey_id_c, value_recode) {
  
  message(survey_id_c)
  
  surv_type <- str_sub(survey_id_c, 8, -5)
  
  recode_columns <- unique(filter(value_recode, survey_id == survey_id_c)$variable)
  
  df <- haven::zap_labels(df)
    
  df <- df %>%
    mutate(
      # across(everything(), haven::zap_labels(df)),
      across(any_of(recode_columns), ~new_val_recode(.x, cur_column(), survey_id_c)),
      survey_id = survey_id_c 
    ) %>%
    type.convert(as.is = TRUE) %>%
    select(survey_id, everything())
  
    
}

all_recoded$BDI2021BBS_FSW

remove_implausible_values <- function(df) {
  
}

## Cannot be aged above 80
## Cannot have started risk behaviour under 10 or over 80
age_risk_10to80 <- function(df, cols) {
  df %>%
    mutate(across(any_of(cols), ~ifelse(.x %in% 10:80, .x, NA)))
}

## Risk duration can't be more than 10 fewer than age (a 40 year old cannot have a risk duration longer than 30)
duration_10to80 <- function(df, cols) {
  if("age" %in% colnames(df))
    df %>%
      mutate(across(any_of(cols), ~ifelse(.x > age - 10, NA, .x)))
  else
    df
}

# Cleaning data!
cleaning_fun2 <- function(df, survey_id_c){

  message(survey_id_c)


  if ("age" %in% colnames(df)){
    df <- df %>%
      mutate(age = ifelse(age <80, age, 999),
             age_changed = ifelse(age == 999, 1, 0),
             age = ifelse(age == 999, NA_integer_, age)) %>%
      type.convert(as.is = T)

    if ("duration_yr" %in% colnames(df)){
      df <- df %>%
        mutate(duration_yr = ifelse(duration_yr < 80, duration_yr, 999),
               duration_yr = ifelse(duration_yr > age - 10, 999, duration_yr),
               duration_yr_changed = ifelse(duration_yr == 999, 1, 0),
               duration_yr = ifelse(duration_yr == 999, NA_integer_, duration_yr)
        )
    }
    else{
      df <- df
    }

    if ("age_fs_paid" %in% colnames(df)){
      df <- df %>%
        mutate(age_fs_paid = ifelse(age_fs_paid < 80, age_fs_paid, 999),
               age_fs_paid = ifelse(age_fs_paid < 10, 999, age_fs_paid),
               age_fs_paid_changed = ifelse(age_fs_paid == 999, 1, 0),
               age_fs_paid = ifelse(age_fs_paid == 999, NA_integer_, age_fs_paid)
        )
    }
    else{
      df <- df
    }

    if ("age_fs_paidorgift" %in% colnames(df)){
      df <- df %>%
        mutate(age_fs_paidorgift = ifelse(age_fs_paidorgift < 80, age_fs_paidorgift, 999),
               age_fs_paidorgift = ifelse(age_fs_paidorgift < 10, 999, age_fs_paidorgift),
               age_fs_paidorgift_changed = ifelse(age_fs_paidorgift == 999, 1, 0),
               age_fs_paidorgift = ifelse(age_fs_paidorgift == 999, NA_integer_, age_fs_paidorgift)
        )
    }
    else{
      df <- df
    }

    if ("age_fs_paidfor" %in% colnames(df)){
      df <- df %>%
        mutate(age_fs_paidfor = ifelse(age_fs_paidfor < 80, age_fs_paidfor, 999),
               age_fs_paidfor = ifelse(age_fs_paidfor < 10, 999, age_fs_paidfor),
               age_fs_paidfor_changed = ifelse(age_fs_paidfor == 999, 1, 0),
               age_fs_paidfor = ifelse(age_fs_paidfor == 999, NA_integer_, age_fs_paidfor)
        )
    }
    else{
      df <- df
    }


    if ("age_startsw" %in% colnames(df)){
      df <- df %>%
        mutate(age_startsw = ifelse(age_startsw < 80, age_startsw, 999),
               age_startsw = ifelse(age_startsw < 10, 999, age_startsw),
               age_startsw_changed = ifelse(age_startsw == 999, 1, 0),
               age_startsw = ifelse(age_startsw == 999, NA_integer_, age_startsw)
        )
    }
    else{
      df <- df
    }


    if ("age_fs_man" %in% colnames(df)){
      df <- df %>%
        mutate(age_fs_man = ifelse(age_fs_man < 80, age_fs_man, 999),
               age_fs_man = ifelse(age_fs_man < 10, 999, age_fs_man),
               age_fs_man_changed = ifelse(age_fs_man == 999, 1, 0),
               age_fs_man = ifelse(age_fs_man == 999, NA_integer_, age_fs_man)
        )
    } else{
      df <- df
    }


    if ("age_fs_man_anal" %in% colnames(df)){
      df <- df %>%
        mutate(age_fs_man_anal = ifelse(age_fs_man_anal < 80, age_fs_man_anal, 999),
               age_fs_man_anal = ifelse(age_fs_man_anal < 10, 999, age_fs_man_anal),
               age_fs_man_anal_changed = ifelse(age_fs_man_anal == 999, 1, 0),
               age_fs_man_anal = ifelse(age_fs_man_anal == 999, NA_integer_, age_fs_man_anal)
        )
    } else{
      df <- df
    }

    if ("age_inject" %in% colnames(df)){
      df <- df %>%
        mutate(age_inject = ifelse(age_inject < 80, age_inject, 999),
               age_inject = ifelse(age_inject < 10, 999, age_inject),
               age_inject_changed = ifelse(age_inject == 999, 1, 0),
               age_inject = ifelse(age_inject == 999, NA_integer_, age_inject)
        )
    } else{
      df <- df
    }

    if ("inject_yr" %in% colnames(df)){
      df <- df %>%
        mutate(inject_yr = ifelse(inject_yr < 80, inject_yr, 999),
               inject_yr = ifelse(inject_yr > age - 10, 999, inject_yr),
               inject_yr_changed = ifelse(inject_yr == 999, 1, 0),
               inject_yr = ifelse(inject_yr == 999, NA_integer_, inject_yr)
        )
    }
    else{
      df <- df
    }

  } else {
    df <- df
  }


}

rds_adjust_new <- function(df, outcome_var, grouping_vars) {
  
  grouping_vars <- "age_group"
  outcome_var <- "hiv"
  
  message(unique(df$survey_id))
  
  stopifnot("network_size" %in% colnames(df))
  
  ## Let's check with CDC what they do for this problem.    
  median_network_size <- median(df$network_size[df$network_size != 0], na.rm = T)
  df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
  nboot <- 30

      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      # vars <- "duration1"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='unique_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='unique_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      grouped_list <- df %>%
        group_by(across(all_of(grouping_vars))) %>%
        group_split()
      
      rds_df <- lapply(grouped_list, function(x) {
        
        class(x) <- 'rds.data.frame'
        
        debugonce(RDS.bootstrap.intervals)
        freq <- RDS.bootstrap.intervals(df, outcome.variable="hiv",
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        # subset = "age_group",
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
    
  }



### Slimmed down rds weighting --> will update this to include the code that breaks down things like age to individual levles.
## will also roll into new_recode_survey_variables I think
rds_adjust <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
  
  message(survey_id_c)
  
  surv_type <- str_sub(survey_id_c, 8, -5)
  
  if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "duration_yr" %in% colnames(df) ) {
  
    df <- df %>% 
      mutate(duration1 = factor(duration_yr)) %>% 
      filter(!is.na(coupon1),
             coupon1 != "")
    
    median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
    
    df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
    
    nboot <- 30
    
    # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
    # vars <- "age1"
    vars <- "duration1"
    coupons <- colnames(df)[grep("^coupon", colnames(df))]
    
    num_coupons <- length(coupons)
    
    df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                        subject.coupon='own_coupon', 
                                        # coupon.variables=c("coupon1","coupon2","coupon3"),
                                        coupon.variables = coupons)
    
    df <- as.rds.data.frame(df, id='subject_id', 
                            recruiter.id='recruiter.id',
                            network.size='network_size',
                            population.size=c(NA,NA,NA), 
                            # max.coupons=3, 
                            max.coupons = num_coupons,
                            notes=NULL)
    
    df$seed <- get.seed.id(df)
    df$wave <- get.wave(df)
    
    
    df <- lapply(vars, function(x) {
      
      freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                      weight.type="RDS-II", uncertainty="Salganik", 
                                      confidence.level=0.95, 
                                      number.of.bootstrap.samples=nboot)
      
      cat <- length(freq$estimate)
      
      df <- data.frame(matrix(freq$interval, nrow = cat))
      colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
      
      df$category <- attr(freq$estimate, "names")
      
      df$var <- x
      
      df
    }) %>%
      bind_rows() %>% 
      mutate(survey_id = survey_id_c)
    
  } else {
    NULL
  }

}}

rds_adjust_dur4 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_startsw" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_startsw) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(duration4 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "duration4"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}

rds_adjust_dur3 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_fs_paidorgift" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_fs_paidorgift) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(duration3 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "duration3"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}



rds_adjust_dur2 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_fs_paid" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(age = as.numeric(age), 
               age_fs_paid= as.numeric(age_fs_paid),
          duration = age - age_fs_paid) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(duration2 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "duration2"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}


rds_adjust_durmsm <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_fs_man" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_fs_man) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(durationmsm = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "durationmsm"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}



rds_adjust_durmsm2 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_fs_man_anal" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_fs_man_anal) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(durationmsm2 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "durationmsm2"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}



rds_adjust_pwid <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_inject" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = age - age_inject) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(durationpwid = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "durationpwid"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}

rds_adjust_dur1 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "inject_dur" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>% 
        mutate(duration = inject_dur) %>% 
        filter(!duration < 0,
               !duration > age - 10) %>% 
        mutate(duration1 = factor(duration)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "duration1"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}


rds_adjust_age <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age" %in% colnames(df) ) {
      
      df <- df %>%
        # mutate(duration = inject_dur) %>%
        filter(!age < 0,
               !age > 80) %>%
        mutate(age1 = factor(age)) %>%
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "age1"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}


rds_adjust_hiv <- function(df, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  survey_id_c <- unique(df$survey_id)
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "hiv" %in% colnames(df) ) {
      
      df <- df %>% 
        # select(own_coupon, coupon1, coupon2, coupon3, network_size, hiv) %>% 
        # mutate(own_coupon = extract_numeric(own_coupon),
        #        coupon1 = extract_numeric(coupon1),
        #        coupon2 = extract_numeric(coupon2),
        #        coupon3 = extract_numeric(coupon3)) %>% 
        # mutate(duration = inject_dur) %>%
        # filter(!age < 0,
        #        !age > 80) %>%
        mutate(hiv1 = factor(hiv))  #%>% 
        # filter(!is.na(coupon1),
        #        coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
        nboot <- 10000
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "hiv1"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x, 
                                        # weight.type = "RDS-II", uncertainty="Salganik", #N = 9600,
                                        weight.type="Gile's SS", uncertainty="Gile", N = 9600,
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}

rds_adjust_hiv_rds1 <- function(df, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  survey_id_c <- unique(df$survey_id)
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "hiv" %in% colnames(df) ) {
      
      df <- df %>% 
        # select(own_coupon, coupon1, coupon2, coupon3, network_size, hiv) %>% 
        # mutate(own_coupon = extract_numeric(own_coupon),
        #        coupon1 = extract_numeric(coupon1),
        #        coupon2 = extract_numeric(coupon2),
        #        coupon3 = extract_numeric(coupon3)) %>% 
        # mutate(duration = inject_dur) %>%
        # filter(!age < 0,
        #        !age > 80) %>%
        mutate(hiv1 = factor(hiv))  #%>% 
      # filter(!is.na(coupon1),
      #        coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 10000
      # pse <- unique(df$pse)
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "hiv1"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x, 
                                        weight.type = "RDS-I", uncertainty="Salganik", #N = 9600,
                                        # weight.type="Gile's SS", uncertainty="Gile", N = pse,
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}

rds_adjust_hiv_giles <- function(df, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  survey_id_c <- unique(df$survey_id)
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "hiv" %in% colnames(df) ) {
      
      df <- df %>% 
        # select(own_coupon, coupon1, coupon2, coupon3, network_size, hiv) %>% 
        # mutate(own_coupon = extract_numeric(own_coupon),
        #        coupon1 = extract_numeric(coupon1),
        #        coupon2 = extract_numeric(coupon2),
        #        coupon3 = extract_numeric(coupon3)) %>% 
        # mutate(duration = inject_dur) %>%
        # filter(!age < 0,
        #        !age > 80) %>%
        mutate(hiv1 = factor(hiv))  #%>% 
      # filter(!is.na(coupon1),
      #        coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 10000
      pse <- unique(df$pse)
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "hiv1"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x, 
                                        # weight.type = "RDS-II", uncertainty="Salganik", #N = 9600,
                                        weight.type="Gile's SS", uncertainty="Gile", N = pse,
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}

rds_adjust2 <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
  
  surv_type <- str_sub(survey_id_c, 8, -5)
  
  if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age" %in% colnames(df) &  "hiv" %in% colnames(df) ) {
    
    subject.id = "subject_id" 
    subject.coupon = "own_coupon" 
    coupons <- colnames(df)[grep("^coupon", colnames(df))]
    
    num_coupons <- length(coupons)
    network_size = "network_size"
    
    df <- df %>%
      filter(age > 15, 
             age < 49) %>% 
      dplyr::mutate(age_group_label = factor(age)) %>%
      dplyr::left_join(naomi::get_age_groups() %>% 
      select(age_group, age_group_label)) %>%
      #dplyr::select(-age_group_label) %>% 
      filter(!(coupon1 == own_coupon))  
   
 
    recruitor.id = "recruitor.id"
    
   
    vars <- intersect(c("hiv", "age"), colnames(df))
    
    df2 <- df %>% 
      group_by(age_group_label) %>% 
      group_split()
    
    names(df2) <- sort(unique(df$age_group_label))
    
    vars_by_age <- Map(function(x,y) {
      
      ## Once you've found which group fails, I use this to step through the function manually by specifying x and y
      #x <- df2[[7]]
      #y <- names(df2[7])
      
      message(y) ## This is useful in a Map loop so you can see where your error occurs
      
    nboot <- 30
    
    x$recruitor.id <- rid.from.coupons(x, subject.id=subject.id, 
                                                      subject.coupon=subject.coupon, 
                                                      coupon.variables=coupons)
    
    x <- as.rds.data.frame(x, id = subject.id, 
                           recruiter.id=recruitor.id,
                          network.size = network_size,
                         population.size=c(NA,NA,NA), 
                           max.coupons=num_coupons, notes=NULL)
    
    x$seed <- get.seed.id(x)
    x$wave <- get.wave(x)
    
    possibly_bootstrap <- purrr::possibly(RDS.bootstrap.intervals, otherwise = NULL)
    
    freq <- possibly_bootstrap(x, outcome.variable="hiv",
                                    weight.type="RDS-II",
                                    uncertainty="Salganik",
                                    confidence.level=0.95, 
                                    number.of.bootstrap.samples=nboot)
    
    ## Then something that does 
    
    # if(is.null(freq)) {
    #   something sensible to return e.g. unweighted estimate? 
    #   and then some dataframe that labels which estimates are RDS adjusted and which are raw?
    # }

    
    cat <- length(freq$estimate)
    
    df <- data.frame(matrix(freq$interval, nrow = cat))
    colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
    
    df$category <- attr(freq$estimate, "names")
    
    df$var <- "hiv"
    
    df$age_group <- y
    
    df
    
    }, x = df2[13:19], y = names(df2[13:19])) %>%
      bind_rows() 
  }
  }
}


multinomial_model <- function(formula, model_name, S = 1000) {
  
  message(paste0("Begin fitting ", model_name, "."))
  
  fit <- inla(formula, data = df, family = 'xPoisson',
              control.predictor = list(link = 1),
              control.compute = list(dic = TRUE, waic = TRUE,
                                     cpo = TRUE, config = TRUE),
              inla.mode = "experimental")
  
  message(paste0("Completed fitting ", model_name, "."))
  
  message("Begin post-processing")
  
  #' Full R-INLA samples
  full_samples <- inla.posterior.sample(n = S, result = fit)
  
  #' Just the latent field
  eta_samples <- lapply(full_samples, "[", "latent")
  
  #' For some reason "latent" is comprised of more than only the latent field
  eta_samples <- lapply(eta_samples, function(eta_sample) {
    data.frame(eta_sample) %>%
      tibble::rownames_to_column() %>%
      rename(eta = 2) %>%
      filter(substr(rowname, 1, 10) == "Predictor:") %>%
      select(-rowname)
  })
  
  #' Into a matrix with a row for each observation and a column for each sample
  eta_samples_matrix <- matrix(unlist(eta_samples), ncol = S)
  eta_samples_df <- data.frame(eta_samples_matrix)
  
  samples <- eta_samples_df  %>%
    mutate(
      #' To split by
      obs_idx = df$id.ref,
      #' To sample predictive
      #' When n_eff_kish is missing there is no survey for that observation,
      #' so the posterior predictive is meaningless. Setting to zero may save
      #' some computation, but probably better to filter out entirely.
      n_eff_kish_new = floor(ifelse(is.na(df$n_eff_kish), 0, df$n_eff_kish))
    ) %>%
    split(.$obs_idx) %>%
    mclapply(function(x) {
      n_eff_kish_new <- x$n_eff_kish_new
      #' Remove the obs_idx and n_eff_kish_new columns
      x_samples <- x[1:(length(x) - 2)]
      #' Normalise each column (to avoid overflow of softmax)
      # x_samples <- apply(x_samples, MARGIN = 2, FUN = function(x) x - max(x))
      #' Exponentiate (can be done outside apply)
      #' WARNING: That these are samples from lambda posterior isn't true! Come back to this
      lambda_samples <- exp(x_samples)
      #' Calculate samples from posterior of probabilites
      prob_samples <- apply(lambda_samples, MARGIN = 2, FUN = function(x) x / sum(x))
      #' Calculate predictive samples (including sampling variability)
      prob_predictive_samples <- apply(prob_samples, MARGIN = 2, FUN = function(x) {
        stats::rmultinom(n = 1, size = n_eff_kish_new, prob = x) / n_eff_kish_new
      })
      #' Return list, allowing extraction of each set of samples 
      list(
        lambda = data.frame(lambda_samples),
        prob = data.frame(prob_samples),
        prob_predictive = data.frame(prob_predictive_samples))
    })
  
  lambda_samples_df <- bind_rows(lapply(samples, "[[", "lambda"))
  prob_samples_df <- bind_rows(lapply(samples, "[[", "prob"))
  prob_predictive_samples_df <- bind_rows(lapply(samples, "[[", "prob_predictive"))
  
  #' Helper functions
  row_summary <- function(df, ...) unname(apply(df, MARGIN = 1, ...))
  median <- function(x) quantile(x, 0.5, na.rm = TRUE)
  lower <- function(x) quantile(x, 0.025, na.rm = TRUE)
  upper <- function(x) quantile(x, 0.975, na.rm = TRUE)
  
  #' Quantile of the observation within posterior predictive
  prob_predictive_quantile <- prob_predictive_samples_df %>%
    mutate(estimate = df$estimate[!is.na(df$estimate)]) %>%
    apply(MARGIN = 1, function(x) {
      estimate <- x[S + 1]
      samples <- x[1:S]
      if(all(is.na(samples))) return(NA)
      else ecdf(samples)(estimate)
    })
  
  #' Calculate mean, median, lower and upper for each set of samples
  df <- df %>%
    mutate(
      lambda_mean = row_summary(lambda_samples_df, mean),
      lambda_median = row_summary(lambda_samples_df, median),
      lambda_lower = row_summary(lambda_samples_df, lower),
      lambda_upper = row_summary(lambda_samples_df, upper),
      prob_mean = row_summary(prob_samples_df, mean),
      prob_median = row_summary(prob_samples_df, median),
      prob_lower = row_summary(prob_samples_df, lower),
      prob_upper = row_summary(prob_samples_df, upper),
      prob_predictive_mean = row_summary(prob_predictive_samples_df, mean),
      prob_predictive_median = row_summary(prob_predictive_samples_df, median),
      prob_predictive_lower = row_summary(prob_predictive_samples_df, lower),
      prob_predictive_upper = row_summary(prob_predictive_samples_df, upper),
      prob_predictive_quantile = prob_predictive_quantile,
      model = model_name
    )
  
  message("Completed post-processing")
  
  return(list(df = df, fit = fit))
}







rds_adjust_partnerage <- function(df, survey_id_c, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  partneragedf = list()
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    agevars <- c("age_3rdlastpartner", "age_2ndlastpartner", "age_4thlastpartner", "age_5thlastpartner", "age_lastpartner", "client_age", "client_agediff", "partner1_age", "partner2_age", "partner3_age", "partner4_age", "partner5_age")
    
    survey_age_vars <- agevars[agevars %in% colnames(df)]
    
    test_fun <- function(agevar) {
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & agevar %in% colnames(df) ) {
      
      
      df <- df %>% 
        mutate(partner_age = factor(!!sym(agevar))) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 30
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "partner_age"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c,
               agequestion = agevar)
      
      partneragedf[[agevar]] <- df
      
    } else {
      NULL
    }
    }
    
    df <- lapply(survey_age_vars, test_fun) %>%
      bind_rows()
    
  }}

rds_adjust_age_group <- function(df, variable_recode) {
  
  rds_survs <- variable_recode %>% 
    filter(variable  == "coupon1")
  
  survey_id_c <- unique(df$survey_id)
  
  if(survey_id_c %in% rds_survs$survey_id) {
    
    message(survey_id_c)
    
    surv_type <- str_sub(survey_id_c, 8, -5)
    
    if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age_group" %in% colnames(df) ) {
      
      df <- df %>% 
        # mutate(duration = inject_dur) %>% 
        filter(!is.na(age_group)) %>% 
        mutate(age1 = factor(age_group)) %>% 
        filter(!is.na(coupon1),
               coupon1 != "")
      
      median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
      
      df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
      
      nboot <- 1000
      
      # vars <- intersect(c("hiv", "age_fs", "age1","hepb", "syphilis", "age_first_paid"), colnames(df))
      # vars <- "age1"
      vars <- "age1"
      coupons <- colnames(df)[grep("^coupon", colnames(df))]
      
      num_coupons <- length(coupons)
      
      df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
                                          subject.coupon='own_coupon', 
                                          # coupon.variables=c("coupon1","coupon2","coupon3"),
                                          coupon.variables = coupons)
      
      df <- as.rds.data.frame(df, id='subject_id', 
                              recruiter.id='recruiter.id',
                              network.size='network_size',
                              population.size=c(NA,NA,NA), 
                              # max.coupons=3, 
                              max.coupons = num_coupons,
                              notes=NULL)
      
      df$seed <- get.seed.id(df)
      df$wave <- get.wave(df)
      
      
      df <- lapply(vars, function(x) {
        
        freq <- RDS.bootstrap.intervals(df, outcome.variable=x,
                                        weight.type="RDS-II", uncertainty="Salganik", 
                                        confidence.level=0.95, 
                                        number.of.bootstrap.samples=nboot)
        
        cat <- length(freq$estimate)
        
        df <- data.frame(matrix(freq$interval, nrow = cat))
        colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
        
        df$category <- attr(freq$estimate, "names")
        
        df$var <- x
        
        df
      }) %>%
        bind_rows() %>% 
        mutate(survey_id = survey_id_c)
      
    } else {
      NULL
    }
    
  }}

# rds_adjust_hiv_by_age <- function(df, survey_id_c, variable_recode) {
#   
#   rds_survs <- variable_recode %>% 
#     filter(variable  == "coupon1")
#   
#   if(survey_id_c %in% rds_survs$survey_id) {
#     
#     message(survey_id_c)
#     
#     surv_type <- str_sub(survey_id_c, 8, -5)
#     
#     if ("coupon1" %in% colnames(df) & "network_size" %in% colnames(df) & "age" %in% colnames(df) &  "hiv" %in% colnames(df) ) {
#       
#       df <- df %>% 
#         # mutate(duration = inject_dur) %>% 
#         filter(!age < 0,
#                !age > 80,
#                hiv %in% c(0,1)) %>% 
#         mutate(age1 = factor(age)) %>% 
#         filter(!is.na(coupon1),
#                coupon1 != "")
#       
#       median_network_size <- median(df$network_size[!is.na(df$network_size) & df$network_size != 0])
#       
#       df$network_size[is.na(df$network_size) | df$network_size == 0] <- median_network_size
#       
#       nboot <- 30
#       
#       vars <- intersect(c("hiv", "age1"), colnames(df))
#       # vars <- "age1"
#       # vars <- "age1"
#       coupons <- colnames(df)[grep("^coupon", colnames(df))]
#       
#       num_coupons <- length(coupons)
#       
#       df$recruiter.id <- rid.from.coupons(df, subject.id='subject_id', 
#                                           subject.coupon='own_coupon', 
#                                           # coupon.variables=c("coupon1","coupon2","coupon3"),
#                                           coupon.variables = coupons)
#       
#       df <- as.rds.data.frame(df, id='subject_id', 
#                               recruiter.id='recruiter.id',
#                               network.size='network_size',
#                               population.size=c(NA,NA,NA), 
#                               # max.coupons=3, 
#                               max.coupons = num_coupons,
#                               notes=NULL)
#       
#       df$seed <- get.seed.id(df)
#       df$wave <- get.wave(df)
#       
#       
#       df <- lapply(vars, function(x) {
#         
#         freq <- RDS.bootstrap.intervals(df, outcome.variable="hiv",
#                                         weight.type="RDS-II", uncertainty="Salganik", 
#                                         confidence.level=0.95, 
#                                         number.of.bootstrap.samples=nboot)
#         
#         cat <- length(freq$estimate)
#         
#         df <- data.frame(matrix(freq$interval, nrow = cat))
#         colnames(df) <- c("estimate", "lower", "upper", "des_effect", "se", "n")
#         
#         df$category <- attr(freq$estimate, "names")
#         
#         df$var <- x
#         
#         df
#       }) %>%
#         bind_rows() %>% 
#         mutate(survey_id = survey_id_c)
#       
#     } else {
#       NULL
#     }
#     
#   }}
#     