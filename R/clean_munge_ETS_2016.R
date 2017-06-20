#' Clean and Munge 2016 Enhanced Tuberculosis Surviellance Data
#' @description A function that imports the ETS data as a stata file, converts it to rds
#' format, cleans it to a tidy format, and finally munges required variables for future analysis.
#' @details Function has 3 distinct sections:
#' - Data Import
#' - Setting factor variable baslines
#' - Munge new variables
#' @param data_path A character string containing the file pathway for the 2016 ETS data.
#' @param return Logical, defaults to \code{TRUE}. Specifies whether to return cleanded data.
#' @param save Logical, defaults to \code{FALSE}. Specifies whether to save data
#' @param save_name A character string containing the name to save the tidy ETS data under.
#' @param save_path A character string containing the file pathway to the folder into
#' which to save the tidy ETS data for the 2016 ETS data.
#'
#' @return A tidy tibble of TB notficiations in England from 2000 to 2016, with a row for
#' each notification.
#' @export
#' @import magrittr
#' @importFrom haven read_stata as_factor
#' @importFrom dplyr mutate filter
#' @importFrom purrr map map_chr
#' @importFrom stats relevel
#' @examples
#'
clean_munge_ETS_2016 <- function(data_path = NULL,
                                 return = TRUE,
                                 save = FALSE,
                                 save_name = "clean_ETS_2016",
                                 save_path = NULL) {
  if (is.null(data_path)) {
    stop("The pathway to the data to munge and clean has not been specified")
  }

  if (is.null(save_path)) {
    stop("The pathway to save munged and cleaned data has not been specified")
  }

  df <- read_stata(data_path)

  ## change from stata format to R factors
  df <- df %>% haven::as_factor(only_labelled = TRUE, levels = "label")

  ## Hand code countries to factor
  df <- df %>%
    mutate(cob = cob %>% as_factor)

  ## Set up missing data labelling using NA
  df <- df %>% map(function(x){

    x[x %in% "NaN"] <- NA
    x[x %in% "NA"] <- NA
    x[x %in% "<NA>"] <- NA
    x[x %in% "Unknown"] <- NA
    x[x %in% ""] <- NA
    return(x)
  }) %>%
    bind_cols %>%
    droplevels

  # occupation baseline: other
  df$occat <- df$occat %>%  relevel(ref = 6)

  # age groups add 5 year breaks
  df <- df %>%
    mutate(agegrp2 = age %>% replace(age > 90, 90) %>%
             cut(breaks = seq(0, 95, 5),
                 right = FALSE,
                 ordered_result = TRUE,
                 labels = c(paste(seq(0, 85, 5),
                                  seq(4, 89, 5),
                                  sep = "-"),
                            "90+")))

  #reorder
  level <- c(levels(df$agegrp2)[1:2],
             levels(df$agegrp2)[length(levels(df$agegrp2))],
             levels(df$agegrp2)[3:(length(levels(df$agegrp2)) - 1)])

  df$agegrp2 <- factor(as.character(df$agegrp2), level)

  #phec - baseline: south west (BCG control region)
  df$phec <- df$phec %>% relevel(ref = 7)

  #ethnic group - baseline: white
  df$ethgrp <- df$ethgrp %>%  relevel(ref = 1)

  #overalloutcome - baseline: not evaluated
  df$overalloutcome <- df$overalloutcome %>%  relevel(ref = 6)

  #clean time to treatcomplete - removing cases
  # that complete treatmet in less that zero days

  df <- df %>%
    mutate(timetocomplete = ifelse(timetocomplete < 0, NA, timetocomplete))

  # Set up useful variables ----------------------

  #Add years since BCG
  df$yr_bcg <- df$year - df$bcgvaccyr

  # Clean years since BCG
  df$yr_bcg[df$yr_bcg <= 0] <- 0
  df$yr_bcg <- as.integer(df$yr_bcg)

  #Age at BCG
  df$age_bcg <- df$age - df$yr_bcg

  # Clean age at BCG
  df$age_bcg[df$age_bcg <= 0] <- 0
  df$age_bcg <- as.integer(df$age_bcg)

  #Vaccinated at Birth (vaccinated before 8 or after 8)
  df$vac_birth <- ifelse(df$age_bcg <= 1, "Yes", "No")
  df$vac_birth <- factor(df$vac_birth)

  #catagorical age at vaccination
  df$ageatvac <- df$age_bcg %>% map_chr(function(.) {
    if (is.na(.)) {
      temp <- NA
    }
    else if (. < 1) {
      temp <- "< 1"
    }else if (. < 12) {
      temp <- "1 \u2264 x < 12"
    }else if (. < 16) {
      temp <- "12 \u2264 x < 16"
    }else {
      temp <- "\u2265 16"
    }
    return(temp)
  }
  )  %>% factor(levels = c("< 1",
                            "1 \u2264 x < 12",
                            "12 \u2264 x < 16",
                            "\u2265 16")
                 )

  #catagorical time since BCG
  df$YrSinceBCG <- df$yr_bcg %>% map_chr(function(.) {
    if (is.na(.)) {
      temp <- NA
    }
    else if (. <= 10) {
      temp <- "\u2264 10"
    }else {
      temp <- "11+"
    }
    return(temp)
  }
  ) %>% factor(levels = c("\u2264 10", "11+"))

  #Treatment Success to the data
  df$succTreat <- sapply(1:nrow(df), function(i) {
    if (df$overalloutcome[i] %in% "Treatment completed") {
      M <- "Yes"
    }else if (sum(df$overalloutcome[i] %in% c("",
                                              "Not Evaluated")
                  ) == 1 ||
              is.na(df$overalloutcome[i])
              ) {
      M <- NA
    }else {
      M <- "No"
    }
    return(M)
  }
  ) %>% factor

  # Filter successful treatment so that only that were confirmed
  # to have died or have death related to TB are counted as a treatment
  #success (of those cases that have died)

  df$succTreat <- df$succTreat %>% as.character
  df$succTreat <- ifelse(!(df$tomdeathrelat %in% c("TB caused death",
                                                   "TB contributed to death")
                           ),
                         ifelse(df$overalloutcome %in% c("Died"),
                                NA,
                                df$succTreat),
                         df$succTreat)  %>%
    factor

  ## Filter treatment so that only those who started treatment are included
  df <- df %>%
    mutate(succTreat = succTreat %>% as.character ) %>%
    mutate(succTreat = ifelse(is.na(starttreatdate), NA, succTreat) %>% factor)

  ## Successful treatment at 12 months - using finaloutcome12
  df$SuccTreat12 <- sapply(1:nrow(df), function(i) {
    if (df$finaloutcome12[i] %in% "Treatment completed") {
      M <- "Yes"
    }else if (sum(df$finaloutcome12[i] %in% c("", "Not Evaluated")) == 1 ||
              is.na(df$finaloutcome12[i])) {
      M <- NA
    }else {
      M <- "No"
    }
    return(M)
  }
  ) %>% factor

  # Filter successful treatment so that only that were confirmed to have died
  # or have death related to TB are counted as a treatment success (of those
  # cases that have died)
  df$SuccTreat12 <- df$SuccTreat12 %>% as.character
  df$SuccTreat12 <- ifelse(!(df$tomdeathrelat %in% c("TB caused death",
                                                     "TB contributed
                                                     to death")),
                           ifelse(df$finaloutcome12 %in% c("Died"),
                                  NA, df$SuccTreat12),
                           df$SuccTreat12)  %>%
    factor

  ## Filter treatment so that only those who started treatment are included
  df <- df %>%
    mutate(SuccTreat12 = SuccTreat12 %>% as.character ) %>%
    mutate(SuccTreat12 = ifelse(is.na(starttreatdate),
                                NA,
                                SuccTreat12
                                ) %>% factor)

  # Add mortality as an outcome
  df$mortality <- sapply(1:nrow(df), function(i) {
    if (df$overalloutcome[i] %in% "Died") {
      M <- "Yes"
    }else if (sum(df$overalloutcome[i] %in%
                  c("","Lost to follow up", "Not Evaluated")) == 1 ||
              is.na(df$overalloutcome[i])) {
      M <- NA
    }else {
      M <- "No"
    }
    return(M)
  }
  ) %>% factor


  ## Add outcome for TB related mortality
  df$TBMortality <- sapply(1:nrow(df), function(i) {
    if (df$overalloutcome[i] %in% "Died" &&
        df$tomdeathrelat[i] %in% c("TB caused death",
                                   "TB contributed to death")
        ) {
      M <- "Yes"
    }else if (sum(df$overalloutcome[i] %in% c("",
                                              "Lost to follow up",
                                              "Not Evaluated")) == 1 ||
              is.na(df$overalloutcome[i]) ||
              (df$overalloutcome[i] %in% "Died" &&
               is.na(df$tomdeathrelat[i]))
              ) {
      M <- NA
    }else {
      M <- "No"
    }
    return(M)
  }
  ) %>% factor

  ## Add outcome for death due to TB
  df$DeathDueTB <- sapply(1:nrow(df), function(i) {
    if (df$tomdeathrelat[i] %in% c("TB caused death",
                                   "TB contributed to death")) {
      M <- "Yes"
    }else if (is.na(df$tomdeathrelat[i])) {
      M <- NA
    }else {
      M <- "No"
    }
    return(M)
  }
  ) %>% factor

  ## Set IMD rank to be an ordered factor
  df <- df %>%
    mutate(natquintile = natquintile %>%  factor(levels = as.character(1:5)),
           natdecile = natdecile %>% factor(levels = as.character(1:10)))

  ## Limit data set to England
  df <- df %>% filter(country %in% c("England"))

  if (save) {
    save(df, file = file.path(save_path, paste0(save_name, ".rda")))
  }

  if (return) {
    return(df)
  }
}
