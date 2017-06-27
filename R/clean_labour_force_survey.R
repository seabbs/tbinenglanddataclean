#' Load and Clean Labour Force Survey Data
#'
#' @description This functions automatically detects Labour Force Survey data in the specified
#' directory and reads it into R. It then extracts the age, sex, UK birth status, and country.
#' From this it creates a tidy dataset. The data can be downloaded [here](https://www.ons.gov.uk/surveys/informationforhouseholdsandindividuals/householdandindividualsurveys/labourforcesurveylfs).
#' @inheritParams clean_demographics_uk
#' @param years A numeric vector specifying which years of data to clean
#' @param years_var A named list of character strings. Each character string should contain the variables
#' to extract from a given year and this should be named with the year of data to extract.
#' @return A tidy data frame of population broken down by country, age, sex and UK birth status
#' for 2000 to 2015.
#' @export
#' @import dplyr
#' @importFrom haven read_stata
#' @importFrom purrr pmap
#' @import ggplot2
#' @importFrom plotly ggplotly
#' @examples
#'
clean_labour_force_survey <- function(data_path = "~/data/LFS",
                          years = 2000:2016,
                          years_var = list( '2000' = c('age', 'sex', 'cry', 'govtof', 'pwt07'),
                                            '2001' = c('age', 'sex', 'cry01', 'country', 'pwt07'),
                                            '2002' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2003' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2004' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2005' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2006' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2007' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2008' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2009' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2010' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2011' = c('AGE', 'SEX', 'CRY01', 'COUNTRY', 'PWT14'),
                                            '2012' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT14'),
                                            '2013' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT16'),
                                            '2014' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT16'),
                                            '2015' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT16'),
                                            '2016' = c('AGE', 'SEX', 'CRY12', 'COUNTRY', 'PWT16')),
                          return = TRUE,
                          save = TRUE,
                          save_name = "formatted_LFS_2000_2016",
                          save_path = "~/data/tbinenglanddataclean",
                          verbose = TRUE,
                          interactive = TRUE,
                          theme = theme_minimal()) {

  # Read in LFS data --------------------------------------------------------
  ## data notes
  ## NA and DNA are coded as -8 and -9
  ## list data folders in directory
  LFS_folders <- list.files(path = data_path)

  ## ignore data
  LFS_folders <- LFS_folders[grep('rds', LFS_folders, invert = TRUE)]

  ## For each data folder in the directory
  LFS_data <- LFS_folders %>%  lapply(function(x){

    ## Find path for folder
    folder_dir <- file.path(data_path, x)

    ## list data folder contents
    folder_folders <- list.files(path = folder_dir)

    ## find stata folder
    data_folder <- folder_folders[grep('stata', folder_folders)]

    ## find data folder dir
    data_sub_path <- file.path(folder_dir, data_folder)

    ## contents of data folder
    stata_folder <- list.files(path = data_sub_path)

    ## dat path
    full_path <- file.path(data_sub_path, stata_folder)

    if (verbose) {
      message("Data loaded from: ", full_path)
    }
    ## read in the data
    df <- read_stata(file = full_path)

    return(df)
  })

  ## name data list
  names(LFS_data) <- LFS_folders



  # Extract key variables and combine ---------------------------------------

  form_LFS_data <- pmap(list(LFS_data, years, years_var), function(x, year, year_var){
    x %>%
      select_(.dots = year_var) %>%
      mutate(Year = year) -> x

    if (verbose) {
      message("Cleaning LFS data from: ", year)
      message("Variables extracted from dataset: ", year_var)
    }
    ## Account for no avail of country in 2000 and format in R format
    if (year == 2000)
    {
      ## clean age with R style missing
      x %>%
        mutate(Age = replace(age, age %in% c(-8, -9), NA)) %>%
        select(-age) -> x

      ## country of residence
      x %>%
        mutate(Country = ifelse(govtof %in% c(1,2,3,4,5,6,7,8,9,10), 'England',
                                ifelse(govtof %in% c(11), 'Wales', ifelse(govtof %in% c(12), 'Scotland',
                                                                          ifelse(govtof %in% c(13), 'Wales', NA))))) %>%
        select(-govtof) -> x
      ## country of birth (UK/not UK)
      x %>%
        mutate(CoB = ifelse(cry %in% c(-8,-9), NA,
                            ifelse(cry %in% c(1), 'UK born', 'Non-UK born'))) %>%
        select(-cry) -> x

      ## formating of sex
      x %>%
        mutate(Sex = ifelse(sex %in% c(-8,-9), NA,
                            ifelse(sex %in% c(1), 'Male', 'Female'))) %>%
        select(-sex) -> x

      ## standardise weight
      x %>%
        rename(Weight = pwt07) -> x

    }else if (year == 2001)
    {
      ## clean age with R style missing
      x %>%
        mutate(Age = replace(age, age %in% c(-8, -9), NA)) %>%
        select(-age) -> x

      ## country of residence
      x %>%
        mutate(Country = ifelse(country %in% c(-9, -8), NA,
                                ifelse(country %in% c(1), 'England',
                                                                   ifelse(country %in% c(2), 'Wales',
                                                                          ifelse(country %in% c(3,4), 'Scotland',
                                                                                 'Northern Ireland'))))) %>%
        select(-country) -> x
      ## country of birth (UK/not UK)
      x %>%
        mutate(CoB = ifelse(cry01 %in% c(-8,-9), NA,
                          ifelse(cry01 %in% c(1,2,3,4,5), 'UK born', 'Non-UK born'))) %>%
        select(-cry01) -> x

      ## formating of sex
      x %>%
        mutate(Sex = ifelse(sex %in% c(-8,-9), NA,
                            ifelse(sex %in% c(1), 'Male', 'Female'))) %>%
        select(-sex) -> x

      ## standardise weight
      x %>%
        rename(Weight = pwt07) -> x

    }else if (year %in% 2002:2011)
    {
      ## clean age with R style missing
      x %>%
        mutate(Age = replace(AGE, AGE %in% c(-8, -9), NA)) %>%
        select(-AGE) -> x

      ## country of residence
      x %>%
        mutate(Country = ifelse(COUNTRY %in% c(-9, -8), NA,
                                ifelse(COUNTRY %in% c(1), 'England',
                                       ifelse(COUNTRY %in% c(2), 'Wales',
                                              ifelse(COUNTRY %in% c(3,4), 'Scotland', 'Northern Ireland'))))) %>%
        select(-COUNTRY) -> x
      ## Split due to  errors in variable encoding between 2002 and 2007
      if (year %in% c(2002:2006))
      {
        ## country of birth (UK/not UK)
        x %>%
          mutate(CoB = ifelse(CRY01 %in% c(-8,-9), NA,
                              ifelse(CRY01 %in% c(1,2,3,4,5), 'UK born', 'Non-UK born'))) %>%
          select(-CRY01) -> x
      }else{
        x %>%
          mutate(CoB = ifelse(CRY01 %in% c(-8,-9), NA,
                              ifelse(CRY01 %in% c(921, 922, 923, 924, 926), 'UK born', 'Non-UK born'))) %>%
          select(-CRY01) -> x
      }
      ## country of birth (UK/not UK)


      ## formating of sex
      x %>%
        mutate(Sex = ifelse(SEX %in% c(-8,-9), NA,
                            ifelse(SEX %in% c(1), 'Male', 'Female'))) %>%
        select(-SEX) -> x

      ## standardise weight
      x %>%
        rename(Weight = PWT14) -> x

    }else if (year %in% 2012:2016)
    {
      ## clean age with R style missing
      x %>%
        mutate(Age = replace(AGE, AGE %in% c(-8, -9), NA)) %>%
        select(-AGE) -> x

      ## country of residence
      x %>%
        mutate(Country = ifelse(COUNTRY %in% c(-9, -8), NA,
                                ifelse(COUNTRY %in% c(1), 'England', ifelse(COUNTRY %in% c(2), 'Wales',
                                                                            ifelse(COUNTRY %in% c(3,4), 'Scotland', 'Northern Ireland'))))) %>%
        select(-COUNTRY) -> x
      ## country of birth (UK/not UK)
      x %>%
        mutate(CoB = ifelse(CRY12 %in% c(-8,-9), NA,
                          ifelse(CRY12 %in% c(921, 922, 923, 924, 926), 'UK born', 'Non-UK born'))) %>%
        select(-CRY12) -> x

      ## formating of sex
      x %>%
        mutate(Sex = ifelse(SEX %in% c(-8,-9), NA,
                          ifelse(SEX %in% c(1), 'Male', 'Female'))) %>%
        select(-SEX) -> x

      #Set weights based on the variable
      if (year %in% c(2012))
      {
        ## standardise weight
        x %>%
          rename(Weight = PWT14) -> x
      }else{
        ## standardise weight
        x %>%
          rename(Weight = PWT16) -> x
      }

    }else{
      stop('Year has no defined variables, or cleaning process; check year_var')
    }

    return(x)
  }) %>% bind_rows

  ## Set variables as factors
  form_LFS_data <- form_LFS_data %>%
    mutate(Country = factor(Country),
           CoB = factor(CoB),
           Sex = factor(Sex))

  ## standidise age with other data formats
  form_LFS_data <- form_LFS_data %>%
    mutate(Age = replace(Age, Age >= 90, '90+')) %>%
    mutate(Age = factor(Age, levels = c(as.character(0:89), '90+')))


if (verbose) {
  # Simple plots of the data ------------------------------------------------
  ## 2000 distribution by UK birth status
  form_LFS_data %>%
    filter(Year %in% 2000) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~CoB, scales = 'free', nrow = 3) +
    theme(axis.text.x = element_text(angle = 90)) -> p

  if (interactive) {
    print(ggplotly(p))
  }else {
    print(p)
  }


  ## 2005 distribution by UK birth status
  form_LFS_data %>%
    filter(Year %in% 2005) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~CoB, scales = 'free', nrow = 3) +
    theme(axis.text.x = element_text(angle = 90)) -> p1

  if (interactive) {
    print(ggplotly(p1))
  }else {
    print(p1)
  }


  ## 2010 distribution by UK birth status
  form_LFS_data %>%
    filter(Year %in% 2010) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~CoB, scales = 'free', nrow = 3) +
    theme(axis.text.x = element_text(angle = 90)) -> p2

  if (interactive) {
    print(ggplotly(p2))
  }else {
    print(p2)
  }



  ## 2015 distribution by UK birth status
  form_LFS_data %>%
    filter(Year %in% 2015) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~CoB, scales = 'free', nrow = 3) +
    theme(axis.text.x = element_text(angle = 90)) -> p3

  if (interactive) {
    print(ggplotly(p3))
  }else {
    print(p3)
  }

  ## 2000 distribution by Country
  form_LFS_data %>%
    filter(Year %in% 2000) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~Country, scales = 'free', nrow = 3) +
    theme(axis.text.x = element_text(angle = 90)) -> p4

  if (interactive) {
    print(ggplotly(p4))
  }else {
    print(p4)
  }

  ## 2005 distribution by Country
  form_LFS_data %>%
    filter(Year %in% 2005) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~Country, scales = 'free', nrow = 3) +
    theme(axis.text.x = element_text(angle = 90)) -> p5

  if (interactive) {
    print(ggplotly(p5))
  }else {
    print(p5)
  }
  ## 2010 distribution by Country
  form_LFS_data %>%
    filter(Year %in% 2010) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~Country, scales = 'free', nrow = 3) +
    theme(axis.text.x = element_text(angle = 90)) -> p6

  if (interactive) {
    print(ggplotly(p6))
  }else {
    print(p6)
  }

  ## 2015 distribution by Country
  form_LFS_data %>%
    filter(Year %in% 2015) %>%
    ggplot(aes(x = Age)) +
    geom_bar(alpha = 0.4) +
    facet_wrap(~Country, scales = 'free', nrow = 3) +
    theme(axis.text.x = element_text(angle = 90)) -> p7

  if (interactive) {
    print(ggplotly(p7))
  }else {
    print(p7)
  }

}

  ## save formatted LFS data
  if (save) {
    save_file_path <- file.path(save_path, paste0(save_name, ".rds"))

    if (verbose) {
      message("Cleaded LFS data has been saved to: ", save_file_path)
    }
    saveRDS(form_LFS_data, file = save_file_path)
  }

}
