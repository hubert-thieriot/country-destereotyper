
#' Prepare raw survey data: convert it, lighten it (e.g. aggregate by country),
#' and export it to RDS
#'
#' @param file.raw Filepath of .sav file
#' @param force Whether to force file creation if it already exists
#'
#' @return
#' @export
#'
#' @examples
utils.prepare_data <- function(file.raw, force=F){
  file.rds <- gsub("\\.sav","\\.RDS",file.raw)

  if(force | !file.exists(file.rds)){
    # We use both libraries haven and foreign
    # foreign keeps attributes, haven keeps labels ordering information
    data.raw.haven <- haven::read_sav(file.raw)
    data.raw.foreign <- foreign::read.spss(file.raw, to.data.frame = T)
    # Lower/Upper caps is not consistent across surveys. We'll upper-case everything
    names(data.raw.haven) <- toupper(names(data.raw.haven))
    names(data.raw.foreign) <- toupper(names(data.raw.foreign))

    # Keep ordered factors, can be important for distance calculation
    data.raw.haven <- purrr::map_df(data.raw.haven, utils.relabel_spss_variable)

    labels <- utils.labels(data.raw.foreign)
    questions <- utils.questions(data.raw.foreign, labels)
    answer.attributes <- utils.answer_attributes(data.raw.foreign, questions)

    data.answers <- data.raw.foreign %>%
      utils.pivot_longer(questions) %>%
      utils.answer_frequency()

    attr(data.answers, "labels") <- labels
    attr(data.answers, "questions") <- questions
    attr(data.answers, "answer.attributes") <- answer.attributes

    saveRDS(data.answers, file.rds)
    return(data.answers)
  }else{
    return(readRDS(file.rds))
  }
}



utils.surveys_data <- function(force=F){

  # First download data from https://www.pewresearch.org/global/datasets/
  # and put sav files in a "data" folder
  file.raws <- list(
    "Spring 2018"=file.path("data", "Pew Research Global Attitudes Spring 2018 Dataset WEB FINAL.sav"),
    "Spring 2019"=file.path("data", "Pew Research Center Global Attitudes Spring 2019 Dataset WEB.sav")
  )

  lapply(file.raws,
         utils.prepare_data,
         force=force)
}

utils.relabel_spss_variable <- function(x) {
  # https://sciphy-stats.com/post/import-ordered-spss-factors-into-r/
  a <- base::attr(x = x, "labels")
  if(!is.null(a)) {
    labels = base::names(a)
    levels = base::as.character(a)
    base::factor(x = x, levels = levels, labels = labels, ordered = TRUE)
  } else {
    warning("x is not label. No relabelling done.")
    x
  }
}

utils.labels <- function(data.raw){
    attributes(data.raw)$variable.labels %>%
      setNames(toupper(names(.)))
}


utils.is_question <- function(q.name, data, labels){
  is.factor(data[[q.name]]) &
    length(levels(data[[q.name]])) < 10 & #Questions with more than 10 levels are probably "opened questions"
    labels[[q.name]] != " " &
    !any(startsWith(toupper(q.name), c("REGION", "STRATUM" ,"QS5", "ID_",'LANGUAGE_','D_','PARTYFAV','PSU'))) &
    !(toupper(q.name) %in% c("SURVEY","COUNTRY","ID","WEIGHT","AGE","ID","SEX","QS8"))
}

utils.questions <- function(data.raw, labels){
  names(data.raw)[sapply(names(data.raw),
                                       utils.is_question,
                                       data.raw,
                                       labels)]
}

utils.answer_attributes <- function(data.raw, questions){
  data.raw %>%
    sapply(function(x) attributes(x[1]))
}
#
# utils.answer_levels <- function(data.raw, questions){
#
#   data.raw %>%
#     select_at(questions) %>%
#     sapply(levels) %>%
#     unlist() %>%
#     unique()
# }

utils.regions <- function(survey.data){
     c(list("All"="All",
            "Europe"="Europe",
            "Asia"="Asia",
            "Africa"="Africa",
            "Americas"="Americas"),
       setNames(as.list(levels(survey.data$COUNTRY)), levels(survey.data$COUNTRY)))

}

util.region_countries <- function(all_countries, region){

  if(region=="All"){
    return(all_countries)
  }

  if(region %in% c("Europe","Asia","Africa","Americas")){
    return(all_countries[countrycode(all_countries, "country.name", "continent")==region])
  }

  return(region)
}

utils.pivot_longer <- function(data.raw, questions,
                               ids = c("ID","SURVEY","COUNTRY","WEIGHT")){

  levels <- c()
  for(q in questions){
    levels <- c(levels, paste(q,levels(data.raw[[q]]),sep="-"))
  }

   d <- data.raw %>%
     select_at(c(ids, questions)) %>%
     tidyr::pivot_longer(all_of(questions), names_to="question", values_to="answer") %>%
     mutate(answer=paste(question,answer,sep="-")) %>%
     filter(!is.na(answer))

   d$answer <- factor(as.character(d$answer), levels, ordered=T)
   d
}

utils.answer_frequency <- function(data.long){
  data.long %>%
    group_by(SURVEY,COUNTRY,question,answer) %>%
    summarise(weight_answer=sum(WEIGHT)) %>%
    mutate(freq=weight_answer/sum(weight_answer)) %>%
    ungroup() %>%
    select(-c(weight_answer))
}

utils.highlight_keywords <- function(text, keywords){
  for(k in keywords){
    text <- sub(glue("([^>])({k})([^a-z]|$)"), # Avoid highlighting twice
         glue("\\1<span style='font-size:16pt; color:#FF1493;'>**\\2**</span>\\3"),
         text,
         ignore.case = TRUE)
  }
  return(text)
}

utils.deploy_app <- function(){
  require(rsconnect)
  readRenviron(".Renviron")
  rsconnect::setAccountInfo(name=Sys.getenv("SHINY_NAME"),
                            token=Sys.getenv("SHINY_TOKEN"),
                            secret=Sys.getenv("SHINY_SECRET"))
  deployApp(account="yunbo")
}
