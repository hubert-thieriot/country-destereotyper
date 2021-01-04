library(shiny)

shinyServer(function(input, output) {

    # Global Variables ---------------------------------------

    # Reactive UI ------------------------------------------
    output$selectAoi <- renderUI({
        pickerInput('aoi', 'Region of interest', survey.regions(),
                    selected=survey.regions()[[6]])
    })

    output$selectBaseline <- renderUI({
        pickerInput('baseline', 'Compared region', survey.regions(),
                    selected="All")
    })


    # Reactive Values ---------------------------------------
    # questions.n <- reactiveValues(old=0, cur=0)
    # observeEvent(input$questions.n, {questions.n$old <- questions.n$cur; questions.n$cur <- input$questions.n})


    survey.data <- reactive({
        req(input$survey)
        surveys.data[[input$survey]]
    })

    survey.regions <- reactive({
        utils.regions(survey.data())
    })

    survey.labels <- reactive({
        attr(survey.data(), "labels")
    })

    # survey.answer.levels <- reactive({
    #     attr(survey.data(), "answer.levels")
    # })

    # survey.answer.attributes <- reactive({
    #     attr(survey.data(), "answer.attributes")
    # })

    countries.aoi <- reactive({
        req(input$aoi)
        util.region_countries(levels(survey.data()$COUNTRY), input$aoi)
    })

    countries.baseline <- reactive({
        req(input$baseline)
        util.region_countries(levels(survey.data()$COUNTRY), input$baseline)
    })

    survey.data.aoi <- reactive({
        tibble(
            region="aoi",
            COUNTRY=countries.aoi()) %>%
        left_join(survey.data(), by="COUNTRY")
    })

    survey.data.baseline <- reactive({
        tibble(
            region="baseline",
            COUNTRY=countries.baseline()
        ) %>%
            left_join(survey.data(), by="COUNTRY")
    })

    survey.data.selected <- reactive({
        bind_rows(survey.data.aoi(),
                  survey.data.baseline())
    })

    survey.answers.freq <- reactive({

        survey.data.selected() %>%
            filter(!is.na(answer)) %>%
            group_by(region, question, answer) %>%
            summarise(freq=mean(freq, na.rm=T)) %>% #TODO Think about how to weigh across countries
            ungroup() %>%
            tidyr::pivot_wider(names_from="region",
                               values_from="freq",
                               names_prefix="freq_")
    })

    survey.questions.ordered <- reactive({

        req(input$metric)

        if(input$metric=="entropy"){
            distance_fn <- function(group){

                # Ignoring answers not read
                group <- group %>% drop_na() %>%
                    filter(!grepl("DO NOT READ", answer))

                if(nrow(group)==0){
                    return(0)
                }


                group <- group %>%
                    mutate(entropy_like=-freq_aoi * freq_baseline * log2(freq_baseline)) %>%
                    # IGNORE ANSWERS WITH DO NOT READ
                    mutate(entropy_like=ifelse(grepl("DO NOT READ", answer), 0, entropy_like))

                return(sum(group$entropy_like))
            }
        }

        if(input$metric=="area"){
            distance_fn <- function(group){

                # Ignoring answers not read
                group <- group %>% drop_na() %>%
                    filter(!grepl("DO NOT READ", answer))

                if(nrow(group)==0){
                    return(0)
                }

                if(!is.ordered(group$answer)){
                    return(sum(abs(group$freq_aoi-group$freq_baseline)))
                }else{
                    group$answer <- as.integer(group$answer)
                    fdiff <- approxfun(group$answer, group$freq_aoi-group$freq_baseline)
                    fabs <- function(x) abs(fdiff(x))
                    return(integrate(fabs, min(group$answer), max(group$answer))$value)
                }
            }
        }

        survey.answers.freq() %>%
            group_by(question) %>%
            mutate(question2=question) %>% # So that we can have the question in grouping function
            summarise(distance=distance_fn(cur_data())) %>%
            arrange(desc(distance))
    })

    survey.questions.selected <- reactive({
        survey.questions.ordered() %>%
            head(questions.n) %>%
            mutate(label=survey.labels()[question]) %>%
            left_join(survey.answers.freq()) %>%
            tidyr::pivot_longer(c(freq_aoi, freq_baseline),
                                names_prefix="freq_",
                                names_to="region")
    })

    survey.plots.selected <- reactive({

        d <- survey.questions.selected()

        questions <- unique(d$question)

        lapply(questions, function(q){
            dq <- d %>% filter(question==q)
            dq$answer <- factor(dq$answer,
                               levels=rev(levels(dq$answer)),
                               labels=gsub("\\(DO NOT READ\\)","",
                                           gsub(paste0(unique(dq$question),"-",collapse="|"),"",
                                                rev(levels(dq$answer)))))


            return(ggplot( dq %>%
                        mutate(label=gsub("\\n","<br>",utils.highlight_keywords(str_wrap(label, width=80), keywords)),
                               region=factor(dq$region,c("aoi","baseline"),c(input$aoi,input$baseline))),
                    aes(answer,
                        value,
                        group=region,
                        linetype=region,
                        col=region)) +
                geom_line(size=0.8, show.legend = F) +
                geom_dl(aes(label=region), method="first.bumpup")+
                scale_linetype_manual(values=c("solid","dashed")) +
                theme_light(base_size=15) +
                scale_y_continuous(labels=scales::percent) +
                facet_wrap(~label, scales="free_x") +
                scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
                labs(x=NULL,y=NULL) +
                theme(
                    strip.text = element_markdown(size=14),
                    strip.background=element_rect(fill="#333343")
                ))
        })
    })


    output$plot_box1 <- renderPlot(
        if(length(survey.plots.selected()) >= 1) survey.plots.selected()[[1]] else NULL)
    output$plot_box2 <- renderPlot(
        if(length(survey.plots.selected()) >= 2) survey.plots.selected()[[2]] else NULL)
    output$plot_box3 <- renderPlot(
        if(length(survey.plots.selected()) >= 3) survey.plots.selected()[[3]] else NULL)
    output$plot_box4 <- renderPlot(
        if(length(survey.plots.selected()) >= 4) survey.plots.selected()[[4]] else NULL)
    output$plot_box5 <- renderPlot(
        if(length(survey.plots.selected()) >= 5) survey.plots.selected()[[5]] else NULL)
    output$plot_box6 <- renderPlot(
        if(length(survey.plots.selected()) >= 6) survey.plots.selected()[[6]] else NULL)


    # General Observers -----------------------------------



})
