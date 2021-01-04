library(shiny)


shinyUI(dashboardPage(

    dashboardHeader(title="Country (de)stereotyper"
    ),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        tags$style(type = "text/css",
                   ".content-wrapper {background: white;}
                    #plot_questions {height: calc(100vh - 265px) !important;}
                    .rowinput{height:15px;}
                    .intro{font-size:18px;}
                    .box{border-top:none;
                        box-shadow:none;
                        margin-bottom:0;}
                    img{height:100%;}"),


        fluidRow(
            box(width=12,
                # column(width=4,
                div(class = "intro",
                    tagList("Finally construct/deconstruct your own prejudice about country cultures in a ",
                    tags$span(style="text-decoration: line-through;","scientific"),
                    " slightly backed-up way. Compare two regions and see on which questions of the",
                            a("Global Attitudes & Trends Survey", href="https://www.pewresearch.org/global/"),
                            " these regions differ the most. Have some ideas, suggestions? Send me an",
                            a("email", href="mailto://hubert.thieriot@gmail.com", target="_blank"),
                            "or contribute to the source code ",
                            a("here", href="https://github.com/hubert-thieriot/country-destereotyper",target="_blank"),
                            ".")
                )
            )
        ),
        fluidRow(
            box(width=4,
            # column(width = 2,
                    uiOutput("selectAoi")
                   ),
            box(width=4,
            # column(width = 2,
                       uiOutput("selectBaseline")
                   ),
            box(width=4,
            # column(width = 2,
                        selectInput('survey', 'Survey', surveys, selected=surveys[-1])
                   ),
            # column(width = 2,
            #            selectInput('metric', 'Metric', metrics)
            #        ),
            # box(width=3,
            #        sliderInput('questions.n', 'Number of questions', min=1, max=8, value=4)
            #        ),
            # class = "rowinput",

            ),

        # fluidRow(
        #     box(width=12,
        #      plotOutput("plot_questions", height="100%")
        #     )
        #  )
        fluidRow(
            lapply(seq(1,questions.n),function(x){
                box(width=6,
                    plotOutput(paste0("plot_box",x)))
                })
        )
       )
    )
)

