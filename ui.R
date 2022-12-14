ui <- fullPage(
  
  opts = list(
    controlArrows = FALSE,
    fadingEffect = TRUE,
    fitToSection = TRUE,
    loopBottom = FALSE,
    loopHorizontal = TRUE,
    navigation = FALSE,
    scrollBar = FALSE,
    scrollOverflow = TRUE,
    scrollOverflowReset = TRUE,
    slidesNavigation = TRUE,
    verticalCentered = TRUE
  ),
  
  # menu tabs----
  menu = c(
    'Home' = 'home',
    'RFM Static Visualization' = 'visualization',
    'Analytics' = 'analytics',
    #'Recommendations' = 'recommendation',
    'About Us' = 'about'
  ),
  # home section----
  fullSection(
    menu = 'home',
    center = TRUE,
      fullSlideImage(
          img = 'webhero-home.jpeg',
          div(
              style = 'padding:10vh 25% 0 25%;color:#e3af32;text-align:center;',
              tags$button(
              id = "btnhome",
              class = 'btn action-button',
              style = 'background-color:rgba(0,0,0,0);',
              img(
                  src = paste0('logo_1.svg'),
                  width = '60%',
                  style = ''
              ),
              onclick ="window.open('https://www.ab-inbev.com/','_blank','resizable,height=260,width=370')"
            ),
              hr(),
              h3('#FUTUREWITHMORECHEERS')
      )
    )
  ),
  fullSection(
    menu = 'visualization',
    center = TRUE,
    uiOutput("visualization_ui")

  ),
  fullSection(
    menu = 'analytics',
    center = TRUE,
    uiOutput("ana_ui")
    

  ),

  # fullSection(
  #   menu = 'recommendation',
  #   center = TRUE,
  #   fullSlideImage(
  #     img = 'AB-InBev-logo.jpeg'
  #   )
  # ),
  fullSection(
    menu = 'about',
    center = TRUE,
    fullSlideImage(
      img = '',
          div(
            style = 'padding:0 17% 0 17%; align:center;',
            br(),
            h1("Meet Our Team", style = "color:gray;"),br(),br(),
            hr(),
            tags$img(src = "pictures_team.png", width = "1000px")
          ),
      div(
        style = 'padding:0 17% 0 17%;',
        br(),br(),
        h3(style = 'color:black;', ''),
        hr()
       )
      )
    )
)