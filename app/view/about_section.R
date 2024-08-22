box::use(
  shiny[NS, div, h4, icon, a, img, tagList, callModule, observeEvent, showModal, modalDialog, hr]
)

topic_section <- function(
    header,
    description) {
  div(
    h4(class = "about-header", header),
    div(
      class = "about-descr",
      description
    )
  )
}

tag <- function(tag_string) {
  div(
    class = "tag-item",
    icon("link"),
    tag_string
  )
}

card <- function(
    href_link,
    img_link,
    card_header,
    card_text) {
  div(
    class = "card-package",
    a(
      class = "card-img",
      href = href_link,
      target = "_blank",
      rel = "noopener noreferrer",
      img(
        src = img_link,
        alt = card_header
      )
    ),
    div(
      class = "card-heading",
      card_header
    ),
    div(
      class = "card-content",
      card_text
    ),
    div(
      class = "card-footer",
      a(
        href = href_link,
        target = "_blank",
        rel = "noopener noreferrer",
        "Learn more"
      )
    )
  )
}

empty_card <- function() {
  div(
    class = "card-empty",
    a(
      href = "https://demo.appsilon.com/",
      target = "_blank",
      rel = "noopener noreferrer",
      shiny::icon("circle-arrow-right"),
      div(
        class = "card-empty-caption",
        "Learn about our technologies"
      )
    )
  )
}

appsilon <- function() {
  div(
    class = "appsilon-card",
    div(
      class = "appsilon-pic",
      a(
        href = "https://appsilon.com/",
        target = "_blank",
        rel = "noopener noreferrer",
        img(
          src = "static/appsilon-logo.png",
          alt = "Appsilon"
        )
      )
    ),
    div(
      class = "appsilon-summary",
      "We create, maintain, and develop Shiny applications
      for enterprise customers all over the world. Appsilon
      provides scalability, security, and modern UI/UX with
      custom R packages that native Shiny apps do not provide.
      Our team is among the world’s foremost experts in R Shiny
      and has made a variety of Shiny innovations over the
      years. Appsilon is a proud Posit Full Service
      Certified Partner."
    )
  )
}

ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = ns("info"),
      icon("info-circle")
    )
  )
}

init_server <- function(id) {
  callModule(server, id)
}

server <- function(input, output, session) {
  ns <- session$ns

  observeEvent(input$open_modal, {
    showModal(
      modalDialog(
        easyClose = TRUE,
        title = "Shiny Enterprise Dashboard App",
        div(
          class = "about-section",
          topic_section(
            header = "About the project",
            description = "The Shiny Enterprise Dashboard is a mock sales metrics
            dashboard for a global enterprise. By presenting key metrics
            in a concise and ease to to consume way, dashboard users can
            quickly identify flags or track KPIs including sales, production,
            users and complaints, and more. Stakeholders have access to
            the information they need, and are able to customize their view."
          ),
          topic_section(
            header = "Dataset Info",
            description = "The dashboard contains cross-sectional data
            of an e-commerce business. Temporal data spans 6 years and
            drills down to the monthly level. Spatial data is available
            at the country level. Key metrics include sales revenue,
            profit, orders, production cost and items produced, user
            details, and customer reports."
          ),
          hr(),
          appsilon()
        )
      )
    )
  })
}
