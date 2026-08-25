library(shiny)
library(bslib)
library(plotly)
library(DT)
#library(dplyr)
library(tibble)
library(htmltools)


# ------------------------------------------------------------
# 2. COLOUR PALETTE
# ------------------------------------------------------------

navy <- "#123B56"
teal <- "#148C9E"
green <- "#3A9563"
orange <- "#F28C45"
red <- "#CF4B4B"
cream <- "#F7F5EF"
light_blue <- "#E4F3F5"
light_green <- "#E7F3EC"
light_orange <- "#FFF0E5"
grey <- "#5E6A71"


# ------------------------------------------------------------
# 3. SCIENTIFIC LITERATURE DATA
# ------------------------------------------------------------

literature_data <- tibble(
  Paper = c(
    "de Jong et al. (2015)",
    "Ahvenniemi et al. (2017)",
    "Yigitcanlar et al. (2019)",
    "Sharifi (2021)",
    "Bibri et al. (2023)",
    "Bibri et al. (2024)"
  ),
  
  Contribution = c(
    "Explains major urban-development concepts, including smart, sustainable, resilient, low-carbon and eco-cities.",
    "Shows that Smart Cities and Sustainable Cities have different priorities and indicators.",
    "Provides the central sustainability critique and warns against technology-centred urban development.",
    "Explains how sustainability should be assessed across environmental, social, economic and governance dimensions.",
    "Examines how AI, IoT and Big Data can support environmentally sustainable urban systems.",
    "Explains how AI, AIoT and Urban Digital Twins can work together in data-driven sustainable urban planning."
  ),
  
  App_role = c(
    "Urban concepts",
    "Smart versus sustainable",
    "Critical framework",
    "Assessment dimensions",
    "Technology applications",
    "Integrated future framework"
  )
)


# ------------------------------------------------------------
# 4. GERMAN CITY DATA
# ------------------------------------------------------------

city_data <- tibble(
  city = c(
    "Hamburg",
    "Berlin",
    "Munich",
    "Freiburg",
    "Cottbus"
  ),
  
  latitude = c(
    53.5511,
    52.5200,
    48.1351,
    47.9990,
    51.7563
  ),
  
  longitude = c(
    9.9937,
    13.4050,
    11.5820,
    7.8421,
    14.3329
  ),
  
  technology = c(
    "Urban Digital Twins, urban data platforms and smart port logistics",
    "Open data, digital public services and smart parking",
    "Smart mobility, digital planning and transport innovation",
    "Renewable energy, green infrastructure and sustainable mobility",
    "Energy transition, regional connectivity and public-service innovation"
  ),
  
  opportunity = c(
    "Improved logistics, climate adaptation, flood planning and transport coordination",
    "Improved access to data, more efficient mobility and transparent public services",
    "Integrated mobility planning, energy efficiency and improved public transport",
    "Low-carbon development, healthier neighbourhoods and climate resilience",
    "Support for post-coal transformation, regional development and service accessibility"
  ),
  
  risk = c(
    "High infrastructure costs, data dependence, cybersecurity and vendor lock-in",
    "Privacy concerns, surveillance, digital exclusion and rebound from easier driving",
    "High implementation costs, unequal access and dependence on private technology providers",
    "Green gentrification, unequal access to green areas and difficulty scaling local solutions",
    "Limited resources, digital skills gaps and unequal connectivity between urban and rural areas"
  ),
  
  indicators = c(
    "Avoided emissions; flood-risk reduction; public-transport accessibility; open-data use",
    "Car kilometres; emissions; service accessibility; privacy complaints; user diversity",
    "Public-transport use; congestion; energy consumption; accessibility",
    "Renewable-energy share; green-space access; cycling share; heat exposure",
    "Renewable-energy capacity; broadband access; service accessibility; regional employment"
  )
)


# ------------------------------------------------------------
# 5. TECHNOLOGY DATA
# ------------------------------------------------------------

technology_data <- tibble(
  technology = c(
    "Artificial Intelligence",
    "Internet of Things",
    "Big Data",
    "Urban Digital Twin",
    "Smart Grid",
    "Smart Parking",
    "Smart Mobility",
    "Smart Buildings",
    "Digital Public Services"
  ),
  
  description = c(
    "Algorithms that analyse data, recognise patterns, make predictions and support urban decision-making.",
    "Connected sensors and devices that collect and exchange real-time information.",
    "Large and complex datasets used to understand urban patterns and support evidence-based decisions.",
    "A dynamic virtual representation of a real object, infrastructure system or city.",
    "An electricity network that uses data and automated control to balance supply, demand and renewable energy.",
    "Sensors, cameras, signs or applications that identify and communicate available parking spaces.",
    "Digital systems that coordinate public transport, traffic, shared mobility, walking and cycling.",
    "Buildings that use sensors and automated controls to manage heating, cooling, lighting and energy.",
    "Online portals and applications through which residents access municipal information and services."
  ),
  
  direct = c(
    "Computing energy use and hardware demand",
    "Sensor production, electricity use and network infrastructure",
    "Data-centre electricity, storage and cooling demand",
    "Computing, sensors and data infrastructure",
    "Smart meters, communication devices and control equipment",
    "Parking sensors, cameras, applications and communication systems",
    "Sensors, platforms, GPS and communication infrastructure",
    "Sensors, controllers and building-management hardware",
    "Servers, devices, software and network infrastructure"
  ),
  
  indirect = c(
    "Better forecasting and optimisation of energy, transport and environmental services",
    "Real-time monitoring of traffic, air quality, water and energy",
    "Improved evidence for planning and resource allocation",
    "Scenario testing for mobility, climate adaptation, energy and land use",
    "Improved renewable-energy integration and demand management",
    "Less time searching for parking and potentially lower fuel consumption",
    "Better public transport information and network efficiency",
    "Reduced heating, cooling and lighting demand",
    "Faster access to public services and greater administrative efficiency"
  ),
  
  systemic = c(
    "Algorithmic decision-making may alter institutional power and accountability",
    "Pervasive sensing may increase surveillance and infrastructure dependence",
    "Data-driven governance may improve planning but concentrate control",
    "Planning may become more anticipatory but dependent on models and data providers",
    "Can enable energy transition but may create cybersecurity and technology dependence",
    "May reduce unnecessary driving or reinforce private-car dependence",
    "May support sustainable mobility or stimulate additional travel through rebound",
    "May reduce urban energy demand or encourage larger and more technology-intensive buildings",
    "May improve inclusion or exclude residents who cannot use digital services"
  )
)


# ------------------------------------------------------------
# 6. USER INTERFACE HELPERS
# ------------------------------------------------------------

section_heading <- function(title) {
  tags$h3(
    title,
    style = paste0(
      "color:", navy, ";",
      "font-weight:700;",
      "margin-top:5px;"
    )
  )
}


small_note <- function(text) {
  tags$p(
    text,
    style = paste0(
      "font-size:0.86rem;",
      "color:", grey, ";"
    )
  )
}


score_colour <- function(score) {
  if (score < 40) {
    red
  } else if (score < 60) {
    orange
  } else if (score < 80) {
    teal
  } else {
    green
  }
}


score_category <- function(score) {
  if (score < 40) {
    "Mainly digital, weak sustainability"
  } else if (score < 60) {
    "Significant sustainability risks"
  } else if (score < 80) {
    "Conditionally sustainable"
  } else {
    "Strong Smart Sustainable City potential"
  }
}


question_slider <- function(
    id,
    label,
    value = 3,
    min = 1,
    max = 5
) {
  sliderInput(
    inputId = id,
    label = label,
    min = min,
    max = max,
    value = value,
    step = 1
  )
}


# ------------------------------------------------------------
# 7. APPLICATION THEME
# ------------------------------------------------------------

app_theme <- bs_theme(
  version = 5,
  bg = cream,
  fg = navy,
  primary = teal,
  secondary = green,
  success = green,
  warning = orange,
  danger = red,
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Source Sans 3")
)


# ------------------------------------------------------------
# 8. USER INTERFACE
# ------------------------------------------------------------

ui <- page_navbar(
  title = "Smart Sustainable City Evaluator",
  theme = app_theme,
  fillable = TRUE,
  id = "main_nav",
  
  header = tags$head(
    tags$style(
      HTML(
        paste0(
          "
          body {
            background-color: ", cream, ";
          }

          .navbar {
            background-color: ", navy, " !important;
          }

          .navbar-brand,
          .navbar-nav .nav-link {
            color: white !important;
          }

          .card {
            border-radius: 14px;
            border: 1px solid #D8E1E4;
            box-shadow: 0 3px 10px rgba(0,0,0,0.06);
          }

          .hero-box {
            background: linear-gradient(120deg, ", navy, ", ", teal, ");
            color: white;
            padding: 35px;
            border-radius: 18px;
            margin-bottom: 20px;
          }

          .argument-box {
            background-color: ", light_green, ";
            border-left: 7px solid ", green, ";
            padding: 20px;
            border-radius: 10px;
          }

          .warning-box {
            background-color: ", light_orange, ";
            border-left: 7px solid ", orange, ";
            padding: 18px;
            border-radius: 10px;
          }

          .framework-step {
            padding: 12px;
            margin: 7px;
            text-align: center;
            border-radius: 9px;
            font-weight: 700;
          }

          .score-number {
            font-size: 2.5rem;
            font-weight: 800;
          }

          .small-source {
            font-size: 0.75rem;
            color: #66757C;
          }
          "
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 1: HOME
  # ==========================================================
  
  nav_panel(
    "Home",
    
    div(
      class = "hero-box",
      
      tags$h1(
        "Smart Sustainable City Evaluator",
        style = "font-weight:800;"
      ),
      
      tags$h4(
        "Can cities become smart without actually being sustainable?"
      ),
      
      tags$p(
        paste(
          "Assess whether a digital urban project contributes",
          "to measurable environmental, social, economic and",
          "governance outcomes."
        ),
        style = "font-size:1.15rem;"
      )
    ),
    
    layout_columns(
      col_widths = c(7, 5),
      
      card(
        card_header("Central argument"),
        
        div(
          class = "argument-box",
          
          tags$h4(
            "Technology is an enabler of sustainability",
            tags$strong("not sustainability itself.")
          ),
          
          tags$p(
            paste(
              "A city is not sustainable simply because it is",
              "digital. Digital tools must work together with",
              "public value, democratic governance and measurable",
              "sustainability outcomes."
            )
          )
        ),
        
        tags$br(),
        
        layout_columns(
          col_widths = c(12),
          
          div(
            class = "framework-step",
            style = paste0(
              "background:", light_blue, ";",
              "color:", navy, ";"
            ),
            "DIGITAL TOOLS"
          ),
          
          tags$div(
            style = "text-align:center;font-size:1.5rem;",
            "↓"
          ),
          
          div(
            class = "framework-step",
            style = paste0(
              "background:", "#E9F0FA", ";",
              "color:", navy, ";"
            ),
            "PUBLIC VALUE"
          ),
          
          tags$div(
            style = "text-align:center;font-size:1.5rem;",
            "↓"
          ),
          
          div(
            class = "framework-step",
            style = paste0(
              "background:", light_green, ";",
              "color:", green, ";"
            ),
            "SUSTAINABILITY OUTCOMES"
          )
        )
      ),
      
      card(
        card_header("How the app evaluates projects"),
        
        tags$ol(
          tags$li("Select a city or Smart City technology."),
          tags$li("Describe and score a proposed project."),
          tags$li("Evaluate environmental, social, economic and governance performance."),
          tags$li("Examine direct, indirect and systemic effects."),
          tags$li("Review recommendations and download the assessment.")
        ),
        
        div(
          class = "warning-box",
          
          tags$strong("Important:"),
          
          tags$p(
            paste(
              "The score is an educational analytical tool.",
              "It is not an official ranking or certification",
              "of a city or project."
            )
          )
        )
      )
    ),
    
    card(
      card_header("Scientific evidence base"),
      
      DTOutput("literature_table"),
      
    
    )
  ),
  
  
  # ==========================================================
  # TAB 2: GERMAN CITY EXPLORER
  # ==========================================================
  
  nav_panel(
    "German City Explorer",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        
        section_heading("Explore a city"),
        
        selectInput(
          inputId = "selected_city",
          label = "Select a German city:",
          choices = city_data$city,
          selected = "Hamburg"
        ),
        
        tags$hr(),
        
        tags$p(
          paste(
            "Click a city marker on the map or use the",
            "selection box above."
          )
        )
      ),
      
      layout_columns(
        col_widths = c(7, 5),
        
        card(
          full_screen = TRUE,
          card_header("German Smart City case studies"),
          plotlyOutput(
            "germany_map",
            height = "600px"
          )
        ),
        
        card(
          card_header(
            uiOutput("city_title")
          ),
          
          uiOutput("city_details")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 3: TECHNOLOGY EXPLORER
  # ==========================================================
  
  nav_panel(
    "Technology Explorer",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 330,
        
        section_heading("Select technology"),
        
        selectInput(
          inputId = "selected_technology",
          label = "Smart City technology:",
          choices = technology_data$technology,
          selected = "Urban Digital Twin"
        ),
        
        tags$hr(),
        
        div(
          class = "warning-box",
          
          tags$strong("Critical question"),
          
          tags$p(
            paste(
              "Does the technology solve a sustainability problem,",
              "or is technology itself being treated as the goal?"
            )
          )
        )
      ),
      
      card(
        card_header(
          uiOutput("technology_title")
        ),
        
        uiOutput("technology_description"),
        
        tags$hr(),
        
        layout_columns(
          col_widths = c(4, 4, 4),
          
          card(
            style = paste0(
              "border-top:5px solid ", orange, ";"
            ),
            card_header("Direct effects"),
            uiOutput("technology_direct")
          ),
          
          card(
            style = paste0(
              "border-top:5px solid ", teal, ";"
            ),
            card_header("Indirect effects"),
            uiOutput("technology_indirect")
          ),
          
          card(
            style = paste0(
              "border-top:5px solid ", navy, ";"
            ),
            card_header("Systemic effects"),
            uiOutput("technology_systemic")
          )
        ),
        
        tags$br(),
        
        DTOutput("technology_table")
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 4: PROJECT EVALUATOR
  # ==========================================================
  
  nav_panel(
    "Project Evaluator",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        
        section_heading("Project information"),
        
        textInput(
          "project_name",
          "Project name:",
          value = "Proposed Smart City Project"
        ),
        
        selectInput(
          "project_city",
          "City:",
          choices = city_data$city
        ),
        
        selectInput(
          "project_technology",
          "Main technology:",
          choices = technology_data$technology
        ),
        
        textAreaInput(
          "urban_problem",
          "What urban problem should the project solve?",
          value = "",
          rows = 4,
          placeholder = paste(
            "Example: Reduce transport emissions",
            "and improve access to public transport."
          )
        ),
        
        numericInput(
          "project_lifespan",
          "Expected project lifespan (years):",
          value = 10,
          min = 1,
          max = 50
        ),
        
        tags$hr(),
        
        actionButton(
          "calculate",
          "Calculate sustainability score",
          class = "btn-primary",
          width = "100%"
        )
      ),
      
      navset_card_tab(
        
        nav_panel(
          "Environment",
          
          section_heading("Environmental performance"),
          
          question_slider(
            "env_energy",
            "Does the project reduce total energy consumption?"
          ),
          
          question_slider(
            "env_emissions",
            "Does the project reduce greenhouse-gas emissions?"
          ),
          
          question_slider(
            "env_materials",
            "Are materials, hardware and lifecycle impacts considered?"
          ),
          
          question_slider(
            "env_ewaste",
            "Are repair, reuse and electronic-waste management included?"
          ),
          
          question_slider(
            "env_rebound",
            "Does the project include measures to prevent rebound effects?"
          )
        ),
        
        nav_panel(
          "Social",
          
          section_heading("Social equity and inclusion"),
          
          question_slider(
            "soc_access",
            "Is the project accessible to people with disabilities?"
          ),
          
          question_slider(
            "soc_digital",
            "Is a non-digital alternative available?"
          ),
          
          question_slider(
            "soc_equity",
            "Are benefits fairly distributed across neighbourhoods and groups?"
          ),
          
          question_slider(
            "soc_participation",
            "Were affected residents involved in project design?"
          ),
          
          question_slider(
            "soc_privacy",
            "Are privacy and surveillance risks adequately controlled?"
          )
        ),
        
        nav_panel(
          "Economic",
          
          section_heading("Economic sustainability"),
          
          question_slider(
            "eco_savings",
            "Will the project generate long-term public value or savings?"
          ),
          
          question_slider(
            "eco_local",
            "Does it support local skills, employment or innovation?"
          ),
          
          question_slider(
            "eco_maintenance",
            "Are maintenance and replacement costs affordable?"
          ),
          
          question_slider(
            "eco_vendor",
            "Can the municipality avoid dependence on one vendor?"
          ),
          
          question_slider(
            "eco_durability",
            "Can the system be repaired, upgraded and reused?"
          )
        ),
        
        nav_panel(
          "Governance",
          
          section_heading("Governance and accountability"),
          
          question_slider(
            "gov_ownership",
            "Is data ownership clearly defined?"
          ),
          
          question_slider(
            "gov_minimisation",
            "Is only necessary data collected?"
          ),
          
          question_slider(
            "gov_transparency",
            "Are algorithms and decisions transparent?"
          ),
          
          question_slider(
            "gov_oversight",
            "Is there democratic and independent oversight?"
          ),
          
          question_slider(
            "gov_open",
            "Does the project use open standards and interoperable systems?"
          )
        ),
        
        nav_panel(
          "Weights",
          
          section_heading("Choose assessment priorities"),
          
          tags$p(
            paste(
              "The weights represent value choices.",
              "The app automatically normalises them to 100%."
            )
          ),
          
          sliderInput(
            "weight_environment",
            "Environment:",
            min = 0,
            max = 100,
            value = 30
          ),
          
          sliderInput(
            "weight_social",
            "Social:",
            min = 0,
            max = 100,
            value = 25
          ),
          
          sliderInput(
            "weight_economic",
            "Economic:",
            min = 0,
            max = 100,
            value = 20
          ),
          
          sliderInput(
            "weight_governance",
            "Governance:",
            min = 0,
            max = 100,
            value = 25
          ),
          
          uiOutput("normalised_weights")
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 5: RESULTS
  # ==========================================================
  
  nav_panel(
    "Results",
    
    uiOutput("results_prompt"),
    
    conditionalPanel(
      condition = "output.results_available",
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        
        card(
          card_header("Environment"),
          uiOutput("environment_score")
        ),
        
        card(
          card_header("Social"),
          uiOutput("social_score")
        ),
        
        card(
          card_header("Economic"),
          uiOutput("economic_score")
        ),
        
        card(
          card_header("Governance"),
          uiOutput("governance_score")
        )
      ),
      
      layout_columns(
        col_widths = c(7, 5),
        
        card(
          full_screen = TRUE,
          card_header("Sustainability profile"),
          plotlyOutput(
            "radar_chart",
            height = "470px"
          )
        ),
        
        card(
          card_header("Overall assessment"),
          uiOutput("overall_score"),
          uiOutput("overall_category")
        )
      ),
      
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Direct, indirect and systemic assessment"),
          DTOutput("impact_table")
        ),
        
        card(
          card_header("Recommended actions"),
          uiOutput("recommendations")
        )
      ),
      
      card(
        card_header("Download assessment"),
        
        downloadButton(
          outputId = "download_report",
          label = "Download project assessment as CSV",
          class = "btn-success"
        ),
        
        tags$br(),
        tags$br(),
        
        small_note(
          paste(
            "The downloaded file records the project information,",
            "dimension scores, overall result and recommendations."
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # TAB 6: ABOUT
  # ==========================================================
  
  nav_panel(
    "About",
    
    card(
      card_header("About this application"),
      
      tags$p(
        paste(
          "The Smart Sustainable City Evaluator is an educational",
          "decision-support application developed from a literature-based",
          "assessment of Smart Cities in Germany."
        )
      ),
      
      tags$p(
        paste(
          "It evaluates individual Smart City projects."
        )
      ),
      
      tags$h4("Core principle"),
      
      tags$blockquote(
        "A project is not sustainable simply because it is digital. It becomes sustainable when measurable environmental, social, economic and governance benefits exceed its direct and systemic costs."
      ),
      
      tags$h4("Limitations"),
      
      tags$ul(
        tags$li(
          paste(
            "The scores depend on user responses and selected weights."
          )
        ),
        tags$li(
          paste(
            "The tool does not replace lifecycle assessment,",
            "social-impact assessment or legal review."
          )
        ),
        tags$li(
          paste(
            "City examples are illustrative and should be checked",
            "against current municipal strategies."
          )
        )
      )
    )
  )
)


# ------------------------------------------------------------
# 9. SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # LITERATURE TABLE
  # ----------------------------------------------------------
  
  output$literature_table <- renderDT({
    
    datatable(
      literature_data,
      rownames = FALSE,
      options = list(
        pageLength = 6,
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        autoWidth = TRUE
      ),
      class = "stripe hover compact"
    )
  })
  
  
  # ----------------------------------------------------------
  # CITY MAP
  # ----------------------------------------------------------
  
  output$germany_map <- renderPlotly({
    
    plot_ly(
      data = city_data,
      type = "scattergeo",
      mode = "markers+text",
      lon = ~longitude,
      lat = ~latitude,
      text = ~city,
      customdata = ~city,
      hovertemplate = paste0(
        "<b>%{text}</b><br>",
        "Technology: %{meta}<br>",
        "<extra></extra>"
      ),
      meta = ~technology,
      textposition = "top center",
      marker = list(
        size = 14,
        color = teal,
        line = list(
          color = navy,
          width = 2
        )
      ),
      source = "city_map"
    ) |>
      layout(
        geo = list(
          scope = "europe",
          projection = list(type = "mercator"),
          center = list(
            lon = 10.45,
            lat = 51.15
          ),
          lonaxis = list(
            range = c(5.5, 15.5)
          ),
          lataxis = list(
            range = c(47, 55.5)
          ),
          showland = TRUE,
          landcolor = "#F0F1EC",
          showcountries = TRUE,
          countrycolor = "#AAB4B8",
          showlakes = TRUE,
          lakecolor = "#E4F3F5"
        ),
        margin = list(
          l = 0,
          r = 0,
          t = 10,
          b = 0
        )
      )
  })
  
  observeEvent(
    event_data("plotly_click", source = "city_map"),
    {
      click <- event_data(
        "plotly_click",
        source = "city_map"
      )
      
      if (!is.null(click$customdata)) {
        updateSelectInput(
          session,
          "selected_city",
          selected = click$customdata
        )
      }
    }
  )
  
  selected_city_data <- reactive({
    
    city_data |>
      filter(city == input$selected_city)
  })
  
  
  output$city_title <- renderUI({
    
    tags$h3(
      selected_city_data()$city,
      style = paste0(
        "color:", navy, ";",
        "font-weight:800;"
      )
    )
  })
  
  
  output$city_details <- renderUI({
    
    city <- selected_city_data()
    
    tagList(
      
      tags$h5(
        "Main technologies",
        style = paste0("color:", teal, ";")
      ),
      tags$p(city$technology),
      
      tags$h5(
        "Sustainability opportunities",
        style = paste0("color:", green, ";")
      ),
      tags$p(city$opportunity),
      
      tags$h5(
        "Main risks",
        style = paste0("color:", red, ";")
      ),
      tags$p(city$risk),
      
      tags$h5(
        "Recommended indicators",
        style = paste0("color:", orange, ";")
      ),
      tags$p(city$indicators),
      
      div(
        class = "warning-box",
        tags$strong("Interpretation"),
        tags$p(
          paste(
            "The city should not be evaluated by the amount",
            "of technology deployed. Its performance should be",
            "judged by measurable net sustainability outcomes."
          )
        )
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # TECHNOLOGY EXPLORER
  # ----------------------------------------------------------
  
  selected_technology_data <- reactive({
    
    technology_data |>
      filter(
        technology == input$selected_technology
      )
  })
  
  
  output$technology_title <- renderUI({
    
    tags$h3(
      selected_technology_data()$technology,
      style = paste0(
        "color:", navy, ";",
        "font-weight:800;"
      )
    )
  })
  
  
  output$technology_description <- renderUI({
    
    tags$p(
      selected_technology_data()$description,
      style = "font-size:1.1rem;"
    )
  })
  
  
  output$technology_direct <- renderUI({
    
    tags$p(selected_technology_data()$direct)
  })
  
  
  output$technology_indirect <- renderUI({
    
    tags$p(selected_technology_data()$indirect)
  })
  
  
  output$technology_systemic <- renderUI({
    
    tags$p(selected_technology_data()$systemic)
  })
  
  
  output$technology_table <- renderDT({
    
    datatable(
      technology_data,
      rownames = FALSE,
      options = list(
        pageLength = 9,
        scrollX = TRUE,
        autoWidth = TRUE
      ),
      filter = "top"
    )
  })
  
  
  # ----------------------------------------------------------
  # NORMALISED WEIGHTS
  # ----------------------------------------------------------
  
  assessment_weights <- reactive({
    
    raw_weights <- c(
      Environment = input$weight_environment,
      Social = input$weight_social,
      Economic = input$weight_economic,
      Governance = input$weight_governance
    )
    
    if (sum(raw_weights) == 0) {
      raw_weights <- c(
        Environment = 25,
        Social = 25,
        Economic = 25,
        Governance = 25
      )
    }
    
    100 * raw_weights / sum(raw_weights)
  })
  
  
  output$normalised_weights <- renderUI({
    
    weights <- assessment_weights()
    
    tags$div(
      class = "argument-box",
      
      tags$strong("Normalised weights:"),
      
      tags$p(
        paste0(
          "Environment: ", round(weights["Environment"], 1), "% | ",
          "Social: ", round(weights["Social"], 1), "% | ",
          "Economic: ", round(weights["Economic"], 1), "% | ",
          "Governance: ", round(weights["Governance"], 1), "%"
        )
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # CALCULATE SCORES
  # ----------------------------------------------------------
  
  calculated_results <- eventReactive(
    input$calculate,
    {
      
      environmental_answers <- c(
        input$env_energy,
        input$env_emissions,
        input$env_materials,
        input$env_ewaste,
        input$env_rebound
      )
      
      social_answers <- c(
        input$soc_access,
        input$soc_digital,
        input$soc_equity,
        input$soc_participation,
        input$soc_privacy
      )
      
      economic_answers <- c(
        input$eco_savings,
        input$eco_local,
        input$eco_maintenance,
        input$eco_vendor,
        input$eco_durability
      )
      
      governance_answers <- c(
        input$gov_ownership,
        input$gov_minimisation,
        input$gov_transparency,
        input$gov_oversight,
        input$gov_open
      )
      
      # Convert average score from the 1–5 scale to 0–100.
      score_dimension <- function(values) {
        (mean(values) - 1) / 4 * 100
      }
      
      dimension_scores <- c(
        Environment = score_dimension(environmental_answers),
        Social = score_dimension(social_answers),
        Economic = score_dimension(economic_answers),
        Governance = score_dimension(governance_answers)
      )
      
      weights <- assessment_weights()
      
      overall <- sum(
        dimension_scores * weights / 100
      )
      
      list(
        scores = dimension_scores,
        weights = weights,
        overall = overall,
        category = score_category(overall)
      )
    }
  )
  
  
  output$results_available <- reactive({
    input$calculate > 0
  })
  
  outputOptions(
    output,
    "results_available",
    suspendWhenHidden = FALSE
  )
  
  
  output$results_prompt <- renderUI({
    
    if (input$calculate == 0) {
      
      div(
        class = "warning-box",
        
        tags$h4("No assessment has been calculated yet."),
        
        tags$p(
          paste(
            "Complete the Project Evaluator questionnaire",
            "and select “Calculate sustainability score”."
          )
        )
      )
    }
  })
  
  
  # ----------------------------------------------------------
  # SCORE CARDS
  # ----------------------------------------------------------
  
  create_score_ui <- function(score) {
    
    tags$div(
      style = "text-align:center;",
      
      tags$div(
        class = "score-number",
        style = paste0(
          "color:", score_colour(score), ";"
        ),
        paste0(round(score), "/100")
      ),
      
      tags$p(score_category(score))
    )
  }
  
  
  output$environment_score <- renderUI({
    
    req(calculated_results())
    
    create_score_ui(
      calculated_results()$scores["Environment"]
    )
  })
  
  
  output$social_score <- renderUI({
    
    req(calculated_results())
    
    create_score_ui(
      calculated_results()$scores["Social"]
    )
  })
  
  
  output$economic_score <- renderUI({
    
    req(calculated_results())
    
    create_score_ui(
      calculated_results()$scores["Economic"]
    )
  })
  
  
  output$governance_score <- renderUI({
    
    req(calculated_results())
    
    create_score_ui(
      calculated_results()$scores["Governance"]
    )
  })
  
  
  output$overall_score <- renderUI({
    
    req(calculated_results())
    
    overall <- calculated_results()$overall
    
    tags$div(
      style = "text-align:center;",
      
      tags$div(
        class = "score-number",
        style = paste0(
          "color:", score_colour(overall), ";"
        ),
        paste0(round(overall), "/100")
      )
    )
  })
  
  
  output$overall_category <- renderUI({
    
    req(calculated_results())
    
    overall <- calculated_results()$overall
    
    div(
      class = "argument-box",
      
      tags$h4(
        calculated_results()$category
      ),
      
      tags$p(
        if (overall >= 80) {
          paste(
            "The project demonstrates strong sustainability potential.",
            "Continue monitoring lifecycle impacts, equity, rebound",
            "and long-term governance."
          )
        } else if (overall >= 60) {
          paste(
            "The project has meaningful sustainability potential,",
            "but important weaknesses must be addressed before",
            "implementation."
          )
        } else if (overall >= 40) {
          paste(
            "The project contains substantial risks or missing",
            "sustainability safeguards. Redesign is recommended."
          )
        } else {
          paste(
            "The proposal appears primarily technology-driven.",
            "The sustainability purpose, governance and distribution",
            "of benefits should be reconsidered."
          )
        }
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # RADAR CHART
  # ----------------------------------------------------------
  
  output$radar_chart <- renderPlotly({
    
    req(calculated_results())
    
    scores <- calculated_results()$scores
    
    categories <- names(scores)
    
    plot_values <- c(
      unname(scores),
      unname(scores[1])
    )
    
    plot_categories <- c(
      categories,
      categories[1]
    )
    
    plot_ly(
      type = "scatterpolar",
      r = plot_values,
      theta = plot_categories,
      fill = "toself",
      name = input$project_name,
      line = list(
        color = teal,
        width = 3
      ),
      fillcolor = "rgba(20,140,158,0.30)"
    ) |>
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            tickvals = c(0, 20, 40, 60, 80, 100)
          )
        ),
        showlegend = FALSE,
        margin = list(
          l = 60,
          r = 60,
          t = 40,
          b = 40
        )
      )
  })
  
  
  # ----------------------------------------------------------
  # IMPACT TABLE
  # ----------------------------------------------------------
  
  output$impact_table <- renderDT({
    
    req(calculated_results())
    
    selected <- technology_data |>
      filter(
        technology == input$project_technology
      )
    
    impact_data <- tibble(
      Effect_level = c(
        "Direct",
        "Indirect",
        "Systemic"
      ),
      
      Interpretation = c(
        selected$direct,
        selected$indirect,
        selected$systemic
      )
    )
    
    datatable(
      impact_data,
      rownames = FALSE,
      options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = TRUE
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # RECOMMENDATION GENERATOR
  # ----------------------------------------------------------
  
  recommendation_data <- reactive({
    
    req(calculated_results())
    
    scores <- calculated_results()$scores
    
    recommendations <- character(0)
    
    if (scores["Environment"] < 60) {
      recommendations <- c(
        recommendations,
        "Conduct a lifecycle assessment covering hardware, energy, materials, maintenance and electronic waste.",
        "Measure total emissions and resource use rather than only operational efficiency.",
        "Introduce indicators for rebound effects and avoided consumption."
      )
    }
    
    if (scores["Social"] < 60) {
      recommendations <- c(
        recommendations,
        "Co-design the project with affected residents and vulnerable groups.",
        "Provide accessible and non-digital alternatives for essential public services.",
        "Assess privacy, surveillance and unequal distribution of benefits."
      )
    }
    
    if (scores["Economic"] < 60) {
      recommendations <- c(
        recommendations,
        "Prepare a full lifecycle cost and maintenance plan.",
        "Use modular, repairable and upgradeable infrastructure.",
        "Include an exit strategy to prevent dependence on one technology vendor."
      )
    }
    
    if (scores["Governance"] < 60) {
      recommendations <- c(
        recommendations,
        "Clarify data ownership, purpose, retention and accountability.",
        "Use open standards and transparent procurement conditions.",
        "Create democratic oversight and an independent complaints mechanism."
      )
    }
    
    if (all(scores >= 60)) {
      recommendations <- c(
        recommendations,
        "Maintain transparent monitoring of environmental, social, economic and governance indicators.",
        "Regularly test whether efficiency gains are producing rebound effects.",
        "Review performance with affected residents throughout the project lifecycle.",
        "Publish open, accessible reports on costs, benefits and risks."
      )
    }
    
    unique(recommendations)
  })
  
  
  output$recommendations <- renderUI({
    
    req(calculated_results())
    
    recommendation_list <- recommendation_data()
    
    tags$div(
      tags$ul(
        lapply(
          recommendation_list,
          tags$li
        )
      ),
      
      div(
        class = "warning-box",
        
        tags$strong("Decision rule:"),
        
        tags$p(
          paste(
            "Outcomes should be specified before technology",
            "is procured. A project should proceed only when",
            "its measurable public and sustainability benefits",
            "are likely to exceed its lifecycle and systemic costs."
          )
        )
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # DOWNLOAD REPORT
  # ----------------------------------------------------------
  
  output$download_report <- downloadHandler(
    
    filename = function() {
      
      safe_name <- gsub(
        "[^A-Za-z0-9_-]",
        "_",
        input$project_name
      )
      
      paste0(
        safe_name,
        "_Smart_City_Assessment.csv"
      )
    },
    
    content = function(file) {
      
      req(calculated_results())
      
      scores <- calculated_results()$scores
      
      recommendations_text <- paste(
        recommendation_data(),
        collapse = " | "
      )
      
      report <- tibble(
        Field = c(
          "Project name",
          "City",
          "Technology",
          "Urban problem",
          "Expected lifespan",
          "Environmental score",
          "Social score",
          "Economic score",
          "Governance score",
          "Overall score",
          "Assessment category",
          "Recommendations",
          "Disclaimer"
        ),
        
        Value = c(
          input$project_name,
          input$project_city,
          input$project_technology,
          input$urban_problem,
          paste(input$project_lifespan, "years"),
          round(scores["Environment"], 1),
          round(scores["Social"], 1),
          round(scores["Economic"], 1),
          round(scores["Governance"], 1),
          round(calculated_results()$overall, 1),
          calculated_results()$category,
          recommendations_text,
          paste(
            "Educational analytical assessment;",
            "not an official certification or city ranking."
          )
        )
      )
      
      write.csv(
        report,
        file,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  )
  
  
  # ----------------------------------------------------------
  # MOVE USER TO RESULTS AFTER CALCULATION
  # ----------------------------------------------------------
  
  observeEvent(input$calculate, {
    
    updateNavbarPage(
      session = session,
      inputId = "main_nav",
      selected = "Results"
    )
  })
}


# ------------------------------------------------------------
# 10. RUN APPLICATION
# ------------------------------------------------------------

shinyApp(
  ui = ui,
  server = server
)
