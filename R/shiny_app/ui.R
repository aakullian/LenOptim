# ui.R
# Shiny UI for LenOptim dashboard

fluidPage(
  titlePanel("Lenacapavir PrEP Allocation Optimizer"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Model Parameters"),

      selectInput(
        "country",
        "Country",
        choices = SUPPORTED_COUNTRIES,
        selected = "ZMB"
      ),

      numericInput(
        "total_courses",
        "Total Len Courses (person-years)",
        value = 500000,
        min = 1000,
        step = 10000
      ),

      numericInput(
        "cost_per_course",
        "Cost per Course (USD)",
        value = 55,
        min = 1,
        step = 5
      ),

      hr(),

      checkboxGroupInput(
        "age_groups",
        "Eligible Age Groups",
        choices = AGE_GROUP_OPTIONS,
        selected = AGE_GROUP_OPTIONS
      ),

      checkboxGroupInput(
        "sex",
        "Eligible Sex Groups",
        choices = SEX_OPTIONS,
        selected = SEX_OPTIONS
      ),

      hr(),

      radioButtons(
        "risk_groups",
        "Within-District Risk Resolution",
        choices = c("1 (district average)" = 1, "4 quantiles" = 4, "8 quantiles" = 8),
        selected = 4,
        inline = TRUE
      ),

      # Dynamic risk targeting slider (hidden for 1 risk group)
      uiOutput("risk_targeting_ui"),

      # Show availability indicator
      uiOutput("data_availability"),

      hr(),

      sliderInput(
        "coverage_cap",
        "Max Coverage of Target Population (%)",
        min = 10,
        max = 100,
        value = 75,
        step = 5,
        post = "%"
      ),

      numericInput(
        "efficacy",
        "Len Efficacy",
        value = 0.95,
        min = 0.01,
        max = 1.0,
        step = 0.01
      ),

      hr(),

      actionButton("run_model", "Run Model", class = "btn-primary btn-lg", width = "100%"),

      hr(),

      h4("Scenario Comparison"),
      actionButton("save_scenario", "Save Current to Comparison", width = "100%"),
      actionButton("clear_scenarios", "Clear Saved Scenarios", width = "100%"),
      textOutput("scenario_count")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",

        tabPanel(
          "Maps",
          br(),
          conditionalPanel(
            condition = "output.has_results",
            plotOutput("allocation_maps", height = "700px", width = "100%")
          ),
          conditionalPanel(
            condition = "!output.has_results",
            div(
              style = "text-align: center; padding: 100px; color: #888;",
              h3("Select parameters and click 'Run Model' to generate allocation maps.")
            )
          )
        ),

        tabPanel(
          "Summary",
          br(),
          conditionalPanel(
            condition = "output.has_results",
            h4("Scenario Summary"),
            DT::dataTableOutput("summary_table")
          ),
          conditionalPanel(
            condition = "!output.has_results",
            div(
              style = "text-align: center; padding: 100px; color: #888;",
              h3("Run the model to see summary results.")
            )
          )
        ),

        tabPanel(
          "District Detail",
          br(),
          conditionalPanel(
            condition = "output.has_results",
            h4("Allocation by District, Age, and Sex"),
            DT::dataTableOutput("district_detail")
          ),
          conditionalPanel(
            condition = "!output.has_results",
            div(
              style = "text-align: center; padding: 100px; color: #888;",
              h3("Run the model to see district-level allocation details.")
            )
          )
        ),

        tabPanel(
          "Dose Finder",
          br(),
          conditionalPanel(
            condition = "output.has_dose_curve",
            fluidRow(
              column(4,
                sliderInput(
                  "target_reduction",
                  "Target Incidence Reduction (%)",
                  min = 1,
                  max = 50,
                  value = 10,
                  step = 1,
                  post = "%"
                ),
                wellPanel(
                  h4("Required Volume"),
                  uiOutput("dose_finder_result")
                )
              ),
              column(8,
                plotOutput("dose_response_plot", height = "550px")
              )
            ),
            hr(),
            h4("Dose-Response Data"),
            DT::dataTableOutput("dose_response_table")
          ),
          conditionalPanel(
            condition = "!output.has_dose_curve",
            div(
              style = "text-align: center; padding: 100px; color: #888;",
              h3("Click 'Run Model' to compute the dose-response curve."),
              p("The Dose Finder shows how many Len courses are needed to achieve any target incidence reduction.")
            )
          )
        ),

        tabPanel(
          "Scenario Comparison",
          br(),
          conditionalPanel(
            condition = "output.has_saved_scenarios",
            h4("Saved Scenario Comparison"),
            DT::dataTableOutput("scenario_comparison")
          ),
          conditionalPanel(
            condition = "!output.has_saved_scenarios",
            div(
              style = "text-align: center; padding: 100px; color: #888;",
              h3("Run models and click 'Save Current to Comparison' to compare scenarios.")
            )
          )
        )
      )
    )
  )
)
