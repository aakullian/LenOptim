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
        selected = "ZAF"
      ),

      numericInput(
        "total_courses",
        "Total Len Courses (person-years)",
        value = 500000,
        min = 1000,
        step = 10000
      ),
      uiOutput("max_courses_text"),

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
        selected = "15-24"
      ),

      checkboxGroupInput(
        "sex",
        "Eligible Sex Groups",
        choices = SEX_OPTIONS,
        selected = "female"
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
          "About",
          br(),
          fluidRow(
            column(10, offset = 1,
              h3("Lenacapavir PrEP Allocation Optimizer"),
              p(style = "font-size: 15px; line-height: 1.7;",
                "This tool optimizes the sub-national allocation of ",
                "Lenacapavir (Len) for HIV prevention (PrEP) ",
                "across 11 countries in sub-Saharan Africa. Given a fixed supply of Len courses, ",
                "it determines which districts, age groups, sex groups, and risk strata should receive ",
                "allocation to maximize HIV infections averted.",
                "This model only estimates the short-term, direct impact of HIV PrEP",
                "and does not include dynamics to estimate secondary infections averted, including mother-to-child",
                "transmissions."
              ),

              tags$p(style = "font-size: 14px; font-weight: bold;", "How the Model Works"),
              tags$ul(style = "font-size: 13px; line-height: 1.8;",
                tags$li("District-level HIV incidence, prevalence, and population estimates are drawn from the ",
                        "UNAIDS Naomi model (2024 estimates)."),
                tags$li("Within each district-, sex-, and age-group individual-level risk heterogeneity is simulated using a ",
                        "gamma distribution parameterized by the district mean incidence, ",
                        "then stratified into risk quantiles (1, 4, or 8 groups). ",
                        "Risk heterogeneity is assumed to be the same across all strata."),
                tags$li("All population strata across all districts are ranked by descending incidence."),
                tags$li("Len courses are allocated top-down to the highest-risk strata first, ",
                        "until the supply is exhausted or the coverage cap is reached in each stratum.")
              ),

              tags$p(style = "font-size: 14px; font-weight: bold;", "How to Use This Dashboard"),
              tags$ul(style = "font-size: 13px; line-height: 1.8;",
                tags$li("Select a country and set the total Len courses available and cost per course."),
                tags$li("Choose eligible populations by age group and sex."),
                tags$li("Set within-district risk resolution (1 = district average, 4 or 8 = finer risk targeting). ",
                        "Then use the slider to restrict allocation to only the highest-risk percentiles if desired."),
                tags$li("Adjust coverage cap to limit the maximum fraction of any target population that receives Len. ",
                        "This reflects real-world uptake constraints."),
                tags$li("Click Run Model to generate results.")
              ),

              tags$p(style = "font-size: 14px; font-weight: bold;", "Output Tabs"),
              tags$ul(style = "font-size: 13px; line-height: 1.8;",
                tags$li("Maps -- Three choropleth maps showing which districts receive Len, ",
                        "the percentage of the population covered, and the percentage reduction in incidence."),
                tags$li("Summary -- Key metrics including infections averted, cost-effectiveness, ",
                        "NNT, PrEP coverage, and the incidence targeting ratio."),
                tags$li("District Detail -- Allocation breakdown by district, age, and sex (downloadable)."),
                tags$li("Volume Finder -- An interactive curve showing how many Len courses are needed ",
                        "to achieve any target incidence reduction. Set a target % and get the required volume and cost."),
                tags$li("Scenario Comparison -- Save multiple model runs and compare them side by side.")
              ),

              tags$p(style = "font-size: 14px; font-weight: bold;", "Notes"),
              p(style = "font-size: 13px;",
                "The default parameters (women 15-24, 4 risk quantiles, top 25% risk targeted) reflect PrEP uptake assumptions ",
                "among women offered PrEP without restriction -- i.e., those most likely to initiate and benefit are in the highest ",
                "risk quartile within their district."
              ),
              p(style = "font-size: 13px;",
                "Within-district risk resolution should be set according to how well Lenacapavir delivery programs can identify ",
                "and prioritize high-risk groups. If set to 1, the model assumes uptake by the average-risk group, whereas if set to 4, the model ",
                "can prioritize to smaller groups with higher risk. ",
                "The risk distribution ",
                "cutoff restricts the model to only target groups with risk higher than the set threshold. Setting a higher threshold will expand ",
                "allocation to more geographies and may skip over lower-risk groups in high-risk geographies, resulting in a sub-optimal allocation."
              ),

              tags$p(style = "font-size: 14px; font-weight: bold;", "Supported Countries"),
              p(style = "font-size: 13px;",
                "Botswana, Eswatini, Kenya, Lesotho, Malawi, Mozambique, South Africa, Tanzania, Uganda, Zambia, Zimbabwe"
              ),

              tags$p(style = "font-size: 14px; font-weight: bold;", "Data Sources"),
              p(style = "font-size: 13px; line-height: 1.7;",
                "District-level estimates are from the ",
                tags$a(href = "https://naomi-spectrum.unaids.org/", target = "_blank",
                       "UNAIDS Naomi model"),
                " (naomi3_2024_07_01), which provides subnational HIV incidence, ",
                "prevalence, and population size estimates for sub-Saharan Africa. Shapefiles are from the ",
                "Naomi combined subnational dataset (2024)."
              ),

              tags$p(style = "font-size: 14px; font-weight: bold;", "Reference"),
              p(style = "font-size: 13px; line-height: 1.7;",
                "Akullian A, Imai-Eaton JW, Sharma M, Subedar H, O'Brien M, Garnett G. ",
                tags$em("Health impact and cost-effectiveness of geographically prioritized long-acting PrEP delivery in southern and eastern Africa."),
                " medRxiv 2026. ",
                tags$a(href = "https://www.medrxiv.org/content/10.1101/2026.01.01.345396v1", target = "_blank",
                       "doi:10.1101/2026.01.01.345396v1")
              ),

              hr(),
              p(style = "font-size: 12px; color: #888;",
                "To get started, configure parameters in the sidebar and click 'Run Model', then explore the output tabs."
              )
            )
          )
        ),

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
          "Volume Finder",
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
