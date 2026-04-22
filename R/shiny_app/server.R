# server.R
# Shiny server logic for LenOptim dashboard

function(input, output, session) {

  # Reactive values to store model results, dose curve, and saved scenarios
  model_results <- reactiveVal(NULL)
  dose_curve_data <- reactiveVal(NULL)
  scenario_log <- reactiveValues(scenarios = list())

  # --- Cached data loader (updates when country or risk groups change) ---
  cached_data <- reactive({
    load_country_data(
      country_iso = input$country,
      risk_groups = as.integer(input$risk_groups),
      data_dir = DATA_DIR
    )
  })

  # --- Max eligible courses (updates live as parameters change) ---
  max_eligible_courses <- reactive({
    data <- cached_data()
    if (!is.null(data$error)) return(NULL)

    inc_df <- data$incidence_df
    rg <- as.integer(input$risk_groups)
    cov <- input$coverage_cap / 100
    min_rq <- get_min_risk_quantile()
    eligible_ages <- input$age_groups
    eligible_sex <- input$sex

    if (length(eligible_ages) == 0 || length(eligible_sex) == 0) return(0)

    # Filter to eligible strata and sum population
    eligible_pop <- inc_df %>%
      mutate(age_clean = recode(age_group_label, "50-99" = "50+")) %>%
      filter(
        tolower(sex) %in% tolower(eligible_sex),
        age_clean %in% eligible_ages | age_group_label %in% eligible_ages,
        quant_target > min_rq
      ) %>%
      summarise(total_pop = sum(pop_subsample, na.rm = TRUE)) %>%
      pull(total_pop)

    ceiling(eligible_pop * cov)
  })

  output$max_courses_text <- renderUI({
    mc <- max_eligible_courses()
    if (is.null(mc)) {
      tags$div(style = "color: red; font-size: 12px; margin-top: 3px;",
               "Data not available for this country/risk group combination.")
    } else {
      requested <- input$total_courses
      color <- if (requested > mc) "red" else "#888"
      tagList(
        tags$div(style = paste0("color: ", color, "; font-size: 12px; margin-top: 3px;"),
                 paste0("Max eligible: ", format(mc, big.mark = ","), " courses")),
        if (requested > mc)
          tags$div(style = "color: red; font-size: 11px;",
                   paste0("(Requested exceeds eligible population by ",
                          format(requested - mc, big.mark = ","), " courses)"))
      )
    }
  })

  # --- Data availability indicator ---
  output$data_availability <- renderUI({
    country <- input$country
    rg <- as.integer(input$risk_groups)
    available <- nrow(AVAILABLE_COMBOS %>%
                        filter(country == !!country, risk_groups == !!rg)) > 0
    if (available) {
      tags$div(
        style = "color: green; font-weight: bold; margin-top: 5px;",
        icon("check-circle"), "Data available"
      )
    } else {
      tags$div(
        style = "color: red; font-weight: bold; margin-top: 5px;",
        icon("times-circle"), "Data not yet generated"
      )
    }
  })

  # --- Condition flags for conditional panels ---
  output$has_results <- reactive({ !is.null(model_results()) })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  output$has_saved_scenarios <- reactive({ length(scenario_log$scenarios) > 0 })
  outputOptions(output, "has_saved_scenarios", suspendWhenHidden = FALSE)

  output$has_dose_curve <- reactive({ !is.null(dose_curve_data()) })
  outputOptions(output, "has_dose_curve", suspendWhenHidden = FALSE)

  # --- Dynamic risk targeting slider + visual ---
  output$risk_targeting_ui <- renderUI({
    rg <- as.integer(input$risk_groups)
    if (rg == 1) {
      helpText("With 1 risk group, all individuals within each district are treated as equal risk.")
    } else {
      step_size <- 100 / rg  # 25 for 4 groups, 12.5 for 8 groups
      tagList(
        sliderInput(
          "min_risk_percentile",
          "Risk Distribution Cutoff",
          min = 0,
          max = 100,
          value = if (rg == 4) 75 else 0,
          step = step_size,
          post = "%"
        ),
        plotOutput("risk_targeting_visual", height = "60px"),
        uiOutput("risk_targeting_label")
      )
    }
  })

  # Visual bar showing targeted range
  output$risk_targeting_visual <- renderPlot({
    req(input$min_risk_percentile)
    rg <- as.integer(input$risk_groups)
    cutoff <- input$min_risk_percentile
    step_size <- 100 / rg

    # Build quantile blocks
    breaks <- seq(0, 100, by = step_size)
    blocks <- data.frame(
      xmin = breaks[-length(breaks)],
      xmax = breaks[-1],
      targeted = breaks[-length(breaks)] >= cutoff & cutoff < 100
    )

    ggplot(blocks) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, fill = targeted),
                color = "white", linewidth = 0.8) +
      scale_fill_manual(values = c("TRUE" = "#d63031", "FALSE" = "#dfe6e9"), guide = "none") +
      scale_x_continuous(
        breaks = breaks,
        labels = paste0(breaks, "%"),
        expand = c(0, 0)
      ) +
      annotate("text", x = 50, y = 1.35,
               label = "\u2190 Lower risk                    Higher risk \u2192",
               size = 3.5, color = "grey40") +
      coord_cartesian(ylim = c(0, 1.5), clip = "off") +
      theme_void(base_size = 10) +
      theme(
        axis.text.x = element_text(size = 8, margin = margin(t = 2)),
        plot.margin = margin(t = 0, r = 5, b = 0, l = 5)
      )
  }, bg = "transparent", res = 96)

  # Label showing what's targeted
  output$risk_targeting_label <- renderUI({
    req(input$min_risk_percentile)
    cutoff <- input$min_risk_percentile
    if (cutoff >= 100) {
      tags$div(style = "color: red; font-weight: bold; margin-top: 3px;",
               "No risk strata selected. Lower the cutoff to target a population.")
    } else if (cutoff == 0) {
      tags$div(style = "color: #666; margin-top: 3px;",
               "Targeting all risk strata (entire population at risk).")
    } else {
      pct_targeted <- 100 - cutoff
      tags$div(style = "color: #666; margin-top: 3px;",
               paste0("Targeting the top ", pct_targeted,
                      "% highest-risk individuals (", cutoff,
                      "th\u2013100th percentile)."))
    }
  })

  # Reactive to get min_risk_quantile (handles NULL when slider not rendered)
  get_min_risk_quantile <- reactive({
    rg <- as.integer(input$risk_groups)
    if (rg == 1) {
      0
    } else {
      val <- input$min_risk_percentile
      if (is.null(val)) 0 else val / 100
    }
  })

  # --- Run Model ---
  observeEvent(input$run_model, {
    # Validate inputs
    if (length(input$age_groups) == 0) {
      showNotification("Please select at least one age group.", type = "error")
      return()
    }
    if (length(input$sex) == 0) {
      showNotification("Please select at least one sex group.", type = "error")
      return()
    }
    if (!is.null(input$min_risk_percentile) && input$min_risk_percentile >= 100) {
      showNotification("Risk cutoff is at 100% -- no population is targeted. Lower the cutoff.", type = "error")
      return()
    }

    # Use cached data (already loaded reactively)
    data <- cached_data()

    # Check for errors
    if (!is.null(data$error)) {
      showNotification(data$error, type = "error", duration = 10)
      return()
    }

    # Compute budget
    budget <- input$total_courses * input$cost_per_course

    # Run allocation model
    withProgress(message = "Running allocation model...", value = 0.4, {
      tryCatch({
        outputs <- generate_prep_allocation_outputs(
          facility_df = data$facility_df,
          incidence_df = data$incidence_df,
          facility_coords_df = data$facility_coords_df,
          district_sf = data$district_sf,
          district_new_infections = data$district_new_infections,
          budget_vec = budget,
          cost_per_unit_vec = input$cost_per_course,
          selected_budget = budget,
          selected_cost = input$cost_per_course,
          efficacy = input$efficacy,
          coverage_mult = input$coverage_cap / 100,
          age_group_allocation_selection = input$age_groups,
          sex_allocation_selection = input$sex,
          risk_groups = as.integer(input$risk_groups),
          min_risk_quantile = get_min_risk_quantile()
        )

        # Store results
        model_results(outputs)

        setProgress(value = 0.8, message = "Computing dose-response curve...")

        # Compute dose-response curve
        dose_result <- compute_dose_response_curve(
          facility_df = data$facility_df,
          incidence_df = data$incidence_df,
          efficacy = input$efficacy,
          coverage_mult = input$coverage_cap / 100,
          age_group_allocation_selection = input$age_groups,
          sex_allocation_selection = input$sex,
          min_risk_quantile = get_min_risk_quantile()
        )
        dose_curve_data(dose_result)

        setProgress(value = 1, message = "Done!")
        showNotification("Model run complete.", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(
          paste("Model error:", e$message),
          type = "error",
          duration = 15
        )
      })
    })
  })

  # --- Render Maps ---
  output$allocation_maps <- renderPlot({
    req(model_results())
    model_results()$formatted_map
  }, res = 96)

  # --- Render Summary Table ---
  output$summary_table <- DT::renderDataTable({
    req(model_results())
    summary_df <- model_results()$summary_table

    # Transpose for readability (single-row table looks better as key-value)
    display_df <- data.frame(
      Metric = c(
        "Scenario",
        "Budget (USD)",
        "Cost per Unit (USD)",
        "Total Units Allocated",
        "Expected Infections (no PrEP)",
        "Infections Averted",
        "% Reduction in Incidence",
        "PrEP Coverage (%)",
        "Total DALYs Averted",
        "Cost per Infection Averted (USD)",
        "Cost per DALY Averted (USD)",
        "Number Needed to Treat",
        "Districts with Allocation",
        "Avg Incidence (allocated pop)",
        "Avg Incidence (total pop)",
        "Incidence Targeting Ratio",
        "Allocation by Age/Sex"
      ),
      Value = c(
        as.character(summary_df$scenario),
        format(round(summary_df$budget), big.mark = ","),
        format(round(summary_df$cost_per_unit), big.mark = ","),
        format(round(summary_df$total_allocated_units), big.mark = ","),
        format(round(summary_df$expected_infections_no_prep), big.mark = ","),
        format(round(summary_df$infections_averted), big.mark = ","),
        paste0(round(summary_df$percent_reduction_in_incidence, 1), "%"),
        paste0(round(summary_df$prep_coverage, 1), "%"),
        format(round(summary_df$total_dalys_averted), big.mark = ","),
        paste0("$", format(round(summary_df$cost_per_infection_averted), big.mark = ",")),
        paste0("$", format(round(summary_df$cost_per_daly_averted), big.mark = ",")),
        round(summary_df$number_needed_to_treat, 1),
        summary_df$facilities_with_allocation,
        round(summary_df$avg_incidence_allocated, 2),
        round(summary_df$avg_incidence_population, 2),
        round(summary_df$incidence_targeting_ratio, 2),
        as.character(summary_df$allocation_by_age_sex)
      ),
      stringsAsFactors = FALSE
    )

    DT::datatable(
      display_df,
      options = list(dom = "t", pageLength = 20, ordering = FALSE),
      rownames = FALSE
    )
  })

  # --- Render District Detail ---
  output$district_detail <- DT::renderDataTable({
    req(model_results())
    detail_df <- model_results()$by_prov_dist_age_sex

    # Filter out zero-only rows
    detail_df <- detail_df %>%
      filter(rowSums(across(where(is.numeric)), na.rm = TRUE) != 0) %>%
      mutate(
        Total = rowSums(across(where(is.numeric)), na.rm = TRUE),
        Percent = round(Total / sum(Total) * 100, 1)
      )

    DT::datatable(
      detail_df,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("csv", "excel")
      ),
      extensions = "Buttons",
      rownames = FALSE
    )
  })

  # --- Save Scenario ---
  observeEvent(input$save_scenario, {
    results <- model_results()
    if (is.null(results)) {
      showNotification("No model results to save. Run the model first.", type = "warning")
      return()
    }

    # Get the country display name (names are display labels, values are ISO3 codes)
    country_name <- names(SUPPORTED_COUNTRIES)[SUPPORTED_COUNTRIES == input$country]
    if (length(country_name) == 0) country_name <- input$country

    risk_group_n <- as.integer(input$risk_groups)
    risk_targeted <- if (risk_group_n == 1) {
      "district avg"
    } else {
      cutoff <- get_min_risk_quantile() * 100
      if (cutoff == 0) "all strata" else paste0("top ", 100 - cutoff, "%")
    }
    age_str <- paste(input$age_groups, collapse = "/")
    male_ages <- if ("male" %in% input$sex) age_str else "--"
    female_ages <- if ("female" %in% input$sex) age_str else "--"

    scenario_row <- results$summary_table %>%
      mutate(
        country = country_name,
        risk_groups_n = risk_group_n,
        risk_targeted = risk_targeted,
        male_ages = male_ages,
        female_ages = female_ages,
        uptake_pct = input$coverage_cap,
        efficacy_pct = input$efficacy * 100
      )

    scenario_log$scenarios <- c(scenario_log$scenarios, list(scenario_row))
    showNotification(
      paste("Scenario saved. Total saved:", length(scenario_log$scenarios)),
      type = "message", duration = 3
    )
  })

  # --- Clear Scenarios ---
  observeEvent(input$clear_scenarios, {
    scenario_log$scenarios <- list()
    showNotification("Saved scenarios cleared.", type = "message", duration = 3)
  })

  # --- Scenario Count ---
  output$scenario_count <- renderText({
    n <- length(scenario_log$scenarios)
    if (n == 0) {
      "No scenarios saved"
    } else {
      paste(n, "scenario(s) saved")
    }
  })

  # --- Dose Finder: update slider max based on what's achievable ---
  observe({
    req(dose_curve_data())
    max_pct <- floor(dose_curve_data()$max_reduction)
    updateSliderInput(session, "target_reduction", max = max(max_pct, 1))
  })

  # --- Dose Finder ---

  # Find the required courses for the target reduction
  dose_finder_info <- reactive({
    req(dose_curve_data())
    curve <- dose_curve_data()$curve
    target <- input$target_reduction

    # Find the first row where cumulative reduction meets or exceeds target
    match_row <- curve %>% filter(pct_reduction >= target) %>% slice(1)

    if (nrow(match_row) == 0) {
      # Target exceeds max achievable reduction
      list(
        achievable = FALSE,
        max_reduction = round(dose_curve_data()$max_reduction, 1),
        max_courses = dose_curve_data()$max_courses
      )
    } else {
      list(
        achievable = TRUE,
        courses_needed = match_row$cum_courses,
        cost = match_row$cum_courses * input$cost_per_course,
        infections_averted = round(match_row$cum_infections_averted),
        pct_achieved = round(match_row$pct_reduction, 1)
      )
    }
  })

  output$dose_finder_result <- renderUI({
    info <- dose_finder_info()
    max_reduction <- round(dose_curve_data()$max_reduction, 1)
    max_courses <- dose_curve_data()$max_courses
    total_pop <- dose_curve_data()$total_population
    max_note <- tags$div(
      style = "margin-top: 10px; padding-top: 8px; border-top: 1px solid #ddd; color: #2e7d32; font-size: 13px;",
      tags$p(style = "margin: 0;",
             tags$strong("Max achievable: "),
             paste0(max_reduction, "% reduction (",
                    input$coverage_cap, "% uptake x ",
                    input$efficacy * 100, "% eff)")),
      tags$p(style = "margin: 0;",
             paste0("at ", format(max_courses, big.mark = ","), " courses"))
    )
    if (!info$achievable) {
      tags$div(
        tags$div(
          style = "color: red;",
          tags$p(tags$strong(paste0("Target ", input$target_reduction, "% is not achievable.")))
        ),
        max_note
      )
    } else {
      prep_cov <- info$courses_needed / total_pop * 100
      tags$div(
        tags$p(tags$strong(paste0("To reduce incidence by ", input$target_reduction, "%:"))),
        tags$p(paste0("Len courses needed: ", format(info$courses_needed, big.mark = ","),
                       " (", sprintf("%.1f", prep_cov), "% PrEP coverage)")),
        tags$p(paste0("Cost: $", format(round(info$cost), big.mark = ","),
                       " (@ $", input$cost_per_course, "/course)")),
        tags$p(paste0("Infections averted: ", format(info$infections_averted, big.mark = ","))),
        max_note
      )
    }
  })

  output$dose_response_plot <- renderPlot({
    req(dose_curve_data())
    curve <- dose_curve_data()$curve
    info <- dose_finder_info()
    target <- input$target_reduction

    max_reduction <- dose_curve_data()$max_reduction
    total_infections <- dose_curve_data()$total_expected_infections
    total_pop <- dose_curve_data()$total_population
    max_courses <- max(curve$cum_courses)
    max_x <- max_courses

    # Scale factor for secondary y-axis (infections averted)
    max_inf_averted <- max(curve$cum_infections_averted)
    y_scale <- max_reduction / max_inf_averted

    p <- ggplot(curve, aes(x = cum_courses)) +
      # Primary line: % incidence reduction
      geom_line(aes(y = pct_reduction), color = "#2c7bb6", linewidth = 1.2) +
      # Secondary line: infections averted (rescaled to primary y-axis)
      geom_line(aes(y = cum_infections_averted * y_scale), color = "#e17055", linewidth = 1, linetype = "solid", alpha = 0.7) +
      # Target line
      geom_hline(yintercept = target, linetype = "dashed", color = "red", linewidth = 0.8) +
      annotate("text", x = max_x * 0.02, y = target + max_reduction * 0.03,
               label = paste0("Target: ", target, "%"), color = "red", hjust = 0, size = 4.5) +
      # Axes
      scale_x_continuous(
        name = "Cumulative Len Courses",
        labels = scales::comma,
        sec.axis = sec_axis(~ . / total_pop * 100, name = "PrEP Coverage (%)",
                            labels = function(x) paste0(round(x, 1), "%"))
      ) +
      scale_y_continuous(
        name = "Incidence Reduction (%)",
        sec.axis = sec_axis(~ . / y_scale, name = "Infections Averted",
                            labels = scales::comma)
      ) +
      labs(
        title = "Volume Finder: Len Courses vs Incidence Reduction",
        caption = paste0("Total expected infections: ",
                         format(round(total_infections), big.mark = ","),
                         "  |  Blue = % reduction, Orange = infections averted")
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 10, hjust = 0.5, color = "grey50"),
        axis.title.y.left = element_text(color = "#2c7bb6"),
        axis.text.y.left = element_text(color = "#2c7bb6"),
        axis.title.y.right = element_text(color = "#e17055"),
        axis.text.y.right = element_text(color = "#e17055"),
        axis.title.x.top = element_text(size = 11)
      )

    # Add vertical line at the required courses if achievable
    if (info$achievable) {
      p <- p +
        geom_vline(xintercept = info$courses_needed, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
        annotate("text",
                 x = info$courses_needed,
                 y = max_reduction * 1.03,
                 label = paste0(format(info$courses_needed, big.mark = ","), " courses"),
                 color = "darkgreen", hjust = 0.5, vjust = 0, size = 4)
    }

    p
  }, res = 96)

  output$dose_response_table <- DT::renderDataTable({
    req(dose_curve_data())
    curve <- dose_curve_data()$curve
    cost_per <- input$cost_per_course

    # Show each unique step in the curve (deduplicated)
    # Each row = a distinct strata allocation that adds courses and averts infections
    table_df <- curve %>%
      mutate(
        pct_reduction_rounded = floor(pct_reduction)
      ) %>%
      # Keep only the first row for each integer % milestone
      group_by(pct_reduction_rounded) %>%
      slice(1) %>%
      ungroup() %>%
      transmute(
        `Incidence Reduction (%)` = round(pct_reduction, 1),
        `Len Courses` = cum_courses,
        `Cost (USD)` = cum_courses * cost_per,
        `Infections Averted` = round(cum_infections_averted)
      ) %>%
      mutate(
        `Len Courses` = format(`Len Courses`, big.mark = ","),
        `Cost (USD)` = paste0("$", format(round(`Cost (USD)`), big.mark = ",")),
        `Infections Averted` = format(`Infections Averted`, big.mark = ",")
      )

    DT::datatable(
      table_df,
      options = list(pageLength = 25, dom = "Bfrtip", buttons = c("csv", "excel")),
      extensions = "Buttons",
      rownames = FALSE
    )
  })

  # --- Render Scenario Comparison ---
  output$scenario_comparison <- DT::renderDataTable({
    req(length(scenario_log$scenarios) > 0)

    comparison_df <- bind_rows(scenario_log$scenarios) %>%
      dplyr::select(
        Country = country,
        `Risk Groups` = risk_groups_n,
        `Risk Targeted` = risk_targeted,
        `Male Ages` = male_ages,
        `Female Ages` = female_ages,
        `Uptake %` = uptake_pct,
        `Efficacy %` = efficacy_pct,
        `Units Allocated` = total_allocated_units,
        `Expected Infections` = expected_infections_no_prep,
        `Infections Averted` = infections_averted,
        `% Reduction` = percent_reduction_in_incidence,
        `PrEP Coverage (%)` = prep_coverage,
        `DALYs Averted` = total_dalys_averted,
        `$/Infection Averted` = cost_per_infection_averted,
        `$/DALY Averted` = cost_per_daly_averted,
        NNT = number_needed_to_treat,
        `Districts Allocated` = facilities_with_allocation,
        `Targeting Ratio` = incidence_targeting_ratio
      ) %>%
      mutate(
        `Uptake %` = paste0(`Uptake %`, "%"),
        `Efficacy %` = paste0(`Efficacy %`, "%"),
        across(c(`Units Allocated`, `Expected Infections`, `Infections Averted`, `DALYs Averted`),
               ~ format(round(.), big.mark = ",")),
        `% Reduction` = paste0(round(`% Reduction`, 1), "%"),
        `PrEP Coverage (%)` = paste0(round(`PrEP Coverage (%)`, 1), "%"),
        `$/Infection Averted` = paste0("$", format(round(`$/Infection Averted`), big.mark = ",")),
        `$/DALY Averted` = paste0("$", format(round(`$/DALY Averted`), big.mark = ",")),
        NNT = round(NNT, 1),
        `Targeting Ratio` = round(`Targeting Ratio`, 2)
      )

    header_container <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th(colspan = 7, "Model Inputs",
             style = "text-align: center; background-color: #e8f0fe; border-bottom: 2px solid #333;"),
          th(colspan = 11, "Model Outputs",
             style = "text-align: center; background-color: #fef3e8; border-bottom: 2px solid #333;")
        ),
        tr(
          lapply(names(comparison_df), th)
        )
      )
    ))

    DT::datatable(
      comparison_df,
      container = header_container,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("csv", "excel")
      ),
      extensions = "Buttons",
      rownames = FALSE
    )
  })
}
