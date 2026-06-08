# =============================================================================
# MetLife Investment Management — Real Estate Dashboard
# Shiny App | dashboard.R
# =============================================================================
# Install once if needed:
# install.packages(c(
#   "shiny","shinydashboard","readxl","dplyr","DT","plotly","ggplot2",
#   "shinycssloaders","scales","shinyWidgets","stringr","tidyr","purrr",
#   "httr2","jsonlite","xml2","htmltools"
# ))
# =============================================================================

library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(DT)
library(plotly)
library(ggplot2)
library(shinycssloaders)
library(scales)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(purrr)
library(httr2)
library(jsonlite)
library(xml2)
library(htmltools)

# ---- File paths ----  # Set this to the path of the data file you want to use.
# DATA_PATH    <- "data/country_data.xlsx"
script_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }
  if (interactive()) {
    return(getwd())
  }
  getwd()
})
DATA_PATH    <- file.path(script_dir,"final_data_latest.xlsx")
SUMMARY_PATH <- file.path(script_dir, "MSCI_SummaryStats.xlsx")
assets_path <- file.path(script_dir, "custom_assets")
if (dir.exists(assets_path)) {
  addResourcePath("custom_assets", normalizePath(assets_path))
}

# ---- API KEY ----
# Not recommended, but you can hardcode the gemini_api_key here.
GEMINI_API_KEY_INLINE <- "AIzaSyDd5i0Q84Nk_1FA_kl4_Y00xdATsb3HUqU"

gemini_api_key <- function() {
  k <- Sys.getenv("GEMINI_API_KEY", unset = "")
  if (nzchar(k)) return(k)
  if (nzchar(GEMINI_API_KEY_INLINE)) return(GEMINI_API_KEY_INLINE)
  ""
}

# =============================================================================
# DATA LOADING & CLEANING
# =============================================================================

standardize_country_name <- function(x) {
  x <- trimws(as.character(x))
  x <- dplyr::case_when(
    x %in% c("US", "U.S.", "USA", "United States of America") ~ "United States",
    x %in% c("UK", "U.K.") ~ "United Kingdom",
    TRUE ~ x
  )
  x
}

load_msci_summary <- function(path) {
  raw <- read_excel(path, sheet = "Sheet1", col_names = FALSE)
  raw <- raw[, 1:7]
  colnames(raw) <- c("Country", as.character(unlist(raw[1, 2:7])))
  summary_df <- raw[-1, ] %>%
    rename(
      `10yr_Average_TROR`     = `10yr Average TROR`,
      `Prior_Yr_TROR`         = `Prior Yr TROR`,
      `StDev_TROR`            = `StDev TROR`,
      `Worst_Drawdown_TROR`   = `Worst Drawdown TROR`,
      `10yr_Average_Cap_Rate` = `10yr Average Cap Rate`,
      `Current_Cap_Rate`      = `Current Cap Rate`
    ) %>%
    mutate(
      Country = standardize_country_name(Country),
      across(-Country, ~ suppressWarnings(as.numeric(.)))
    ) %>%
    filter(!is.na(Country), Country != "")
  # Some workbook versions contain duplicate country rows (often with mixed scales).
  # Keep one row per country using a plausibility + completeness heuristic.
  summary_df <- summary_df %>%
    mutate(
      .non_missing = rowSums(!is.na(pick(-Country))),
      .plausibility =
        ifelse(!is.na(`10yr_Average_TROR`) & abs(`10yr_Average_TROR`) <= 0.5, 2, 0) +
        ifelse(!is.na(`Prior_Yr_TROR`) & abs(`Prior_Yr_TROR`) <= 0.5, 2, 0) +
        ifelse(!is.na(`StDev_TROR`) & `StDev_TROR` <= 0.5, 3, 0) +
        ifelse(!is.na(`Worst_Drawdown_TROR`) & `Worst_Drawdown_TROR` <= 0.2, 2, 0) +
        ifelse(!is.na(`10yr_Average_Cap_Rate`) & `10yr_Average_Cap_Rate` >= 0 & `10yr_Average_Cap_Rate` <= 0.2, 1, 0) +
        ifelse(!is.na(`Current_Cap_Rate`) & `Current_Cap_Rate` >= 0 & `Current_Cap_Rate` <= 0.2, 2, 0)
    ) %>%
    arrange(Country, desc(.plausibility), desc(.non_missing)) %>%
    group_by(Country) %>%
    slice(1) %>%
    ungroup() %>%
    select(-.non_missing, -.plausibility)
  summary_df
}

ensure_us_present <- function(df, msci_summary) {
  if ("United States" %in% df$Country) return(df)
  us_msci <- msci_summary %>% filter(Country == "United States")
  if (nrow(us_msci) == 0) {
    us_msci <- tibble(
      Country = "United States",
      `10yr_Average_TROR` = NA_real_, `Prior_Yr_TROR` = NA_real_,
      `StDev_TROR` = NA_real_, `Worst_Drawdown_TROR` = NA_real_,
      `10yr_Average_Cap_Rate` = NA_real_, `Current_Cap_Rate` = NA_real_
    )
  }
  template <- df[1, , drop = FALSE]
  template[1, ] <- NA
  template$Country <- "United States"
  template$Region <- "North America"
  template$Region_Specific <- "North America"
  missing_cols <- setdiff(names(us_msci), names(template))
  for (nm in missing_cols) template[[nm]] <- NA
  template[names(us_msci)] <- us_msci[1, names(us_msci)]
  bind_rows(df, template)
}

load_data <- function() {
  base <- read_excel(DATA_PATH, sheet = "Sheet1")
  msci_summary <- load_msci_summary(SUMMARY_PATH)
  base <- base %>%
    filter(!is.na(Country)) %>%
    filter(Country != "Category Bucket") %>%
    mutate(Country = standardize_country_name(Country))
  base <- base %>%
    rename(
      Country            = Country,
      Region             = region,
      Region_Specific    = Region_Specific,
      HDI                = hdi,
      Gini               = gini,
      Gender_Inequality  = gender_ineq,
      Life_Expectancy    = life_exp,
      Reg_Quality        = reg_quality,
      # cpi_score in workbook = Transparency International Corruption Perceptions Index (not consumer prices)
      Corr_Percep_Score          = cpi_score,
      Unemployment       = Unemployment,
      Inflation          = Inflation,
      Property_Rights    = `Property Rights Score`,
      Public_Debt        = `Public Debt`,
      Business_Freedom   = `Business Freedom Score`,
      Tax_Burden         = `Tax Burden Score`,
      Trade_Freedom      = `Trade Freedom Score`,
      Investment_Freedom = `Investment Freedom Score`,
      Financial_Freedom  = `Financial Freedom Score`,
      Education_Rank     = EducationRankingsWorldTop20_2026,
      Mkt_Transparency   = `Market Transparency Overall Score`,
      Mkt_Trans_Invest   = `Market Transparency Investment Performance`,
      Mkt_Trans_Fund     = `Market Transparency Market Fundamentals`,
      Mkt_Trans_Legal    = `Market Transparency Regulatory & Legal`,
      Mkt_Trans_Sustain  = `Market Transparency Sustainability`,
      FDI_Inflow         = `FDI Inflow Numeric`,
      GDP_PPP            = `GDP_(PPP)_Numeric`,
      Population_Num     = Population_Numeric,
      Public_Debt_Ratio  = `Public Debt Ratio`,
      Country_Risk_Premium = `Country Risk Premium`,
      Country_Risk_Z       = `Country Risk Premium Z-Score`
    ) %>%
    mutate(across(c(
      HDI, Gini, Gender_Inequality, Life_Expectancy, Reg_Quality,
      Corr_Percep_Score, Unemployment, Inflation, Property_Rights, Public_Debt,
      Business_Freedom, Tax_Burden, Trade_Freedom, Investment_Freedom,
      Financial_Freedom, Education_Rank, Mkt_Transparency,
      Mkt_Trans_Invest, Mkt_Trans_Fund, Mkt_Trans_Legal, Mkt_Trans_Sustain,
      FDI_Inflow, GDP_PPP, Population_Num, Public_Debt_Ratio,
      Country_Risk_Premium, Country_Risk_Z,
      `10yr Average TROR`, `Prior Yr TROR`, `StDev TROR`,
      `Worst Drawdown TROR`, `10yr Average Cap Rate`, `Current Cap Rate`
    ), ~ suppressWarnings(as.numeric(.))))
  base <- base %>%
    select(-any_of(c(
      "10yr Average TROR", "Prior Yr TROR", "StDev TROR",
      "Worst Drawdown TROR", "10yr Average Cap Rate", "Current Cap Rate",
      "10yr_Average_TROR", "Prior_Yr_TROR", "StDev_TROR",
      "Worst_Drawdown_TROR", "10yr_Average_Cap_Rate", "Current_Cap_Rate"
    ))) %>%
    left_join(msci_summary, by = "Country")
  base <- ensure_us_present(base, msci_summary)
  base <- base %>%
    mutate(
      Region = case_when(
        Country == "United States" & (is.na(Region) | Region == "") ~ "North America",
        TRUE ~ Region
      ),
      Region_Specific = case_when(
        Country == "United States" & (is.na(Region_Specific) | Region_Specific == "") ~ "North America",
        TRUE ~ Region_Specific
      ),
      FDI_B = FDI_Inflow / 1e9,
      GDP_B = GDP_PPP / 1e9,
      Pop_M = Population_Num / 1e6
    )
  base
}

df_all_raw <- load_data()

tier_colors <- c(
  "Tier 1 — Core"          = "#1a5276",
  "Tier 2 — Value-Add"     = "#1f618d",
  "Tier 3 — Opportunistic" = "#5dade2",
  "Watch"                  = "#aab7b8"
)

# =============================================================================
# TWO-TIER BUCKET DEFINITIONS
# =============================================================================

BUCKETS <- list(
  list(
    id = "econ", name = "Economic Indicators",
    subfactors = list(
      list(id = "sf_hdi",   name = "HDI",          col = "HDI",          higher_better = TRUE,  scale = c(0, 1)),
      list(id = "sf_gdp",   name = "GDP PPP ($B)", col = "GDP_B",        higher_better = TRUE,  scale = NULL),
      list(id = "sf_fdi",   name = "FDI ($B)",     col = "FDI_B",        higher_better = TRUE,  scale = NULL),
      list(id = "sf_unemp", name = "Unemployment", col = "Unemployment", higher_better = FALSE, scale = NULL),
      list(id = "sf_infl",  name = "Inflation",    col = "Inflation",    higher_better = FALSE, scale = NULL)
    )
  ),
  list(
    id = "freedom", name = "Business Friendliness",
    subfactors = list(
      list(id = "sf_biz",    name = "Business",   col = "Business_Freedom",   higher_better = TRUE, scale = c(0, 100)),
      list(id = "sf_trade",  name = "Trade",      col = "Trade_Freedom",      higher_better = TRUE, scale = c(0, 100)),
      list(id = "sf_invest", name = "Investment", col = "Investment_Freedom", higher_better = TRUE, scale = c(0, 100)),
      list(id = "sf_fin",    name = "Financial",  col = "Financial_Freedom",  higher_better = TRUE, scale = c(0, 100)),
      list(id = "sf_tax",    name = "Tax Burden", col = "Tax_Burden",         higher_better = TRUE, scale = c(0, 100))
    )
  ),
  list(
    id = "judicial", name = "Judicial System",
    subfactors = list(
      list(id = "sf_prop", name = "Property Rights", col = "Property_Rights", higher_better = TRUE, scale = c(0, 100)),
      list(id = "sf_corr_percep",  name = "Corruption Perceptions",       col = "Corr_Percep_Score",       higher_better = TRUE, scale = c(0, 100)),
      list(id = "sf_reg",  name = "Reg. Quality",    col = "Reg_Quality",     higher_better = TRUE, scale = NULL)
    )
  ),
  list(
    id = "re_perf", name = "RE Performance",
    subfactors = list(
      list(id = "sf_tror10",   name = "10Y Avg TROR",    col = "10yr_Average_TROR",   higher_better = TRUE,  scale = NULL),
      list(id = "sf_tror_py",  name = "Prior Yr TROR",   col = "Prior_Yr_TROR",       higher_better = TRUE,  scale = NULL),
      list(id = "sf_tror_vol", name = "TROR Volatility", col = "StDev_TROR",          higher_better = FALSE, scale = NULL),
      list(id = "sf_drawdown", name = "Worst Drawdown",  col = "Worst_Drawdown_TROR", higher_better = TRUE,  scale = NULL),
      list(id = "sf_caprate",  name = "Cap Rate",        col = "Current_Cap_Rate",    higher_better = TRUE,  scale = NULL)
    )
  ),
  list(
    id = "transparency", name = "Market Transparency",
    subfactors = list(
      list(id = "sf_tr_overall", name = "Overall",        col = "Mkt_Transparency",  higher_better = FALSE, scale = c(1, 5)),
      list(id = "sf_tr_invest",  name = "Investment",     col = "Mkt_Trans_Invest",  higher_better = FALSE, scale = c(1, 5)),
      list(id = "sf_tr_fund",    name = "Fundamentals",   col = "Mkt_Trans_Fund",    higher_better = FALSE, scale = c(1, 5)),
      list(id = "sf_tr_legal",   name = "Legal",          col = "Mkt_Trans_Legal",   higher_better = FALSE, scale = c(1, 5)),
      list(id = "sf_tr_sustain", name = "Sustainability", col = "Mkt_Trans_Sustain", higher_better = FALSE, scale = c(1, 5))
    )
  ),
  list(
    id = "risk", name = "Risk Factor",
    subfactors = list(
      list(
        id = "sf_country_risk",
        name = "Country Risk Premium Z",
        col = "Country_Risk_Z",
        higher_better = TRUE,
        scale = NULL
      )
    )
  )
)

# =============================================================================
# SCORING ENGINE
# =============================================================================

normalize_to_100 <- function(x, lo, hi, higher_better = TRUE) {
  if (is.na(x) || is.na(lo) || is.na(hi) || hi == lo) return(NA_real_)
  norm <- pmin(pmax((x - lo) / (hi - lo) * 100, 0), 100)
  if (!higher_better) norm <- 100 - norm
  norm
}

# Helper: build a normalised weights list from raw slider values.
# Used in BOTH the base server and the Krish layer so there is no cross-scope dependency.
build_model_weights <- function(input) {
  bkt_raw <- setNames(
    vapply(BUCKETS, function(b) {
      v <- input[[paste0("w_bucket_", b$id)]]; if (is.null(v) || is.na(v)) 20 else v
    }, numeric(1)),
    vapply(BUCKETS, function(b) b$id, character(1))
  )
  bkt_total <- sum(bkt_raw)
  bkt_norm  <- if (bkt_total > 0) bkt_raw / bkt_total
  else setNames(rep(1 / length(bkt_raw), length(bkt_raw)), names(bkt_raw))
  
  sf_weights <- setNames(
    lapply(BUCKETS, function(bkt) {
      sf_raw <- setNames(
        vapply(bkt$subfactors, function(sf) {
          v <- input[[paste0("w_", sf$id)]]; if (is.null(v) || is.na(v)) 20 else v
        }, numeric(1)),
        vapply(bkt$subfactors, function(sf) sf$id, character(1))
      )
      sf_total <- sum(sf_raw)
      if (sf_total > 0) sf_raw / sf_total
      else setNames(rep(1 / length(sf_raw), length(sf_raw)), names(sf_raw))
    }),
    vapply(BUCKETS, function(b) b$id, character(1))
  )
  
  list(bucket_weights = bkt_norm, sf_weights = sf_weights)
}

compute_re_score <- function(df, weights) {
  global_ranges <- list()
  for (bkt in BUCKETS) {
    for (sf in bkt$subfactors) {
      if (is.null(sf$scale)) {
        vals <- suppressWarnings(as.numeric(df[[sf$col]]))
        lo <- min(vals, na.rm = TRUE)
        hi <- max(vals, na.rm = TRUE)
        if (!is.finite(lo) || !is.finite(hi) || lo == hi) {
          lo <- 0
          hi <- 1
        }
        global_ranges[[sf$id]] <- c(lo, hi)
      }
    }
  }
  
  score_vec <- vapply(seq_len(nrow(df)), function(i) {
    total_score <- 0; total_weight <- 0
    for (bkt in BUCKETS) {
      bkt_w <- weights$bucket_weights[bkt$id]
      if (is.na(bkt_w) || bkt_w <= 0) next
      bkt_score <- 0; bkt_weight_used <- 0
      for (sf in bkt$subfactors) {
        sf_w <- weights$sf_weights[[bkt$id]][sf$id]
        if (is.na(sf_w) || sf_w <= 0) next
        raw_val <- suppressWarnings(as.numeric(df[[sf$col]][i]))
        if (is.na(raw_val)) next
        norm_val <- if (!is.null(sf$scale)) {
          normalize_to_100(raw_val, sf$scale[1], sf$scale[2], sf$higher_better)
        } else {
          rng <- global_ranges[[sf$id]]
          normalize_to_100(raw_val, rng[1], rng[2], sf$higher_better)
        }
        if (!is.na(norm_val)) {
          bkt_score       <- bkt_score       + sf_w * norm_val
          bkt_weight_used <- bkt_weight_used + sf_w
        }
      }
      if (bkt_weight_used > 0) {
        total_score  <- total_score  + bkt_w * (bkt_score / bkt_weight_used)
        total_weight <- total_weight + bkt_w
      }
    }
    if (total_weight > 0) total_score / total_weight else NA_real_
  }, numeric(1))
  
  df$RE_Score <- score_vec
  df$Tier <- dplyr::case_when(
    df$RE_Score >= 70 ~ "Tier 1 — Core",
    df$RE_Score >= 50 ~ "Tier 2 — Value-Add",
    df$RE_Score >= 35 ~ "Tier 3 — Opportunistic",
    TRUE              ~ "Watch"
  )
  df
}

# =============================================================================
# Interactive Map Layers
# =============================================================================
build_map_summary <- function(df) {
  classify_vs_global <- function(value, global_vals) {
    global_vals <- suppressWarnings(as.numeric(global_vals))
    global_vals <- global_vals[!is.na(global_vals)]
    
    if (is.na(value) || length(global_vals) < 3) {
      return(list(
        value = value,
        avg = mean(global_vals, na.rm = TRUE),
        comparison = NA_character_
      ))
    }
    
    qs <- quantile(global_vals, probs = c(0.33, 0.67), na.rm = TRUE, names = FALSE)
    
    comparison <- dplyr::case_when(
      is.na(value) ~ NA_character_,
      value <= qs[1] ~ "Low",
      value >= qs[2] ~ "High",
      TRUE ~ "Average"
    )
    
    list(
      value = value,
      avg = mean(global_vals, na.rm = TRUE),
      comparison = comparison
    )
  }
  
  results <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    
    econ_val <- suppressWarnings(as.numeric(row$GDP_B))
    econ_res <- classify_vs_global(econ_val, df$GDP_B)
    
    biz_val <- suppressWarnings(as.numeric(row$Investment_Freedom))
    biz_res <- classify_vs_global(biz_val, df$Investment_Freedom)
    
    jud_val <- suppressWarnings(as.numeric(row$Property_Rights))
    jud_res <- classify_vs_global(jud_val, df$Property_Rights)
    
    re_val <- suppressWarnings(as.numeric(row$`10yr_Average_TROR`))
    re_res <- classify_vs_global(re_val, df$`10yr_Average_TROR`)
    
    trans_val <- suppressWarnings(as.numeric(row$Mkt_Transparency))
    trans_res <- classify_vs_global(trans_val, df$Mkt_Transparency)
    
    risk_val <- suppressWarnings(as.numeric(row$Country_Risk_Z))
    risk_res <- classify_vs_global(risk_val, df$Country_Risk_Z)
    
    results[[i]] <- data.frame(
      Country = row$Country,
      Region = row$Region,
      
      econ_feature = "GDP PPP ($B)",
      econ_value = econ_res$value,
      econ_avg = econ_res$avg,
      econ_comparison = econ_res$comparison,
      
      biz_feature = "Investment Freedom",
      biz_value = biz_res$value,
      biz_avg = biz_res$avg,
      biz_comparison = biz_res$comparison,
      
      jud_feature = "Property Rights",
      jud_value = jud_res$value,
      jud_avg = jud_res$avg,
      jud_comparison = jud_res$comparison,
      
      re_feature = "10Y Avg TROR",
      re_value = re_res$value,
      re_avg = re_res$avg,
      re_comparison = re_res$comparison,
      
      trans_feature = "Overall Market Transparency",
      trans_value = trans_res$value,
      trans_avg = trans_res$avg,
      trans_comparison = trans_res$comparison,
      
      risk_feature = "Country Risk Premium Z",
      risk_value = risk_res$value,
      risk_avg = risk_res$avg,
      risk_comparison = risk_res$comparison,
      
      stringsAsFactors = FALSE
    )
  }
  
  dplyr::bind_rows(results)
}

# =============================================================================
# GEMINI API HELPERS
# =============================================================================

GEMINI_MODEL_CANDIDATES <- c(
  "gemini-2.5-flash", "gemini-2.5-flash-lite", "gemini-2.5-pro", "gemini-2.0-flash"
)

gemini_strip_cli <- function(x) {
  if (!length(x) || !is.character(x)) return(x)
  gsub("\033\\[[0-9;]*m", "", x, perl = TRUE)
}

gemini_is_quota_error <- function(err_msg) {
  if (is.null(err_msg) || !is.character(err_msg) || !length(err_msg)) return(FALSE)
  msg <- tolower(paste(err_msg, collapse = " "))
  grepl("http\\s*429|quota exceeded|exceeded your current quota|limit:\\s*0|rate-limits", msg)
}

build_news_fallback_summary <- function(scope, value, news_df, quota_blocked = FALSE) {
  n <- min(nrow(news_df), 5L)
  top <- if (n > 0) {
    paste0(
      "<li><strong>", htmltools::htmlEscape(news_df$title[seq_len(n)]), "</strong>",
      " <span style='color:#8b9eb0;'>(",
      htmltools::htmlEscape(news_df$source[seq_len(n)]), ", ",
      htmltools::htmlEscape(news_df$pub_date[seq_len(n)]), ")</span></li>",
      collapse = ""
    )
  } else ""
  quota_note <- if (isTRUE(quota_blocked)) {
    "<p style='margin-top:10px;color:#8b9eb0;font-size:11px;'>Gemini is temporarily unavailable due to API quota limits. Showing headline-only fallback.</p>"
  } else {
    "<p style='margin-top:10px;color:#8b9eb0;font-size:11px;'>Gemini summary unavailable. Showing headline-only fallback.</p>"
  }
  paste0(
    "<div class='ai-insight-text'><span class='ai-badge'>✦ Fallback Summary</span>",
    "<h4>Regulatory / Policy Changes</h4><p>Manual review required from latest headlines for ",
    htmltools::htmlEscape(scope), ": <strong>", htmltools::htmlEscape(value), "</strong>.</p>",
    "<h4>Economic Development Signals</h4><p>Recent coverage indicates active market-related news flow; verify impact before underwriting decisions.</p>",
    "<h4>Real-Estate Investment Relevance</h4><ul>", top, "</ul>",
    "<h4>Risks / Watch Items</h4><p>Without model synthesis, treat this as directional only and validate with primary sources.</p>",
    quota_note,
    "</div>"
  )
}

extract_gemini_text <- function(parsed) {
  candidates <- tryCatch(parsed$candidates, error = function(e) NULL)
  if (is.null(candidates) || !length(candidates)) return("")
  txt <- c()
  for (cand in candidates) {
    parts <- tryCatch(cand$content$parts, error = function(e) NULL)
    if (is.null(parts) || !length(parts)) next
    cand_txt <- vapply(parts, function(p) {
      val <- tryCatch(p$text, error = function(e) "")
      if (is.null(val)) "" else as.character(val)
    }, character(1))
    txt <- c(txt, cand_txt)
  }
  out <- paste(txt[nzchar(trimws(txt))], collapse = "\n")
  if (is.null(out)) "" else out
}

gemini_generate_content <- function(body) {
  api_key <- gemini_api_key()
  if (!nzchar(api_key)) return(list(ok = FALSE, last_error = "GEMINI_API_KEY not found."))
  last_error <- "Unknown Gemini API error."
  for (model_name in GEMINI_MODEL_CANDIDATES) {
    resp <- tryCatch({
      request("https://generativelanguage.googleapis.com") %>%
        req_url_path_append("v1beta", "models", paste0(model_name, ":generateContent")) %>%
        req_url_query(key = api_key) %>%
        req_body_json(body) %>%
        req_error(is_error = function(resp) FALSE) %>%
        req_perform()
    }, error = function(e) e)
    if (inherits(resp, "error")) {
      last_error <- paste0(model_name, ": ", gemini_strip_cli(conditionMessage(resp))); next
    }
    parsed <- tryCatch(resp_body_json(resp), error = function(e) NULL)
    if (resp_status(resp) >= 300) {
      api_msg <- tryCatch(parsed$error$message, error = function(e) NA_character_)
      if (is.na(api_msg) || identical(api_msg, ""))
        api_msg <- tryCatch(resp_body_string(resp), error = function(e) "")
      last_error <- paste0(model_name, " [HTTP ", resp_status(resp), "]: ", api_msg); next
    }
    if (is.null(parsed)) { last_error <- paste0(model_name, ": could not parse API response."); next }
    text_out <- extract_gemini_text(parsed)
    if (!is.null(text_out) && nzchar(trimws(text_out))) {
      finish_reason <- tryCatch(parsed$candidates[[1]]$finishReason, error = function(e) NA_character_)
      return(list(ok = TRUE, text = text_out, last_error = NA_character_, finish_reason = finish_reason))
    }
    last_error <- paste0(model_name, ": empty or blocked response.")
  }
  list(ok = FALSE, text = NA_character_, last_error = last_error, finish_reason = NA_character_)
}

build_country_context <- function(r) {
  glue_line <- function(label, value) paste0(label, ": ", value)
  lines <- c(
    glue_line("Country", r$Country),
    glue_line("Region", ifelse(is.na(r$Region), "N/A", r$Region)),
    glue_line("RE score", ifelse(is.na(r$RE_Score), "N/A", round(r$RE_Score, 1))),
    glue_line("Tier", ifelse(is.na(r$Tier), "N/A", r$Tier)),
    glue_line("HDI", ifelse(is.na(r$HDI), "N/A", round(r$HDI, 3))),
    glue_line("Corruption Perceptions", ifelse(is.na(r$Corr_Percep_Score), "N/A", round(r$Corr_Percep_Score, 1))),
    glue_line("Regulatory quality", ifelse(is.na(r$Reg_Quality), "N/A", round(r$Reg_Quality, 2))),
    glue_line("Inflation", ifelse(is.na(r$Inflation), "N/A", paste0(round(100 * r$Inflation, 1), "%"))),
    glue_line("Unemployment", ifelse(is.na(r$Unemployment), "N/A", paste0(round(100 * r$Unemployment, 1), "%"))),
    glue_line("GDP PPP in billions USD", ifelse(is.na(r$GDP_B), "N/A", round(r$GDP_B, 1))),
    glue_line("FDI inflow in billions USD", ifelse(is.na(r$FDI_B), "N/A", round(r$FDI_B, 1))),
    glue_line("Property rights", ifelse(is.na(r$Property_Rights), "N/A", round(r$Property_Rights, 1))),
    glue_line("Business freedom", ifelse(is.na(r$Business_Freedom), "N/A", round(r$Business_Freedom, 1))),
    glue_line("Trade freedom", ifelse(is.na(r$Trade_Freedom), "N/A", round(r$Trade_Freedom, 1))),
    glue_line("Investment freedom", ifelse(is.na(r$Investment_Freedom), "N/A", round(r$Investment_Freedom, 1))),
    glue_line("Financial freedom", ifelse(is.na(r$Financial_Freedom), "N/A", round(r$Financial_Freedom, 1))),
    glue_line("Country risk premium Z", ifelse(is.na(r$Country_Risk_Z), "N/A", round(r$Country_Risk_Z, 3))),
    glue_line("Overall market transparency", ifelse(is.na(r$Mkt_Transparency), "N/A", round(r$Mkt_Transparency, 2))),
    glue_line("10-year average TROR", ifelse(is.na(r$`10yr_Average_TROR`), "N/A", round(r$`10yr_Average_TROR`, 3))),
    glue_line("Prior year TROR", ifelse(is.na(r$`Prior_Yr_TROR`), "N/A", round(r$`Prior_Yr_TROR`, 3))),
    glue_line("TROR volatility", ifelse(is.na(r$`StDev_TROR`), "N/A", round(r$`StDev_TROR`, 3))),
    glue_line("Worst drawdown TROR", ifelse(is.na(r$`Worst_Drawdown_TROR`), "N/A", round(r$`Worst_Drawdown_TROR`, 3))),
    glue_line("10-year average cap rate", ifelse(is.na(r$`10yr_Average_Cap_Rate`), "N/A", round(r$`10yr_Average_Cap_Rate`, 3))),
    glue_line("Current cap rate", ifelse(is.na(r$`Current_Cap_Rate`), "N/A", round(r$`Current_Cap_Rate`, 3)))
  )
  lines <- lines[!grepl(":\\s*N/A\\s*$", lines)]
  paste(lines, collapse = "\n")
}

get_fallback_ai_insight <- function(country, r) {
  n   <- function(x, d = 1) if (is.na(x)) "N/A" else round(x, d)
  pct <- function(x, d = 1) if (is.na(x)) "N/A" else paste0(round(x * 100, d), "%")
  climate_adj <- if (!is.na(r$RE_Score) && r$RE_Score >= 70) "highly favourable"
  else if (!is.na(r$RE_Score) && r$RE_Score >= 55) "moderately attractive"
  else if (!is.na(r$RE_Score) && r$RE_Score >= 40) "selectively investable"
  else "challenging"
  s1 <- paste0("<p><strong>", country, "</strong> looks <strong>", climate_adj, "</strong> from a cross-market screening standpoint (RE Score: <strong>", n(r$RE_Score), "/100</strong>). Institutional quality is reflected in the Corruption Perceptions Index (TI) (<strong>", n(r$Corr_Percep_Score), "</strong>), property rights (<strong>", n(r$Property_Rights), "</strong>), and investment freedom (<strong>", n(r$Investment_Freedom), "</strong>).</p>")
  s2 <- paste0("<p>Macro conditions show GDP PPP of <strong>", ifelse(is.na(r$GDP_B), "N/A", paste0("$", n(r$GDP_B), "B")), "</strong>, FDI inflow of <strong>", ifelse(is.na(r$FDI_B), "N/A", paste0("$", n(r$FDI_B), "B")), "</strong>, inflation at <strong>", pct(r$Inflation), "</strong>, and unemployment at <strong>", pct(r$Unemployment), "</strong>.</p>")
  s3 <- paste0("<p>Real estate market evidence is ", ifelse(is.na(r$Mkt_Transparency), "limited by missing MSCI/JLL transparency data", paste0("supported by transparency score <strong>", n(r$Mkt_Transparency, 2), "</strong>")), ". From a portfolio lens, this market currently screens as <strong>", r$Tier, "</strong>.</p>")
  paste0("<div class='ai-insight-text'><span class='ai-badge'>✦ Fallback Summary</span><br><br>", s1, s2, s3, "</div>")
}

call_gemini_summary <- function(country, r) {
  api_key <- gemini_api_key()
  if (identical(api_key, "")) return(list(ok = FALSE, text = "GEMINI_API_KEY not found."))
  prompt <- paste(
    "You are advising an institutional real-estate investor.",
    "Write a concise market brief in plain English.",
    "Output format: exactly 3 short paragraphs plus 1 one-line takeaway.",
    "Length limit: 120-170 words total.",
    "Do not invent facts. If key evidence is missing, state that briefly and move on.",
    "", build_country_context(r), sep = "\n"
  )
  gen <- gemini_generate_content(list(
    contents = list(list(parts = list(list(text = prompt)))),
    generationConfig = list(temperature = 0.25, maxOutputTokens = 9999)
  ))
  if (!isTRUE(gen$ok)) return(list(ok = FALSE, text = gemini_strip_cli(gen$last_error)))
  trunc_note <- ""
  if (!is.na(gen$finish_reason) && !identical(gen$finish_reason, "STOP"))
    trunc_note <- paste0("<div style='margin-top:10px; color:#8b9eb0; font-size:11px;'>Model finish reason: ", htmltools::htmlEscape(as.character(gen$finish_reason)), " (response may be truncated).</div>")
  gemini_html <- paste0("<div class='ai-insight-text'><span class='ai-badge'>✦ Gemini Summary</span><br><br>", paste0("<p>", gsub("\n\n", "</p><p>", gsub("\n", "<br>", gen$text)), "</p>"), trunc_note, "</div>")
  list(ok = TRUE, text = gemini_html)
}

# =============================================================================
# CUSTOM CSS
# =============================================================================

metlife_css <- "

  /* --- THEME VARIABLES --- */
  :root {
    --bg-dark: #1e2227;
    --bg-darker: #181a1f;
    --accent-blue: #61afef;
    --accent-green: #98c379;
    --text-main: #abb2bf;
    --text-bright: #ffffff;
    --border-color: #3e4451;
    --header-height: 45px;
  }


  /* --- GENERAL BODY --- */
  body, .content-wrapper, .right-side { 
    background-color: var(--bg-dark) !important;
    color: var(--text-main); 
    font-family: 'Inter', 'Segoe UI', sans-serif; 
  }

  /* --- BOXES --- */
  .box { 
    border-top: 3px solid var(--accent-blue) !important; 
    border-radius: 4px; 
    background-color: var(--bg-darker) !important;
    box-shadow: 0 4px 6px rgba(0,0,0,0.3);
    border-left: none !important;
    border-right: none !important;
    border-bottom: none !important;
  }
  .box-header, .box-body { background-color: transparent !important; color: var(--text-main); }


  /* --- GLOBAL TABLE WRAPPING FIX --- */
  /* This prevents the 'NOT PREFERRED' wrap by forcing single-line text */
  table.dataTable thead th, 
  table.dataTable tbody td {
    white-space: nowrap !important;
    vertical-align: middle !important;
  }


  /* Enable horizontal scrolling if the table gets wide */
  .dataTables_wrapper {
    overflow-x: auto !important;
    width: 100% !important;
  }


  /* --- TABLES (Clean IDE Style) --- */
  table.dataTable {
      background-color: var(--bg-darker) !important;
      margin: 0 !important;
  }


  table.dataTable tbody tr { 
      background-color: var(--bg-darker) !important; 
      color: var(--text-main) !important;
  }


  table.dataTable tbody td {
      border-top: 1px solid var(--border-color) !important;
  }


  .dataTables_wrapper .dataTables_scrollHeadInner table thead th,
  table.dataTable thead th {
      color: var(--text-bright) !important;
      background-color: #282c34 !important; 
      border-bottom: 2px solid var(--accent-blue) !important;
      text-transform: uppercase;
      font-size: 0.85em;
      letter-spacing: 1px;
      padding: 12px 15px !important;
  }


  /* --- INPUTS & SELECTIZE --- */
  input[type='text'], input[type='search'], .form-control, .selectize-input, .selectize-dropdown {
    color: var(--accent-green) !important;        
    background-color: #121417 !important; 
    border: 1px solid var(--border-color) !important;    
    font-family: 'Monaco', 'Consolas', monospace !important; 
    border-radius: 4px !important;
    box-shadow: none !important;
  }


  .selectize-dropdown-content .active {
    background-color: var(--accent-blue) !important;
    color: var(--bg-darker) !important;
  }


  /* --- SIDEBAR & NAV --- */
  .main-header .navbar, .main-header .logo { 
    background-color: var(--bg-darker) !important; 
    border-bottom: 1px solid var(--border-color);
  }


  .main-sidebar { background-color: var(--bg-darker) !important; }


  /* --- HEADER & LAYOUT FIXES --- */
  .main-header { max-height: var(--header-height) !important; }
  .main-header .logo {
    height: var(--header-height) !important;      
    line-height: var(--header-height) !important; 
    font-weight: bold;
    color: var(--accent-blue) !important;
  }


  .main-header .navbar { min-height: var(--header-height) !important; }
  .main-sidebar, .left-side { padding-top: var(--header-height) !important; }


  .main-header .sidebar-toggle {
    height: var(--header-height) !important;
    line-height: var(--header-height) !important; 
    padding: 0 15px !important;
  }


  /* --- SCROLLBAR --- */
  ::-webkit-scrollbar { width: 8px; height: 8px; }
  ::-webkit-scrollbar-track { background: var(--bg-darker); }
  ::-webkit-scrollbar-thumb { background: #3e4451; border-radius: 4px; }
  ::-webkit-scrollbar-thumb:hover { background: var(--accent-blue); }


  

  /* Sidebar Table Specifics */
  #small-data-table-container table.dataTable {
      border-collapse: collapse !important; 
      width: 100% !important;
      margin-top: 0 !important;
  }


  /* REMOVE HEADER BOTTOM BORDER */
  #small-data-table-container table.dataTable thead th {
      border-bottom: none !important; 
      padding: 8px !important;
  }


  /* REMOVE ROW TOP BORDER */
  #small-data-table-container table.dataTable tbody td {
      border-top: none !important;
  }



  /* --- REFINED TABLE HEADERS --- */
  .dataTables_wrapper .dataTables_scrollHeadInner table thead th,
  table.dataTable thead th {
      color: var(--text-bright) !important;
      background-color: #282c34 !important; 
      border-bottom: 2px solid var(--accent-blue) !important;
      text-transform: uppercase;
      font-size: 0.85em;
      letter-spacing: 1px;
      padding: 12px 15px !important;


      /* --- ARROW ALIGNMENT FIX --- */
      vertical-align: middle !important;
      white-space: nowrap !important;
      cursor: pointer;
  }


  /* Target the DataTables sorting icons specifically */
  table.dataTable thead .sorting,
  table.dataTable thead .sorting_asc,
  table.dataTable thead .sorting_desc {
      background-position: right 10px center !important; /* Fixed padding from the right */
      padding-right: 30px !important; /* Creates clear space for the arrows */
  }


  /* Optional: If your arrows are using pseudo-elements (common in newer DT) */
  table.dataTable thead > tr > th.sorting:before,
  table.dataTable thead > tr > th.sorting:after,
  table.dataTable thead > tr > th.sorting_asc:before,
  table.dataTable thead > tr > th.sorting_asc:after {
      right: 8px !important; /* Uniform distance from right edge */
      opacity: 0.4;           /* Subtler look for that 'pro' feel */
  }


  table.dataTable thead > tr > th.sorting_asc:before,
  table.dataTable thead > tr > th.sorting_desc:after {
      opacity: 1;             /* Highlights the active sort arrow */
      color: var(--accent-blue);
  }

/* --- HEADER CLEANUP FIX --- */
  /* This targets ONLY the empty auto-generated row from the scroll body */
  /* We leave the .filters row alone so your search inputs stay visible */
  .dataTables_scrollBody thead tr:not(.filters) {
    visibility: collapse !important;
    line-height: 0 !important;
    height: 0 !important;
  }
  
  /* Ensure the main header remains high-quality and visible */
  .dataTables_scrollHead table {
    margin-bottom: 0 !important;
  }
  
  /* --- DATA TABLES BUTTON (COPY) --- */
.dt-button {
  background-color: transparent !important;
  color: #61afef !important;              /* light blue */
  border: 1px solid #61afef !important;
  font-weight: 600;
  letter-spacing: 0.5px;
}

.dt-button:hover,
.dt-button:focus {
  background-color: #61afef !important;
  color: #181a1f !important;              /* dark background */
  border-color: #61afef !important;
}

/* --- SHINY DOWNLOAD BUTTON --- */
.btn,
.btn-default {
  background-color: transparent !important;
  color: #61afef !important;
  border: 1px solid #61afef !important;
  font-weight: 600;
}

.btn:hover,
.btn:focus,
.btn-default:hover,
.btn-default:focus {
  background-color: #61afef !important;
  color: #181a1f !important;
  border-color: #61afef !important;
}

  /* --- TIER BADGES (kept from dashboard for functionality) --- */
  .tier-badge{display:inline-block;padding:3px 10px;border-radius:12px;font-size:11px;font-weight:600;letter-spacing:.3px;white-space:nowrap;}
  .tier-core{background:rgba(26,82,118,.45);color:#7fb3d3;border:1px solid #1a5276;}
  .tier-valueadd{background:rgba(31,97,141,.45);color:#85c1e9;border:1px solid #1f618d;}
  .tier-opportun{background:rgba(93,173,226,.25);color:#5dade2;border:1px solid #5dade2;}
  .tier-watch{background:rgba(170,183,184,.2);color:#aab7b8;border:1px solid #aab7b8;}

  /* --- AI INSIGHT PANEL (kept from dashboard for functionality) --- */
  .ai-insight-panel{background:linear-gradient(135deg,rgba(97,175,239,.06) 0%,rgba(24,26,31,.8) 100%);border:1px solid rgba(97,175,239,.25);border-left:3px solid var(--accent-blue);border-radius:6px;padding:16px 18px;margin-bottom:14px;}
  .detail-gemini-insight-wrap{padding:0;margin:0;min-height:0;line-height:0;}
  .ai-insight-title{font-family:'Monaco','Consolas',monospace;font-size:11px;text-transform:uppercase;letter-spacing:1.5px;color:var(--accent-blue);margin-bottom:8px;}
  .ai-badge{display:inline-block;background:rgba(97,175,239,.18);color:var(--accent-blue);font-size:10px;font-weight:600;letter-spacing:.5px;padding:2px 7px;border-radius:10px;margin-right:8px;border:1px solid rgba(97,175,239,.3);}
  .ai-insight-text{font-size:13px;line-height:1.7;color:var(--text-main);}
  .ai-insight-placeholder{color:var(--text-main);font-size:13px;line-height:1.6;opacity:0.6;}
  .ai-insight-error{color:#e06c75;font-size:13px;line-height:1.6;}
  .ai-loading{color:var(--accent-blue);font-size:12px;font-family:'Monaco','Consolas',monospace;}

  /* --- METRIC CARDS (kept from dashboard for functionality) --- */
  .metric-card{background:var(--bg-dark);border:1px solid var(--border-color);border-radius:6px;padding:12px 14px;margin-bottom:10px;}
  .metric-label{font-size:10px;text-transform:uppercase;letter-spacing:.8px;color:var(--text-main);opacity:0.6;margin-bottom:4px;}
  .metric-value{font-family:'Monaco','Consolas',monospace;font-size:18px;font-weight:500;color:var(--text-bright);}
  .metric-value.positive{color:var(--accent-green);}
  .metric-value.negative{color:#e06c75;}
  .metric-value.neutral{color:var(--accent-blue);}
  .score-bar-wrap{background:rgba(255,255,255,.08);border-radius:3px;height:5px;margin-top:5px;overflow:hidden;}
  .score-bar-fill{height:5px;border-radius:3px;background:linear-gradient(90deg,var(--accent-blue),var(--accent-green));transition:width .6s ease;}
  .section-divider{border:none;border-top:1px solid var(--border-color);margin:16px 0;}
  .factor-help{color:var(--text-main);font-size:12px;line-height:1.6;opacity:0.7;}
  .weight-chip{display:inline-block;padding:4px 10px;border-radius:12px;border:1px solid var(--border-color);background:rgba(97,175,239,.08);color:var(--accent-blue);font-size:11px;margin-right:8px;margin-bottom:8px;}

  /* --- COUNTRY HEADER --- */
  .country-header{font-family:'Inter','Segoe UI',sans-serif;font-size:22px;font-weight:600;color:var(--text-bright);margin-bottom:4px;}
  .country-sub{font-size:12px;color:var(--text-main);text-transform:uppercase;letter-spacing:1px;opacity:0.7;}

  /* --- COUNTRY DETAIL TOOLBAR --- */
  .country-detail-toolbar{display:flex;flex-wrap:wrap;gap:12px;align-items:flex-start;width:100%;}
  .country-detail-toolbar .country-detail-select-wrap{flex:1 1 200px;min-width:180px;}
  .country-detail-toolbar .country-detail-select-wrap .shiny-input-container{margin-bottom:0!important;}
  .country-detail-toolbar .country-detail-gemini-wrap{flex-shrink:0;padding-top:26px;display:flex;align-items:center;}

  /* --- SLIDER --- */
  .irs--shiny .irs-bar,.irs--shiny .irs-handle{background-color:var(--accent-blue)!important;border-color:var(--accent-blue)!important;}

  /* --- LABELS --- */
  label{color:var(--text-main)!important;font-size:12px;font-weight:500;letter-spacing:.3px;text-transform:uppercase;}

  /* --- PAGINATION --- */
  .dataTables_info,.dataTables_paginate{color:var(--text-main)!important;font-size:12px;}
"


clipboard_copy_js <- "
Shiny.addCustomMessageHandler('copyTsvToClipboard', function(message) {
  var text = (message && message.text != null) ? String(message.text) : '';
  if (!text.length) return;
  function copyViaTextarea(str) {
    var ta = document.createElement('textarea');
    ta.value = str; ta.setAttribute('readonly','');
    ta.style.cssText = 'position:fixed;top:0;left:0;opacity:0;width:1px;height:1px;';
    document.body.appendChild(ta); ta.focus();
    if (typeof ta.setSelectionRange==='function') ta.setSelectionRange(0,str.length); else ta.select();
    var ok=false; try{ok=document.execCommand('copy');}catch(e){}
    document.body.removeChild(ta); return ok;
  }
  var ok = copyViaTextarea(text);
  if (!ok && navigator.clipboard && window.isSecureContext && navigator.clipboard.writeText)
    navigator.clipboard.writeText(text).catch(function(){copyViaTextarea(text);});
});
"

# =============================================================================
# HELPERS
# =============================================================================

# lbl_slider: wraps sliderInput with a small descriptive caption underneath.
# desc  = short plain-English explanation shown in muted text below the slider.
# dir   = optional direction tag: "↑ higher = better" / "↓ lower = better"
lbl_slider <- function(inputId, label, desc, dir = NULL, ...) {
  dir_html <- if (!is.null(dir))
    tags$span(style = "color:#4a728a; font-style:italic;", paste0(" · ", dir))
  tagList(
    sliderInput(inputId, label, ...),
    tags$div(
      style = "font-size:9px; color:#4a6a8a; margin-top:-13px; margin-bottom:3px; line-height:1.35; padding-left:1px;",
      desc, dir_html
    )
  )
}

fmt_num <- function(x, digits = 1, suffix = "") { if (is.na(x)) "N/A" else paste0(round(x, digits), suffix) }
fmt_pct <- function(x, digits = 1) { if (is.na(x)) "N/A" else paste0(round(x * 100, digits), "%") }
fmt_b   <- function(x, digits = 1) {
  if (is.na(x)) "N/A"
  else if (abs(x) >= 1000) paste0("$", round(x / 1000, 1), "T")
  else paste0("$", round(x, digits), "B")
}

tier_badge_html <- function(tier) {
  rank <- dplyr::case_when(
    identical(tier, "Tier 1 — Core") ~ 1L,
    identical(tier, "Tier 2 — Value-Add") ~ 2L,
    identical(tier, "Tier 3 — Opportunistic") ~ 3L,
    identical(tier, "Watch") ~ 4L,
    TRUE ~ 99L
  )
  cls <- switch(tier,
                "Tier 1 — Core"          = "tier-core",
                "Tier 2 — Value-Add"     = "tier-valueadd",
                "Tier 3 — Opportunistic" = "tier-opportun",
                "tier-watch")
  paste0('<span style="display:none;">', rank, '</span><span class="tier-badge ', cls, '">', tier, '</span>')
}

format_main_grid_for_clipboard <- function(rows_df) {
  rows_df %>%
    select(Country, Region, Tier, RE_Score, Corr_Percep_Score, Mkt_Transparency,
           Investment_Freedom, FDI_B, GDP_B, HDI, Unemployment, Inflation,
           `10yr_Average_TROR`, `Current_Cap_Rate`) %>%
    mutate(
      RE_Score = round(RE_Score, 1), Corr_Percep_Score = round(Corr_Percep_Score, 1),
      Mkt_Transparency = round(Mkt_Transparency, 2), Investment_Freedom = round(Investment_Freedom, 1),
      FDI_B = round(FDI_B, 1), GDP_B = round(GDP_B, 0), HDI = round(HDI, 3),
      Unemployment = ifelse(is.na(Unemployment), "N/A", paste0(round(Unemployment*100,1),"%")),
      Inflation = ifelse(is.na(Inflation), "N/A", paste0(round(Inflation*100,1),"%")),
      `10yr_Average_TROR` = ifelse(is.na(`10yr_Average_TROR`), "N/A", round(`10yr_Average_TROR`,3)),
      `Current_Cap_Rate`  = ifelse(is.na(`Current_Cap_Rate`),  "N/A", round(`Current_Cap_Rate`,3))
    ) %>%
    rename(`RE Score`=RE_Score,`TI Corr. Percept.`=Corr_Percep_Score,`Transparency`=Mkt_Transparency,
           `Invest. Freedom`=Investment_Freedom,`FDI ($B)`=FDI_B,`GDP ($B)`=GDP_B,
           `10Y TROR`=`10yr_Average_TROR`,`Cap Rate`=`Current_Cap_Rate`)
}

df_to_clipboard_tsv <- function(df) {
  paste(capture.output(utils::write.table(df, sep="\t", row.names=FALSE, quote=FALSE, eol="\n")), collapse="\n")
}

build_country_detail_export <- function(row) {
  if (nrow(row) == 0) return(tibble::tibble())
  r <- row[1, , drop = FALSE]
  tibble::tibble(
    Country=r$Country, Region=r$Region, Tier=as.character(r$Tier),
    `RE Score`=round(r$RE_Score,1), `Corruption Perceptions`=round(r$Corr_Percep_Score,1), HDI=round(r$HDI,3),
    `Life Expectancy (yrs)`=round(r$Life_Expectancy,1), `Population (M)`=round(r$Pop_M,1),
    `GDP PPP ($B)`=round(r$GDP_B,1), `FDI ($B)`=round(r$FDI_B,1),
    Unemployment=ifelse(is.na(r$Unemployment),"N/A",paste0(round(r$Unemployment*100,1),"%")),
    Inflation=ifelse(is.na(r$Inflation),"N/A",paste0(round(r$Inflation*100,1),"%")),
    `Public Debt / GDP`=ifelse(is.na(r$Public_Debt_Ratio),"N/A",paste0(round(r$Public_Debt_Ratio*100,1),"%")),
    `Business Freedom`=round(r$Business_Freedom,1), `Tax Burden`=round(r$Tax_Burden,1),
    `Trade Freedom`=round(r$Trade_Freedom,1), `Investment Freedom`=round(r$Investment_Freedom,1),
    `Financial Freedom`=round(r$Financial_Freedom,1), `Property Rights`=round(r$Property_Rights,1),
    `Market Transparency`=round(r$Mkt_Transparency,2),
    `Transparency — Investment`=round(r$Mkt_Trans_Invest,2),
    `Transparency — Fundamentals`=round(r$Mkt_Trans_Fund,2),
    `Transparency — Legal`=round(r$Mkt_Trans_Legal,2),
    `Transparency — Sustainability`=round(r$Mkt_Trans_Sustain,2),
    `Country Risk Premium`=ifelse(is.na(r$Country_Risk_Premium),"N/A",round(r$Country_Risk_Premium,4)),
    `Country Risk Z`=ifelse(is.na(r$Country_Risk_Z),"N/A",round(r$Country_Risk_Z,4)),
    `10Y TROR`=ifelse(is.na(r$`10yr_Average_TROR`),"N/A",round(r$`10yr_Average_TROR`,3)),
    `Current Cap Rate`=ifelse(is.na(r$`Current_Cap_Rate`),"N/A",round(r$`Current_Cap_Rate`,3))
  )
}

fetch_google_news <- function(scope, value, max_items = 12) {
  parse_rss_datetime <- function(x) {
    if (is.na(x) || identical(trimws(x), "")) return(NA_real_)
    s <- sub("^[A-Za-z]{3},\\s*", "", trimws(x))
    tz_map <- c("GMT"="+0000","UTC"="+0000","EST"="-0500","EDT"="-0400","CST"="-0600","CDT"="-0500","MST"="-0700","MDT"="-0600","PST"="-0800","PDT"="-0700","CET"="+0100","CEST"="+0200")
    for (abbr in names(tz_map)) s <- sub(paste0("\\s",abbr,"$"), paste0(" ",tz_map[[abbr]]), s)
    t1 <- suppressWarnings(as.POSIXct(s, format="%d %b %Y %H:%M:%S %z", tz="UTC"))
    if (!is.na(t1)) return(as.numeric(t1))
    t2 <- suppressWarnings(as.POSIXct(s, tz="UTC"))
    if (!is.na(t2)) return(as.numeric(t2))
    NA_real_
  }
  topic <- if (scope == "Country") paste0(value," real estate investment regulation economy")
  else paste0(value," real estate investment regulation economy development")
  rss_url <- paste0("https://news.google.com/rss/search?q=",URLencode(topic,reserved=TRUE),"&hl=en-US&gl=US&ceid=US:en")
  rss_doc <- tryCatch(read_xml(rss_url), error=function(e) NULL)
  if (is.null(rss_doc)) return(data.frame())
  items <- xml_find_all(rss_doc, "//item")
  if (length(items) == 0) return(data.frame())
  get_node_text <- function(item, xpath) {
    node <- xml_find_first(item, xpath)
    if (inherits(node, "xml_missing")) return(NA_character_)
    txt <- xml_text(node)
    if (is.null(txt) || identical(txt,"")) NA_character_ else txt
  }
  clean_news_snippet <- function(x) {
    if (is.na(x) || identical(trimws(x),"")) return(NA_character_)
    s <- gsub("<[^>]+>"," ",x); s <- gsub("&nbsp;"," ",s,fixed=TRUE)
    s <- gsub("&amp;","&",s,fixed=TRUE); s <- gsub("&quot;","\"",s,fixed=TRUE)
    s <- gsub("&#39;","'",s,fixed=TRUE); s <- stringr::str_squish(s)
    if (nchar(s)>220) s <- paste0(substr(s,1,217),"...")
    if (identical(s,"")) NA_character_ else s
  }
  news_df <- tibble(
    title    = vapply(items, get_node_text, character(1), xpath="./title"),
    link     = vapply(items, get_node_text, character(1), xpath="./link"),
    pub_date = vapply(items, get_node_text, character(1), xpath="./pubDate"),
    source   = vapply(items, get_node_text, character(1), xpath="./source"),
    summary  = vapply(items, get_node_text, character(1), xpath="./description")
  ) %>%
    filter(!is.na(title),title!="",!is.na(link),link!="") %>%
    mutate(
      pub_date=as.character(pub_date),
      summary=vapply(summary,clean_news_snippet,character(1)),
      pub_datetime_num=vapply(pub_date,parse_rss_datetime,numeric(1)),
      pub_datetime=as.POSIXct(pub_datetime_num,origin="1970-01-01",tz="UTC")
    ) %>%
    filter(!is.na(pub_datetime), pub_datetime>=(Sys.time()-365*24*60*60)) %>%
    arrange(desc(pub_datetime))
  head(select(news_df,-pub_datetime,-pub_datetime_num), max_items)
}

summarize_news_with_gemini <- function(scope, value, news_df) {
  api_key <- gemini_api_key()
  if (nrow(news_df)==0) return(list(summary_html="<p class='ai-insight-placeholder'>No recent Google News items were found for this selection.</p>", raw=""))
  if (identical(api_key,"")) return(list(summary_html="<p class='ai-insight-error'>Gemini API key missing.</p>", raw=""))
  headlines <- paste0(seq_len(nrow(news_df)),". ",news_df$title," (",news_df$source,", ",news_df$pub_date,")",collapse="\n")
  prompt <- paste0("You are an investment research analyst for real estate markets.\nContext scope: ",scope,"\nSelected target: ",value,"\n\nBelow are recent Google News headlines:\n",headlines,"\n\nProduce a concise briefing with exactly four sections and clear bullet points:\n1) Regulatory / Policy Changes\n2) Economic Development Signals\n3) Real-Estate Investment Relevance\n4) Risks / Watch Items\n\nRequirements:\n- Focus only on materially relevant information for institutional real-estate investing.\n- If evidence is weak, explicitly say confidence is low.\n- Keep it to around 160-220 words total.\n- Return plain HTML only, using <h4>, <ul>, <li>, <p> tags (no markdown fences).")
  gen <- gemini_generate_content(list(contents=list(list(parts=list(list(text=prompt)))),generationConfig=list(temperature=0.25,maxOutputTokens=9999)))
  if (isTRUE(gen$ok)) return(list(summary_html=gen$text,raw=gen$text))
  err_txt <- gemini_strip_cli(gen$last_error)
  if (gemini_is_quota_error(err_txt)) {
    return(list(summary_html = build_news_fallback_summary(scope, value, news_df, quota_blocked = TRUE), raw = ""))
  }
  list(summary_html = build_news_fallback_summary(scope, value, news_df, quota_blocked = FALSE), raw = "")
}

fetch_gemini_news <- function(msa_name, scan_type="pulse") {
  api_key <- trimws(gemini_api_key())
  if (!nzchar(api_key)||nchar(api_key)<10) return(NULL)
  clean_city <- str_split(msa_name,"-|,")[[1]][1]
  url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
  if (scan_type=="warning") {
    search_prompt <- paste0("SEARCH 1: '",clean_city," rent control tax increase legislation news' SEARCH 1: '",clean_city," ballot measure real estate news' SEARCH 2: '",clean_city," city council zoning ordinance real estate journal'")
    focus_prompt <- "Prioritize 'Early Warning' signals: Rent Control discussions, Property Tax hikes, Zoning changes, and specific ordinances."
    time_prompt <- "Exclude articles > 12 months old."
  } else {
    search_prompt <- paste0("SEARCH 1: '",clean_city," Commercial Real Estate transactions news last 3 months' SEARCH 2: '",clean_city," business journal economic development news'")
    focus_prompt <- "Prioritize large transactions (>$50M), corporate relocations, and general local economic business conditions."
    time_prompt <- "Exclude articles > 3 months old."
  }
  prompt <- paste0("You run Research & Strategy for a major institutional real estate investment manager. Perform these specific Google Searches for ",clean_city,": ",search_prompt,"Consolidate results into 10 distinct items. ",focus_prompt," ",time_prompt," Return the result as a raw JSON list of objects. Each object must have exactly these keys: 'headline', 'source', 'date', 'summary', 'url'. Do NOT use Markdown formatting. Just return the raw JSON text.")
  empty_tool_config <- structure(list(), names=character(0))
  body <- list(contents=list(list(parts=list(list(text=prompt)))),tools=list(list(google_search=empty_tool_config)))
  tryCatch({
    resp <- request(url) %>% req_method("POST") %>% req_headers("x-goog-api-key"=api_key) %>%
      req_body_json(body,auto_unbox=TRUE) %>% req_timeout(90) %>%
      req_retry(max_tries=5,backoff=function(i)5+runif(1,0,5),is_transient=function(resp){s<-resp_status(resp);s==429||s>=500}) %>%
      req_perform()
    result <- resp %>% resp_body_json()
    if (is.null(result$candidates[[1]]$content$parts[[1]]$text)) return(NULL)
    raw_text <- result$candidates[[1]]$content$parts[[1]]$text
    clean_text <- trimws(gsub("```","",gsub("```json","",raw_text)))
    clean_text <- gsub("\\$","$",clean_text,fixed=TRUE)
    clean_text <- gsub("\\%","%",clean_text,fixed=TRUE)
    fromJSON(clean_text)
  }, error=function(e){message(paste("Gemini API Error for",msa_name,"(",scan_type,"):",conditionMessage(e)));NULL})
}

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  title = "MetLife IM - Real Estate Dashboard",
  skin  = "black",
  dashboardHeader(
    title = tags$img(
      src = "custom_assets/MIM_logo_TwoLine_white_text.png",
      alt = "MetLife Investment Management",
      style = "height:42px;width:auto;max-width:100%;display:block;margin:4px 0;"
    ),
    titleWidth = 240
  ),
  dashboardSidebar(
    width = 240,
    tags$head(tags$style(HTML(metlife_css))),
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview",          tabName = "overview",  icon = icon("globe")),
      menuItem("Country Detail",    tabName = "detail",    icon = icon("map-marker")),
      menuItem("Market Comparison", tabName = "compare",   icon = icon("bar-chart")),
      menuItem("News Intelligence", tabName = "news",      icon = icon("newspaper")),
      menuItem("Return Model", tabName = "return_model", icon = icon("chart-line"))
    ),
    hr(style="border-color:#1e3a5a;margin:10px 16px;"),
    div(style="padding:16px;margin-top:10px;",
        tags$p(style="font-size:10px;color:#4a6a8a;line-height:1.5;margin:0;",
               "MetLife Investment Management",tags$br(),"Global Real Estate Dashboard",tags$br(),
               paste0("Data refreshed: ",format(Sys.Date(),"%b %Y")))
    )
  ),
  dashboardBody(
    tags$head(tags$script(HTML(clipboard_copy_js))),
    tabItems(
      
      # =====================================================================
      # OVERVIEW
      # =====================================================================
      tabItem(tabName = "overview",
              fluidRow(column(12,
                              tags$div(style="margin-bottom:20px;",
                                       tags$h1(style="font-family:'Playfair Display',serif;font-size:24px;color:#f0f4f8;margin:0;","Preferred Markets Analysis"),
                                       tags$p(style="color:#8b9eb0;font-size:12px;margin:4px 0 0;text-transform:uppercase;letter-spacing:1px;","Real Estate Investment Dashboard")
                              )
              )),
              
              # ── TWO-TIER FACTOR WEIGHTS ──────────────────────────────────────────
              fluidRow(box(
                title  = "Dynamic Factor Weights", width = 12, status = "primary",
                
                tags$details(
                  class = "weight-collapse",
                  open = "open",
                  tags$summary(
                    style = "cursor:pointer;list-style:none;outline:none;margin-bottom:10px;",
                    tags$div(class = "weight-collapse-header",
                             tags$p(class="ai-insight-title",style="display:inline-block;margin:0;",
                                    "Bucket Weights ",
                                    tags$span(class="factor-help",style="text-transform:none;letter-spacing:0;","(relative importance; auto-normalised to 100%)")
                             ),
                             tags$span(class = "weight-collapse-arrow", HTML("&#9662;"))
                    )
                  ),
                  fluidRow(
                    column(2, sliderInput("w_bucket_econ",        "Economic Indicators",   min=0,max=100,value=17,step=1,width="100%"), uiOutput("pct_w_bucket_econ")),
                    column(2, sliderInput("w_bucket_freedom",     "Business Friendliness", min=0,max=100,value=17,step=1,width="100%"), uiOutput("pct_w_bucket_freedom")),
                    column(2, sliderInput("w_bucket_judicial",    "Judicial System",       min=0,max=100,value=17,step=1,width="100%"), uiOutput("pct_w_bucket_judicial")),
                    column(2, sliderInput("w_bucket_re_perf",     "RE Performance",        min=0,max=100,value=17,step=1,width="100%"), uiOutput("pct_w_bucket_re_perf")),
                    column(2, sliderInput("w_bucket_transparency","Market Transparency",   min=0,max=100,value=16,step=1,width="100%"), uiOutput("pct_w_bucket_transparency")),
                    column(2, sliderInput("w_bucket_risk",        "Risk Factor",           min=0,max=100,value=16,step=1,width="100%"), uiOutput("pct_w_bucket_risk"))
                  )
                ),
                hr(class="section-divider"),
                
                tags$details(
                  class = "weight-collapse",
                  open = "open",
                  tags$summary(
                    style = "cursor:pointer;list-style:none;outline:none;margin-bottom:10px;",
                    tags$div(class = "weight-collapse-header",
                             tags$p(class="ai-insight-title",style="display:inline-block;margin:0;",
                                    "Sub-Factor Weights ",
                                    tags$span(class="factor-help",style="text-transform:none;letter-spacing:0;","(auto-normalised to 100% within each bucket)")
                             ),
                             tags$span(class = "weight-collapse-arrow", HTML("&#9662;"))
                    )
                  ),
                  fluidRow(
                    column(2,
                           tags$div(class="metric-label",style="margin-bottom:6px;color:#5dade2;","Economic Indicators"),
                           sliderInput("w_sf_hdi",  "HDI",         min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_hdi"),
                           sliderInput("w_sf_gdp",  "GDP PPP ($B)",min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_gdp"),
                           sliderInput("w_sf_fdi",  "FDI ($B)",    min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_fdi"),
                           sliderInput("w_sf_unemp","Unemployment",min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_unemp"),
                           sliderInput("w_sf_infl", "Inflation",   min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_infl")
                    ),
                    column(2,
                           tags$div(class="metric-label",style="margin-bottom:6px;color:#58d68d;","Business Friendliness"),
                           sliderInput("w_sf_biz",   "Business",   min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_biz"),
                           sliderInput("w_sf_trade", "Trade",      min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_trade"),
                           sliderInput("w_sf_invest","Investment", min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_invest"),
                           sliderInput("w_sf_fin",   "Financial",  min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_fin"),
                           sliderInput("w_sf_tax",   "Tax Burden", min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_tax")
                    ),
                    column(2,
                           tags$div(class="metric-label",style="margin-bottom:6px;color:#f39c12;","Judicial System"),
                           sliderInput("w_sf_prop","Property Rights",min=0,max=100,value=34,step=1,width="100%"), uiOutput("pct_sf_prop"),
                           sliderInput("w_sf_corr_percep", "Corruption Perceptions",      min=0,max=100,value=33,step=1,width="100%"), uiOutput("pct_sf_corr_percep"),
                           sliderInput("w_sf_reg", "Reg. Quality",   min=0,max=100,value=33,step=1,width="100%"), uiOutput("pct_sf_reg")
                    ),
                    column(2,
                           tags$div(class="metric-label",style="margin-bottom:6px;color:#e74c3c;","RE Performance"),
                           sliderInput("w_sf_tror10",  "10Y Avg TROR",   min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_tror10"),
                           sliderInput("w_sf_tror_py", "Prior Yr TROR",  min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_tror_py"),
                           sliderInput("w_sf_tror_vol","TROR Volatility",min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_tror_vol"),
                           sliderInput("w_sf_drawdown","Worst Drawdown", min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_drawdown"),
                           sliderInput("w_sf_caprate", "Cap Rate",       min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_caprate")
                    ),
                    column(2,
                           tags$div(class="metric-label",style="margin-bottom:6px;color:#9b59b6;","Market Transparency"),
                           sliderInput("w_sf_tr_overall","Overall",       min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_tr_overall"),
                           sliderInput("w_sf_tr_invest", "Investment",    min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_tr_invest"),
                           sliderInput("w_sf_tr_fund",   "Fundamentals",  min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_tr_fund"),
                           sliderInput("w_sf_tr_legal",  "Legal",         min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_tr_legal"),
                           sliderInput("w_sf_tr_sustain","Sustainability",min=0,max=100,value=20,step=1,width="100%"), uiOutput("pct_sf_tr_sustain")
                    ),
                    column(2,
                           tags$div(class="metric-label",style="margin-bottom:6px;color:#af7ac5;","Risk Factor"),
                           sliderInput("w_sf_country_risk","Country Risk Premium Z",min=0,max=100,value=100,step=1,width="100%"), uiOutput("pct_sf_country_risk")
                    )
                  )
                ),
                hr(class="section-divider"),
                fluidRow(
                  column(9, div(class="factor-help","Sliders show relative importance and are auto-normalised so each tier always sums to 100%. The RE Score recalculates in real time; countries with missing data for a subfactor are scored using the remaining available factors within that bucket.")),
                  column(3, actionButton("reset_weights","Reset Weights",style="width:100%;background:#c9a84c;color:#0d1b2a;border:none;font-weight:600;margin-top:0;"))
                ),
                br(),
                uiOutput("weight_summary")
              )),
              
              # ── MARKET GRID ──────────────────────────────────────────────────────
              fluidRow(
                box(
                  title = div(style="display:flex;justify-content:space-between;align-items:center;flex-wrap:wrap;gap:10px;width:100%;",
                              span("Market Grid"),
                              div(style="display:flex;gap:8px;align-items:center;flex-wrap:wrap;",
                                  actionButton("btn_main_grid_select_all","Select All",icon=icon("check-square"),style="background:transparent;color:#c4d3df;font-weight:600;border:1px solid #8b9eb0;border-radius:4px;padding:6px 12px;font-size:12px;"),
                                  actionButton("btn_main_grid_deselect","Deselect",icon=icon("square"),style="background:transparent;color:#8b9eb0;font-weight:600;border:1px solid #4a6a8a;border-radius:4px;padding:6px 12px;font-size:12px;"),
                                  downloadButton("dl_main_grid_csv_all","Download all",style="background:transparent;color:#58d68d;font-weight:600;border:1px solid #58d68d;border-radius:4px;padding:6px 12px;font-size:12px;"),
                                  downloadButton("dl_main_grid_csv","Download selected",style="background:transparent;color:#c9a84c;font-weight:600;border:1px solid #c9a84c;border-radius:4px;padding:6px 12px;font-size:12px;"),
                                  actionButton("btn_copy_main_grid","Copy",icon=icon("copy"),style="background:#c9a84c;color:#0d1b2a;font-weight:600;border:none;border-radius:4px;padding:6px 14px;font-size:12px;")
                              )
                  ),
                  width=8, status="primary",
                  fluidRow(class="market-grid-filter-row",
                    column(2, uiOutput("region_ui")),
                    column(2, uiOutput("subregion_ui")),
                    column(2, selectInput("sel_tier","Investment Tier",
                                          choices=c("All","Tier 1 — Core","Tier 2 — Value-Add","Tier 3 — Opportunistic","Watch"),
                                          selected="All", width="100%")),
                    column(3, sliderInput("sld_rescore","Min RE Score",min=0,max=100,value=0,step=5,width="100%")),
                    column(3, class="market-grid-search-col", div(class="market-grid-filter-search dataTables_filter",
                                  textInput("main_grid_search","Search",value="",placeholder="Search...",width="100%")))
                  ),
                  div(class="market-grid-table-wrap",withSpinner(DTOutput("main_table"),type=4,color="#c9a84c",size=0.7))
                ),
                box(title="Country Insight",width=4,status="primary",uiOutput("country_panel"))
              ),
              fluidRow(
                box(title="RE Attractiveness Score by Country",width=8,status="primary",
                    withSpinner(plotlyOutput("bar_rescore",height="320px"),type=4,color="#c9a84c",size=0.7)),
                box(title="Tier Distribution",width=4,status="primary",
                    withSpinner(plotlyOutput("pie_tier",height="320px"),type=4,color="#c9a84c",size=0.7))
              ),
              fluidRow(
                box(
                  title = tags$div(
                    style = "display:flex;align-items:center;justify-content:space-between;gap:12px;flex-wrap:wrap;",
                    tags$span("Global Factor Importance Map"),
                    tags$div(
                      class = "overview-map-header-select",
                      selectInput(
                        "map_bucket_select",
                        NULL,
                        choices = c(
                          "Economic Indicators",
                          "Business Friendliness",
                          "Judicial System",
                          "RE Performance",
                          "Market Transparency",
                          "Risk Factor"
                        ),
                        selected = "Economic Indicators",
                        width = "260px"
                      )
                    )
                  ),
                  width = 12,
                  status = "primary",
                  withSpinner(
                    plotlyOutput("factor_map", height = "600px"),
                    type = 4, color = "#c9a84c", size = 0.7
                  )
                )
              )
      ), # end overview
      
      # =====================================================================
      # COUNTRY DETAIL
      # =====================================================================
      tabItem(tabName="detail",
              fluidRow(
                class = "country-detail-select-row",
                column(8, class = "country-detail-selectors-outer",
                       tags$div(class = "country-detail-selectors-block",
                                fluidRow(
                                  column(6, uiOutput("country_detail_ui")),
                                  column(6, uiOutput("krish_country_compare_ui"))
                                ),
                                uiOutput("krish_detail_header_info")
                       )
                ),
                column(4, class = "country-detail-actions-outer",
                       tags$div(class = "country-detail-actions-toolbar",
                                tags$div(class = "country-detail-actions-a",
                                         div(class="metric-label", style="text-align:right;margin-bottom:8px;", "Country A"),
                                         div(style="display:flex;flex-wrap:wrap;gap:10px;align-items:center;justify-content:flex-end;",
                                             downloadButton("dl_country_detail_csv","Download .CSV",style="background:transparent;color:#58d68d;font-weight:600;border:1px solid #58d68d;border-radius:4px;padding:8px 16px;font-size:12px;"),
                                             actionButton("btn_country_detail_copy","Copy",icon=icon("copy"),style="background:transparent;color:#c9a84c;font-weight:600;border:1px solid #c9a84c;border-radius:4px;padding:8px 16px;font-size:12px;")
                                         )
                                ),
                                tags$div(class = "country-detail-actions-b",
                                         div(class="metric-label", style="text-align:right;margin-bottom:8px;", "Country B"),
                                         div(style="display:flex;flex-wrap:wrap;gap:10px;align-items:center;justify-content:flex-end;",
                                             downloadButton("dl_country_detail_b_csv","Download .CSV",style="background:transparent;color:#58d68d;font-weight:600;border:1px solid #58d68d;border-radius:4px;padding:8px 16px;font-size:12px;"),
                                             actionButton("btn_country_detail_b_copy","Copy",icon=icon("copy"),style="background:transparent;color:#c9a84c;font-weight:600;border:1px solid #c9a84c;border-radius:4px;padding:8px 16px;font-size:12px;")
                                         )
                                )
                       )
                )
              ),
              fluidRow(
                class = "detail-gemini-anchor-row",
                column(12,
                       div(class = "detail-gemini-insight-wrap", style = "height:0;margin:0;padding:0;border:0;overflow:visible;")
                )
              ),
              fluidRow(
                column(4,box(title=uiOutput("detail_box_title_macro"),width=NULL,status="primary",uiOutput("detail_macro"))),
                column(4,box(title=uiOutput("detail_box_title_freedom"),width=NULL,status="primary",uiOutput("detail_freedom"))),
                column(4,box(title=uiOutput("detail_box_title_transparency"),width=NULL,status="primary",uiOutput("detail_transparency")))
              ),
              fluidRow(
                box(title="Transparency Radar",width=6,status="primary",withSpinner(plotlyOutput("radar_chart",height="320px"),type=4,color="#c9a84c",size=0.7)),
                box(title="Freedom Scores vs. Region Average",width=6,status="primary",withSpinner(plotlyOutput("freedom_bar",height="320px"),type=4,color="#c9a84c",size=0.7))
              )
      ),
      
      # =====================================================================
      # MARKET COMPARISON
      # =====================================================================
      tabItem(tabName="compare",
              fluidRow(
                column(8,
                       tags$div(class = "market-compare-toolbar",
                                tags$div(class = "market-compare-select-wrap",
                                         selectInput("compare_x","X Axis",choices=c("RE Score"="RE_Score","HDI"="HDI","Corruption Perceptions"="Corr_Percep_Score","FDI Inflow ($B)"="FDI_B","GDP PPP ($B)"="GDP_B","Market Transparency"="Mkt_Transparency","Country Risk (Z)"="Country_Risk_Z","Investment Freedom"="Investment_Freedom","Unemployment"="Unemployment","Inflation"="Inflation"),selected="GDP_B",width="100%")
                                ),
                                tags$div(class = "market-compare-select-wrap",
                                         selectInput("compare_y","Y Axis",choices=c("RE Score"="RE_Score","HDI"="HDI","Corruption Perceptions"="Corr_Percep_Score","FDI Inflow ($B)"="FDI_B","GDP PPP ($B)"="GDP_B","Market Transparency"="Mkt_Transparency","Country Risk (Z)"="Country_Risk_Z","Investment Freedom"="Investment_Freedom","Unemployment"="Unemployment","Inflation"="Inflation"),selected="RE_Score",width="100%")
                                )
                       )
                )
              ),
              fluidRow(
                box(title="Scatter: Market Positioning",width=8,status="primary",withSpinner(plotlyOutput("scatter_compare",height="450px"),type=4,color="#c9a84c",size=0.7)),
                box(title="Top 15 Markets by RE Score",width=4,status="primary",withSpinner(plotlyOutput("top15_bar",height="450px"),type=4,color="#c9a84c",size=0.7))
              )
      ),
      
      # =====================================================================
      # NEWS INTELLIGENCE
      # =====================================================================
      tabItem(tabName="news",
              fluidRow(box(title="AI News Briefing",width=12,status="primary",
                           fluidRow(
                             column(6,selectizeInput("news_market_select","Select Market:",choices=unique(df_all_raw$Country),selected="United States",width="100%")),
                             column(2,div(style="margin-top:24px;",uiOutput("btn_news_ui"))),
                             column(2,div(style="margin-top:24px;",uiOutput("btn_scan_news_ui")))
                           ),
                           div(class="ai-insight-panel",uiOutput("news_summary"))
              )),
              fluidRow(
                box(title="Market Pulse (Transactions & Econ)",width=6,solidHeader=FALSE,status="primary",uiOutput("news_pulse_ui")),
                box(title="Early Warning System (Policy & Risk)",width=6,solidHeader=FALSE,status="danger",uiOutput("news_warning_ui"))
              )
      ),
      tabItem(tabName = "return_model",
              fluidRow(column(12,
                              tags$div(style="margin-bottom:20px;",
                                       tags$h1(style="font-family:'Playfair Display',serif;font-size:24px;color:#f0f4f8;margin:0;",
                                               "Return Driver Model"),
                                       tags$p(style="color:#8b9eb0;font-size:12px;margin:4px 0 0;text-transform:uppercase;letter-spacing:1px;",
                                              "Estimate risk-adjusted return and identify key performance drivers")
                              )
              )),
              
              fluidRow(
                box(title = "Model Summary", width = 4, status = "primary",
                    htmlOutput("return_model_summary")),
                
                box(title = "Top Return Drivers", width = 8, status = "primary",
                    plotlyOutput("return_driver_plot", height = "360px"))
              ),
              
              fluidRow(
                box(title = "Model Data", width = 12, status = "primary",
                    DTOutput("return_model_table"))
              )
      )
    ) # end tabItems
    
  ) # end dashboardBody
)

# =============================================================================
# SERVER — BASE LAYER
# =============================================================================

server <- function(input, output, session) {
  
  news_data        <- reactiveVal(data.frame())
  dual_scan_running<- reactiveVal(FALSE)
  ai_brief_running <- reactiveVal(FALSE)
  news_pulse_data  <- reactiveVal(NULL)
  news_warning_data<- reactiveVal(NULL)
  news_summary_rv  <- reactiveVal(div(class="ai-insight-placeholder","Select a country/region and click Run AI Briefing to generate an investment-oriented briefing."))
  
  output$btn_news_ui <- renderUI({
    if (isTRUE(dual_scan_running()))
      actionButton("btn_news","✦ Run AI Briefing",disabled="disabled",style="width:100%;background:#7f7f7f;color:#d9d9d9;font-weight:600;border:none;border-radius:4px;padding:8px 12px;font-size:12px;cursor:not-allowed;")
    else
      actionButton("btn_news","✦ Run AI Briefing",style="width:100%;background:#c9a84c;color:#0d1b2a;font-weight:600;border:none;border-radius:4px;padding:8px 12px;font-size:12px;")
  })
  
  output$btn_scan_news_ui <- renderUI({
    if (isTRUE(ai_brief_running()))
      actionButton("btn_scan_news","Run Dual-Scan",icon=icon("satellite-dish"),disabled="disabled",style="background:#7f7f7f;color:#d9d9d9;width:100%;font-weight:bold;border:none;border-radius:4px;padding:8px 12px;font-size:12px;cursor:not-allowed;")
    else
      actionButton("btn_scan_news","Run Dual-Scan",icon=icon("satellite-dish"),style="background-color:#28a745;color:white;width:100%;font-weight:bold;border:none;border-radius:4px;padding:8px 12px;font-size:12px;")
  })
  
  observeEvent(input$btn_scan_news,{
    req(input$news_market_select)
    dual_scan_running(TRUE); on.exit(dual_scan_running(FALSE),add=TRUE)
    withProgress(message=paste("Running dual scan:",input$news_market_select),detail="Checking financial press...",value=0.2,{
      news_pulse_data(fetch_gemini_news(input$news_market_select,scan_type="pulse"))
      incProgress(0.4,detail="Checking local ordinances...")
      news_warning_data(fetch_gemini_news(input$news_market_select,scan_type="warning"))
      incProgress(0.4,detail="Done")
    })
  })
  
  # ── Reset all factor-weight sliders ───────────────────────────────────────
  observeEvent(input$reset_weights,{
    for (bkt in BUCKETS) {
      updateSliderInput(session,paste0("w_bucket_",bkt$id),value=20)
      eq_sf <- round(100/length(bkt$subfactors))
      for (sf in bkt$subfactors) updateSliderInput(session,paste0("w_",sf$id),value=eq_sf)
    }
  })
  
  # ── Model weights reactive (uses helper so Krish layer can call same fn) ──
  model_weights <- reactive({ build_model_weights(input) })
  
  df_scored <- reactive({ compute_re_score(df_all_raw, model_weights()) })
  return_model_data <- reactive({
    df_scored() %>%
      mutate(
        Risk_Adjusted_Return = `10yr_Average_TROR` / `StDev_TROR`
      ) %>%
      select(
        Country, Region, RE_Score, Tier,
        Risk_Adjusted_Return,
        `10yr_Average_TROR`, `StDev_TROR`,
        Inflation, Unemployment,
        Mkt_Transparency,
        Property_Rights,
        Country_Risk_Z
      ) %>%
      filter(
        is.finite(Risk_Adjusted_Return),
        !is.na(RE_Score),
        !is.na(Inflation),
        !is.na(Unemployment),
        !is.na(Mkt_Transparency)
      )
  })
  return_model_fit <- reactive({
    lm(
      Risk_Adjusted_Return ~ RE_Score +
        Inflation + Unemployment +
        Mkt_Transparency +
        Property_Rights +
        Country_Risk_Z,
      data = return_model_data()
    )
  })
  output$return_model_summary <- renderUI({
    fit <- return_model_fit()
    s <- summary(fit)
    
    HTML(paste0(
      "<div class='metric-card'>",
      "<div class='metric-label'>Model R-squared</div>",
      "<div class='metric-value neutral'>", round(s$r.squared, 3), "</div>",
      "</div>",
      "<div class='metric-card'>",
      "<div class='metric-label'>Adjusted R-squared</div>",
      "<div class='metric-value neutral'>", round(s$adj.r.squared, 3), "</div>",
      "</div>",
      "<div class='metric-card'>",
      "<div class='metric-label'>Countries Used</div>",
      "<div class='metric-value neutral'>", nrow(return_model_data()), "</div>",
      "</div>"
    ))
  })
  
  output$return_driver_plot <- renderPlotly({
    fit <- return_model_fit()
    
    coef_df <- broom::tidy(fit) %>%
      filter(term != "(Intercept)") %>%
      mutate(abs_estimate = abs(estimate)) %>%
      arrange(desc(abs_estimate)) %>%
      slice_head(n = 10)
    
    plot_ly(
      coef_df,
      x = ~estimate,
      y = ~reorder(term, estimate),
      type = "bar",
      orientation = "h",
      text = ~round(estimate, 3),
      textposition = "auto"
    ) %>%
      layout(
        xaxis = list(title = "Coefficient"),
        yaxis = list(title = ""),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#8b9eb0")
      )
  })
  
  output$return_model_table <- renderDT({
    datatable(
      return_model_data(),
      options = list(scrollX = TRUE, pageLength = 10),
      rownames = FALSE
    )
  })
  # ── Effective-% labels: buckets ──────────────────────────────────────────
  bucket_eff_pcts <- reactive({
    bkt_raw <- setNames(
      vapply(BUCKETS,function(b){v<-input[[paste0("w_bucket_",b$id)]];if(is.null(v)||is.na(v))20 else v},numeric(1)),
      vapply(BUCKETS,function(b)b$id,character(1))
    )
    tot <- sum(bkt_raw)
    if(tot>0) round(bkt_raw/tot*100,1) else setNames(rep(20,length(bkt_raw)),names(bkt_raw))
  })
  
  for (.bkt in BUCKETS) {
    local({
      bid <- .bkt$id
      output[[paste0("pct_w_bucket_",bid)]] <- renderUI({
        p <- bucket_eff_pcts()[bid]
        tags$div(style="font-size:10px;color:#c9a84c;text-align:center;margin-top:-8px;margin-bottom:6px;font-family:'IBM Plex Mono',monospace;",paste0("Effective: ",p,"%"))
      })
    })
  }
  
  # ── Effective-% labels: subfactors ───────────────────────────────────────
  sf_eff_pcts <- reactive({
    setNames(lapply(BUCKETS,function(bkt){
      sf_raw <- setNames(
        vapply(bkt$subfactors,function(sf){v<-input[[paste0("w_",sf$id)]];if(is.null(v)||is.na(v))20 else v},numeric(1)),
        vapply(bkt$subfactors,function(sf)sf$id,character(1))
      )
      tot <- sum(sf_raw)
      if(tot>0) round(sf_raw/tot*100,1) else setNames(rep(round(100/length(sf_raw),1),length(sf_raw)),names(sf_raw))
    }), vapply(BUCKETS,function(b)b$id,character(1)))
  })
  
  for (.bkt in BUCKETS) {
    for (.sf in .bkt$subfactors) {
      local({
        bid  <- .bkt$id; sfid <- .sf$id
        output[[paste0("pct_",sfid)]] <- renderUI({
          p <- sf_eff_pcts()[[bid]][sfid]
          tags$div(style="font-size:9px;color:#8b9eb0;text-align:center;margin-top:-10px;margin-bottom:4px;font-family:'IBM Plex Mono',monospace;",paste0(p,"%"))
        })
      })
    }
  }
  
  output$weight_summary <- renderUI({
    pcts <- bucket_eff_pcts()
    chips <- lapply(BUCKETS,function(bkt) tags$span(class="weight-chip",paste0(bkt$name,": ",pcts[bkt$id],"%")))
    do.call(tags$div,chips)
  })
  
  output$region_ui <- renderUI({
    regions <- sort(unique(na.omit(df_scored()$Region)))
    selectInput("sel_region","Region",choices=c("All",regions),selected="All",width="100%")
  })
  
  output$subregion_ui <- renderUI({
    d <- df_scored()
    if (!is.null(input$sel_region) && input$sel_region != "All") {
      d <- d %>% filter(Region == input$sel_region)
    }
    subregions <- d %>%
      filter(!is.na(Region_Specific), Region_Specific != "") %>%
      pull(Region_Specific) %>%
      unique() %>%
      sort()
    selectInput("sel_subregion", "Subregion", choices = c("All", subregions), selected = "All", width = "100%")
  })
  
  output$country_detail_ui <- renderUI({
    countries <- sort(unique(na.omit(df_scored()$Country)))
    default_country <- if("United States"%in%countries)"United States" else countries[[1]]
    div(class="country-detail-toolbar",
        div(class="country-detail-select-wrap",selectInput("sel_country_detail","Select Country",choices=countries,selected=default_country,width="100%"))
    )
  })
  
  df_filtered <- reactive({
    d <- df_scored()
    if(!is.null(input$sel_region)&&input$sel_region!="All") d <- filter(d,Region==input$sel_region)
    if(!is.null(input$sel_subregion)&&input$sel_subregion!="All") d <- filter(d,Region_Specific==input$sel_subregion)
    if(!is.null(input$sel_tier)&&input$sel_tier!="All") d <- filter(d,Tier==input$sel_tier)
    filter(d,RE_Score>=input$sld_rescore|is.na(RE_Score))
  })
  
  # ── Market Grid table — grouped two-row header ───────────────────────────
  output$main_table <- renderDT({
    
    dt_container <- htmltools::withTags(table(
      class="display",
      thead(
        tr(
          th(rowspan=2,style="vertical-align:middle;text-align:center;background:#1a2f45;color:#c9a84c;border-bottom:2px solid #c9a84c;","Country"),
          th(rowspan=2,style="vertical-align:middle;text-align:center;background:#1a2f45;color:#c9a84c;border-bottom:2px solid #c9a84c;","Region"),
          th(rowspan=2,style="vertical-align:middle;text-align:center;background:#1a2f45;color:#c9a84c;border-bottom:2px solid #c9a84c;","Subregion"),
          th(rowspan=2,style="vertical-align:middle;text-align:center;background:#1a2f45;color:#c9a84c;border-bottom:2px solid #c9a84c;","Tier"),
          th(rowspan=2,style="vertical-align:middle;text-align:center;background:#1a2f45;color:#c9a84c;border-bottom:2px solid #c9a84c;","RE Score"),
          th(colspan=5,style="text-align:center;background:#0e2133;color:#5dade2;border-bottom:1px solid #1e3a5a;border-left:2px solid #1e3a5a;padding:6px;","Economic Indicators"),
          th(colspan=5,style="text-align:center;background:#0e2133;color:#58d68d;border-bottom:1px solid #1e3a5a;border-left:2px solid #1e3a5a;padding:6px;","Business Friendliness"),
          th(colspan=3,style="text-align:center;background:#0e2133;color:#f39c12;border-bottom:1px solid #1e3a5a;border-left:2px solid #1e3a5a;padding:6px;","Judicial System"),
          th(colspan=5,style="text-align:center;background:#0e2133;color:#e74c3c;border-bottom:1px solid #1e3a5a;border-left:2px solid #1e3a5a;padding:6px;","RE Performance"),
          th(colspan=5,style="text-align:center;background:#0e2133;color:#9b59b6;border-bottom:1px solid #1e3a5a;border-left:2px solid #1e3a5a;padding:6px;","Market Transparency"),
          th(colspan=1,style="text-align:center;background:#0e2133;color:#af7ac5;border-bottom:1px solid #1e3a5a;border-left:2px solid #1e3a5a;padding:6px;","Country Risk")
        ),
        tr(
          th(style="text-align:center;background:#0d1b2a;color:#5dade2;border-left:2px solid #1e3a5a;","HDI"),
          th(style="text-align:center;background:#0d1b2a;color:#5dade2;","GDP ($B)"),
          th(style="text-align:center;background:#0d1b2a;color:#5dade2;","FDI ($B)"),
          th(style="text-align:center;background:#0d1b2a;color:#5dade2;","Unemp."),
          th(style="text-align:center;background:#0d1b2a;color:#5dade2;","Infl."),
          th(style="text-align:center;background:#0d1b2a;color:#58d68d;border-left:2px solid #1e3a5a;","Business"),
          th(style="text-align:center;background:#0d1b2a;color:#58d68d;","Trade"),
          th(style="text-align:center;background:#0d1b2a;color:#58d68d;","Investment"),
          th(style="text-align:center;background:#0d1b2a;color:#58d68d;","Financial"),
          th(style="text-align:center;background:#0d1b2a;color:#58d68d;","Tax Burden"),
          th(style="text-align:center;background:#0d1b2a;color:#f39c12;border-left:2px solid #1e3a5a;","Prop. Rights"),
          th(style="text-align:center;background:#0d1b2a;color:#f39c12;","Corr. Percept."),
          th(style="text-align:center;background:#0d1b2a;color:#f39c12;","Reg. Quality"),
          th(style="text-align:center;background:#0d1b2a;color:#e74c3c;border-left:2px solid #1e3a5a;","10Y TROR"),
          th(style="text-align:center;background:#0d1b2a;color:#e74c3c;","Pr.Yr TROR"),
          th(style="text-align:center;background:#0d1b2a;color:#e74c3c;","Volatility"),
          th(style="text-align:center;background:#0d1b2a;color:#e74c3c;","Drawdown"),
          th(style="text-align:center;background:#0d1b2a;color:#e74c3c;","Cap Rate"),
          th(style="text-align:center;background:#0d1b2a;color:#9b59b6;border-left:2px solid #1e3a5a;","Overall"),
          th(style="text-align:center;background:#0d1b2a;color:#9b59b6;","Invest."),
          th(style="text-align:center;background:#0d1b2a;color:#9b59b6;","Fundamentals"),
          th(style="text-align:center;background:#0d1b2a;color:#9b59b6;","Legal"),
          th(style="text-align:center;background:#0d1b2a;color:#9b59b6;","Sustainability"),
          th(style="text-align:center;background:#0d1b2a;color:#af7ac5;border-left:2px solid #1e3a5a;","Risk Premium Z")
        )
      )
    ))
    
    d0 <- df_filtered()
    if (!"Country_Risk_Z" %in% names(d0)) d0$Country_Risk_Z <- NA_real_
    
    d <- d0 %>%
      select(Country, Region, Region_Specific, Tier, RE_Score,
             HDI, GDP_B, FDI_B, Unemployment, Inflation,
             Business_Freedom, Trade_Freedom, Investment_Freedom, Financial_Freedom, Tax_Burden,
             Property_Rights, Corr_Percep_Score, Reg_Quality,
             `10yr_Average_TROR`, `Prior_Yr_TROR`, `StDev_TROR`, `Worst_Drawdown_TROR`, `Current_Cap_Rate`,
             Mkt_Transparency, Mkt_Trans_Invest, Mkt_Trans_Fund, Mkt_Trans_Legal, Mkt_Trans_Sustain,
             Country_Risk_Z
      ) %>%
      mutate(
        Region_Specific    = ifelse(is.na(Region_Specific) | Region_Specific == "", "N/A", Region_Specific),
        Tier_html          = sapply(Tier, tier_badge_html),
        # Keep RE_Score as-is for formatStyle — rename to display label below
        RE_Score           = round(RE_Score, 1),
        HDI                = round(HDI, 3),
        GDP_B              = round(GDP_B, 0),
        FDI_B              = round(FDI_B, 1),
        Unemployment       = ifelse(is.na(Unemployment),"N/A",paste0(round(Unemployment*100,1),"%")),
        Inflation          = ifelse(is.na(Inflation),   "N/A",paste0(round(Inflation*100,1),   "%")),
        Business_Freedom   = round(Business_Freedom, 1),
        Trade_Freedom      = round(Trade_Freedom, 1),
        Investment_Freedom = round(Investment_Freedom, 1),
        Financial_Freedom  = round(Financial_Freedom, 1),
        Tax_Burden         = round(Tax_Burden, 1),
        Property_Rights    = round(Property_Rights, 1),
        Corr_Percep_Score          = round(Corr_Percep_Score, 1),
        Reg_Quality        = round(Reg_Quality, 2),
        `10yr_Average_TROR`   = ifelse(is.na(`10yr_Average_TROR`),  "N/A",round(`10yr_Average_TROR`,  3)),
        `Prior_Yr_TROR`       = ifelse(is.na(`Prior_Yr_TROR`),      "N/A",round(`Prior_Yr_TROR`,      3)),
        `StDev_TROR`          = ifelse(is.na(`StDev_TROR`),         "N/A",round(`StDev_TROR`,         3)),
        `Worst_Drawdown_TROR` = ifelse(is.na(`Worst_Drawdown_TROR`),"N/A",round(`Worst_Drawdown_TROR`,3)),
        `Current_Cap_Rate`    = ifelse(is.na(`Current_Cap_Rate`),   "N/A",round(`Current_Cap_Rate`,   3)),
        Mkt_Transparency   = round(Mkt_Transparency, 2),
        Mkt_Trans_Invest   = round(Mkt_Trans_Invest, 2),
        Mkt_Trans_Fund     = round(Mkt_Trans_Fund, 2),
        Mkt_Trans_Legal    = round(Mkt_Trans_Legal, 2),
        Mkt_Trans_Sustain  = round(Mkt_Trans_Sustain, 2),
        Country_Risk_Z     = ifelse(is.na(Country_Risk_Z), "N/A", round(Country_Risk_Z, 3))
      ) %>%
      select(-Tier) %>%
      # Tier_html is created at end of mutate(); relocate so column order matches thead (Tier is col 4).
      relocate(Tier_html, .after = Region_Specific) %>%
      # Rename RE_Score AFTER mutate so formatStyle can reference the new name
      rename(
        `Subregion` = Region_Specific, `RE Score` = RE_Score, `Tier` = Tier_html,
        `Corr. Percept.` = Corr_Percep_Score,
        `Risk Premium Z` = Country_Risk_Z
      )
    
    out <- datatable(
      d,
      container  = dt_container,
      colnames   = NULL,
      escape     = FALSE,
      selection  = "multiple",
      class      = "compact hover",
      rownames   = FALSE,
      options    = list(
        pageLength = 15, dom = "tip", autoWidth = FALSE,
        order = list(list(4, "desc")),
        columnDefs = list(
          list(width = "118px", targets = c(0, 2, 4), className = "dt-main-grid-fixed"),
          list(width = "118px", targets = 3, className = "dt-main-grid-tier"),
          list(className="dt-center",targets="_all")
        )
      ),
      callback = JS(
        "var $tbl = $(table.table().node());",
        "if (!$tbl.parent().hasClass('market-grid-x-scroll')) {",
        "  $tbl.wrap('<div class=\"market-grid-x-scroll\"></div>');",
        "}",
        "var $search = $('#main_grid_search');",
        "$search.val(table.search());",
        "$search.off('.mainTableSearch').on('input.mainTableSearch', function() {",
        "  table.search(this.value).draw();",
        "});"
      )
    ) %>%
      formatStyle(
        "RE Score",  # matches the renamed column
        background         = styleColorBar(c(0,100),"#1a5276"),
        backgroundSize     = "100% 70%",
        backgroundRepeat   = "no-repeat",
        backgroundPosition = "center"
      )
    out
  })
  
  observeEvent(input$btn_main_grid_select_all,{
    n <- nrow(df_filtered())
    if(n==0){showNotification("No rows in the current grid.",type="warning",duration=3);return()}
    selectRows(dataTableProxy("main_table"),seq_len(n))
  })
  
  observeEvent(input$btn_main_grid_deselect,{ selectRows(dataTableProxy("main_table"),NULL) })
  
  observeEvent(input$btn_copy_main_grid,{
    sel <- input$main_table_rows_selected
    if(is.null(sel)||length(sel)==0){showNotification("Select one or more rows in the Market Grid first.",type="warning",duration=3);return()}
    rows <- df_filtered()[sort(sel),,drop=FALSE]
    session$sendCustomMessage("copyTsvToClipboard",list(text=df_to_clipboard_tsv(format_main_grid_for_clipboard(rows))))
    showNotification("Copied to clipboard (tab-separated). If your browser blocks this, use Download selected or Download all.",type="message",duration=4)
  })
  
  output$dl_main_grid_csv <- downloadHandler(
    filename=function() paste0("market_grid_selected_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv"),
    content=function(file){
      sel <- input$main_table_rows_selected; req(sel); req(length(sel)>0)
      utils::write.csv(format_main_grid_for_clipboard(df_filtered()[sort(sel),,drop=FALSE]),file,row.names=FALSE,fileEncoding="UTF-8")
    }
  )
  
  output$dl_main_grid_csv_all <- downloadHandler(
    filename=function() paste0("market_grid_all_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv"),
    content=function(file) utils::write.csv(format_main_grid_for_clipboard(df_filtered()),file,row.names=FALSE,fileEncoding="UTF-8")
  )
  
  output$country_panel <- renderUI({
    sel <- input$main_table_rows_selected
    if(is.null(sel)||length(sel)==0) return(div(style="color:#4a6a8a;font-size:13px;padding:20px;","← Select a row to view country details."))
    row <- df_filtered()[min(sel),,drop=FALSE]; cnt <- row$Country
    div(
      div(class="country-header",cnt),
      div(class="country-sub",row$Region," | ",row$Tier),
      hr(class="section-divider"),
      fluidRow(
        column(6,div(class="metric-card",div(class="metric-label","RE Score"),
                     div(class=paste0("metric-value ",if(!is.na(row$RE_Score)&&row$RE_Score>=60)"positive" else if(!is.na(row$RE_Score)&&row$RE_Score<=35)"negative" else "neutral"),fmt_num(row$RE_Score)),
                     div(class="score-bar-wrap",div(class="score-bar-fill",style=paste0("width:",min(100,max(0,row$RE_Score)),"%;"))))),
        column(6,div(class="metric-card",div(class="metric-label","Corruption Perceptions"),div(class="metric-value neutral",fmt_num(row$Corr_Percep_Score))))
      ),
      fluidRow(
        column(6,div(class="metric-card",div(class="metric-label","FDI Inflow"),div(class="metric-value neutral",fmt_b(row$FDI_B)))),
        column(6,div(class="metric-card",div(class="metric-label","GDP (PPP)"),div(class="metric-value neutral",fmt_b(row$GDP_B))))
      ),
      fluidRow(
        column(6,div(class="metric-card",div(class="metric-label","10Y TROR"),div(class="metric-value neutral",fmt_num(row$`10yr_Average_TROR`,3)))),
        column(6,div(class="metric-card",div(class="metric-label","Current Cap Rate"),div(class="metric-value neutral",fmt_num(row$`Current_Cap_Rate`,3))))
      ),
      actionButton(paste0("btn_detail_",gsub(" ","_",cnt)),"View Full Detail →",
                   onclick=sprintf("Shiny.setInputValue('go_detail','%s',{priority:'event'})",cnt),
                   style="width:100%;margin-top:10px;background:transparent;border:1px solid #c9a84c;color:#c9a84c;font-size:11px;border-radius:4px;padding:7px;letter-spacing:.5px;text-transform:uppercase;font-weight:600;")
    )
  })
  
  observeEvent(input$go_detail,{
    updateSelectInput(session,"sel_country_detail",selected=input$go_detail)
    updateTabItems(session,"sidebar","detail")
  })
  
  output$bar_rescore <- renderPlotly({
    d <- df_filtered()%>%filter(!is.na(RE_Score))%>%arrange(desc(RE_Score))%>%head(30)%>%mutate(Country=factor(Country,levels=rev(Country)))
    plot_ly(d,x=~Country,y=~RE_Score,type="bar",color=~Tier,colors=tier_colors,
            text=~paste0(Country,"<br>Score: ",round(RE_Score,1),"<br>Region: ",Region,"<br>",Tier),
            hoverinfo="text",marker=list(line=list(width=0))) %>%
      layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
             font=list(family="IBM Plex Sans",color="#8b9eb0",size=11),
             xaxis=list(showgrid=FALSE,color="#8b9eb0",tickangle=-40,title="",tickfont=list(size=10)),
             yaxis=list(showgrid=TRUE,gridcolor="rgba(30,58,90,.5)",color="#8b9eb0",title="RE Attractiveness Score"),
             legend=list(font=list(color="#8b9eb0",size=10),bgcolor="rgba(0,0,0,0)",orientation="h",x=0,y=1.18,xanchor="left",yanchor="bottom"),
             margin=list(b=90,l=60,r=20,t=55),bargap=0.25) %>% config(displayModeBar=FALSE)
  })
  
  output$pie_tier <- renderPlotly({
    d <- df_filtered()%>%count(Tier)%>%filter(!is.na(Tier))
    plot_ly(d,labels=~Tier,values=~n,type="pie",
            marker=list(colors=unname(tier_colors),line=list(color="#07111c",width=2)),
            textinfo="label+percent",textfont=list(color="#f0f4f8",size=10),hoverinfo="label+value") %>%
      layout(paper_bgcolor="rgba(0,0,0,0)",font=list(family="IBM Plex Sans",color="#8b9eb0"),
             legend=list(font=list(color="#8b9eb0",size=10),bgcolor="rgba(0,0,0,0)",orientation="h",x=0,y=1.18,xanchor="left",yanchor="bottom"),
             margin=list(t=55,b=20,l=20,r=20)) %>% config(displayModeBar=FALSE)
  })
  
  output$factor_map <- renderPlotly({
    df <- df_filtered()
    req(nrow(df) > 0)
    
    selected_bucket <- input$map_bucket_select
    map_df <- build_map_summary(df)
    
    df <- df %>%
      left_join(map_df, by = c("Country", "Region")) %>%
      mutate(
        selected_bucket = selected_bucket,
        selected_comparison = dplyr::case_when(
          selected_bucket == "Economic Indicators" ~ econ_comparison,
          selected_bucket == "Business Friendliness" ~ biz_comparison,
          selected_bucket == "Judicial System" ~ jud_comparison,
          selected_bucket == "RE Performance" ~ re_comparison,
          selected_bucket == "Market Transparency" ~ trans_comparison,
          selected_bucket == "Risk Factor" ~ risk_comparison,
          TRUE ~ NA_character_
        ),
        selected_feature = dplyr::case_when(
          selected_bucket == "Economic Indicators" ~ econ_feature,
          selected_bucket == "Business Friendliness" ~ biz_feature,
          selected_bucket == "Judicial System" ~ jud_feature,
          selected_bucket == "RE Performance" ~ re_feature,
          selected_bucket == "Market Transparency" ~ trans_feature,
          selected_bucket == "Risk Factor" ~ risk_feature,
          TRUE ~ NA_character_
        ),
        selected_value = dplyr::case_when(
          selected_bucket == "Economic Indicators" ~ econ_value,
          selected_bucket == "Business Friendliness" ~ biz_value,
          selected_bucket == "Judicial System" ~ jud_value,
          selected_bucket == "RE Performance" ~ re_value,
          selected_bucket == "Market Transparency" ~ trans_value,
          selected_bucket == "Risk Factor" ~ risk_value,
          TRUE ~ NA_real_
        ),
        selected_avg = dplyr::case_when(
          selected_bucket == "Economic Indicators" ~ econ_avg,
          selected_bucket == "Business Friendliness" ~ biz_avg,
          selected_bucket == "Judicial System" ~ jud_avg,
          selected_bucket == "RE Performance" ~ re_avg,
          selected_bucket == "Market Transparency" ~ trans_avg,
          selected_bucket == "Risk Factor" ~ risk_avg,
          TRUE ~ NA_real_
        ),
        comparison_num = dplyr::case_when(
          selected_comparison == "Low" ~ 1,
          selected_comparison == "Average" ~ 2,
          selected_comparison == "High" ~ 3,
          TRUE ~ NA_real_
        ),
        hover_text = paste0(
          "<b>Country:</b> ", Country,
          "<br><b>Region:</b> ", Region,
          "<br><b>Current Color Bucket:</b> ", selected_bucket,
          "<br><b>Selected Factor:</b> ", selected_feature,
          "<br><b>Selected Value:</b> ", ifelse(is.na(selected_value), "NA", round(selected_value, 3)),
          "<br><b>Selected Global Avg:</b> ", ifelse(is.na(selected_avg), "NA", round(selected_avg, 3)),
          "<br><b>Selected Status:</b> ", ifelse(is.na(selected_comparison), "NA", selected_comparison),
          
          "<br><br><b>Economic Indicators</b> — ", econ_feature,
          "<br>Value: ", ifelse(is.na(econ_value), "NA", round(econ_value, 2)),
          "<br>Global Avg: ", ifelse(is.na(econ_avg), "NA", round(econ_avg, 2)),
          "<br>Status: ", ifelse(is.na(econ_comparison), "NA", econ_comparison),
          
          "<br><br><b>Business Friendliness</b> — ", biz_feature,
          "<br>Value: ", ifelse(is.na(biz_value), "NA", round(biz_value, 2)),
          "<br>Global Avg: ", ifelse(is.na(biz_avg), "NA", round(biz_avg, 2)),
          "<br>Status: ", ifelse(is.na(biz_comparison), "NA", biz_comparison),
          
          "<br><br><b>Judicial System</b> — ", jud_feature,
          "<br>Value: ", ifelse(is.na(jud_value), "NA", round(jud_value, 2)),
          "<br>Global Avg: ", ifelse(is.na(jud_avg), "NA", round(jud_avg, 2)),
          "<br>Status: ", ifelse(is.na(jud_comparison), "NA", jud_comparison),
          
          "<br><br><b>RE Performance</b> — ", re_feature,
          "<br>Value: ", ifelse(is.na(re_value), "NA", round(re_value, 2)),
          "<br>Global Avg: ", ifelse(is.na(re_avg), "NA", round(re_avg, 2)),
          "<br>Status: ", ifelse(is.na(re_comparison), "NA", re_comparison),
          
          "<br><br><b>Market Transparency</b> — ", trans_feature,
          "<br>Value: ", ifelse(is.na(trans_value), "NA", round(trans_value, 2)),
          "<br>Global Avg: ", ifelse(is.na(trans_avg), "NA", round(trans_avg, 2)),
          "<br>Status: ", ifelse(is.na(trans_comparison), "NA", trans_comparison),
          
          "<br><br><b>Risk Factor</b> — ", risk_feature,
          "<br>Value: ", ifelse(is.na(risk_value), "NA", round(risk_value, 2)),
          "<br>Global Avg: ", ifelse(is.na(risk_avg), "NA", round(risk_avg, 2)),
          "<br>Status: ", ifelse(is.na(risk_comparison), "NA", risk_comparison)
        )
      )
    
    plot_ly(
      data = df,
      type = "choropleth",
      locations = ~Country,
      locationmode = "country names",
      z = ~comparison_num,
      text = ~hover_text,
      hoverinfo = "text",
      colorscale = list(
        c(0.0, "#e74c3c"),
        c(0.5, "#f1c40f"),
        c(1.0, "#2ecc71")
      ),
      zmin = 1,
      zmax = 3,
      marker = list(line = list(color = "white", width = 0.4)),
      colorbar = list(
        title = paste0(selected_bucket, "<br>Status"),
        tickvals = c(1, 2, 3),
        ticktext = c("Low", "Average", "High")
      )
    ) %>%
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = "natural earth"),
          bgcolor = "rgba(0,0,0,0)"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#f0f4f8")
      )
  })
  
  detail_country_subtitle <- function(label) {
    renderUI({
      cnt <- input$sel_country_detail
      sub <- if (!is.null(cnt) && nzchar(as.character(cnt))) {
        tags$div(style = "margin-top:4px;font-size:11px;font-weight:600;color:#c9a84c;", cnt)
      } else {
        NULL
      }
      tags$div(style = "line-height:1.35;", tags$span(style = "display:block;", label), sub)
    })
  }
  output$detail_box_title_macro        <- detail_country_subtitle("Macro Indicators")
  output$detail_box_title_freedom      <- detail_country_subtitle("Economic Freedom")
  output$detail_box_title_transparency <- detail_country_subtitle("Market Transparency")
  
  output$detail_macro <- renderUI({
    cnt <- input$sel_country_detail
    req(cnt)
    row <- df_scored()%>%filter(Country==cnt)%>%slice(1)
    if(nrow(row)==0) return(div("Country not found"))
    mk <- function(label,value,cls="") div(class="metric-card",div(class="metric-label",label),div(class=paste("metric-value",cls),value))
    div(
      mk("Human Development Index",fmt_num(row$HDI,3),"neutral"),
      mk("Life Expectancy",paste0(fmt_num(row$Life_Expectancy,1)," yrs")),
      mk("Population",paste0(fmt_num(row$Pop_M,1),"M"),"neutral"),
      mk("GDP (PPP)",fmt_b(row$GDP_B),"neutral"),
      mk("Unemployment",fmt_pct(row$Unemployment),if(!is.na(row$Unemployment)&&row$Unemployment>0.08)"negative" else "positive"),
      mk("Inflation",fmt_pct(row$Inflation),if(!is.na(row$Inflation)&&row$Inflation>0.05)"negative" else "positive"),
      mk("Public Debt / GDP",fmt_pct(row$Public_Debt_Ratio))
    )
  })
  
  output$detail_freedom <- renderUI({
    cnt <- input$sel_country_detail
    req(cnt)
    row <- df_scored()%>%filter(Country==cnt)%>%slice(1)
    if(nrow(row)==0) return(div("Country not found"))
    score_row <- function(label, val) {
      div(class="metric-card",
          div(class="metric-label", label),
          div(class="metric-value neutral", fmt_num(val, 1)))
    }
    div(score_row("Business Freedom",row$Business_Freedom),score_row("Tax Burden",row$Tax_Burden),
        score_row("Trade Freedom",row$Trade_Freedom),score_row("Investment Freedom",row$Investment_Freedom),
        score_row("Financial Freedom",row$Financial_Freedom),score_row("Property Rights",row$Property_Rights))
  })
  
  output$detail_transparency <- renderUI({
    cnt <- input$sel_country_detail
    req(cnt)
    row <- df_scored()%>%filter(Country==cnt)%>%slice(1)
    if(nrow(row)==0) return(div("Country not found"))
    sr <- function(label,val) div(class="metric-card",div(class="metric-label",label),div(class="metric-value neutral",fmt_num(val,2)))
    div(sr("Overall Transparency",row$Mkt_Transparency),sr("Investment Performance",row$Mkt_Trans_Invest),
        sr("Market Fundamentals",row$Mkt_Trans_Fund),sr("Regulatory & Legal",row$Mkt_Trans_Legal),
        sr("Sustainability",row$Mkt_Trans_Sustain),sr("10Y TROR",row$`10yr_Average_TROR`),sr("Current Cap Rate",row$`Current_Cap_Rate`))
  })
  
  output$radar_chart <- renderPlotly({
    cnt <- input$sel_country_detail
    req(cnt)
    row <- df_scored()%>%filter(Country==cnt)%>%slice(1)
    if(nrow(row)==0) return(NULL)
    cats <- c("Invest. Perf.","Market Fund.","Regulatory","Sustainability","Transparency")
    vals <- as.numeric(c(row$Mkt_Trans_Invest,row$Mkt_Trans_Fund,row$Mkt_Trans_Legal,row$Mkt_Trans_Sustain,row$Mkt_Transparency))
    plot_ly(type="scatterpolar",mode="lines+markers",r=c(vals,vals[1]),theta=c(cats,cats[1]),fill="toself",
            fillcolor="rgba(201,168,76,.15)",line=list(color="#c9a84c",width=2),marker=list(color="#c9a84c",size=5)) %>%
      layout(paper_bgcolor="rgba(0,0,0,0)",
             polar=list(bgcolor="rgba(15,30,45,.5)",
                        angularaxis=list(color="#8b9eb0",tickfont=list(size=10,color="#8b9eb0")),
                        radialaxis=list(color="#4a6a8a",gridcolor="rgba(30,58,90,.7)",tickfont=list(size=9,color="#4a6a8a"))),
             margin=list(t=20,b=20,l=50,r=50)) %>% config(displayModeBar=FALSE)
  })
  
  output$freedom_bar <- renderPlotly({
    cnt <- input$sel_country_detail
    req(cnt)
    row <- df_scored()%>%filter(Country==cnt)%>%slice(1)
    if(nrow(row)==0) return(NULL)
    reg_avg <- df_scored()%>%filter(Region==row$Region)%>%summarise(across(c(Business_Freedom,Tax_Burden,Trade_Freedom,Investment_Freedom,Financial_Freedom),~mean(.x,na.rm=TRUE)))
    metrics <- c("Business","Tax Burden","Trade","Investment","Financial")
    cv <- as.numeric(c(row$Business_Freedom,row$Tax_Burden,row$Trade_Freedom,row$Investment_Freedom,row$Financial_Freedom))
    rv <- as.numeric(c(reg_avg$Business_Freedom,reg_avg$Tax_Burden,reg_avg$Trade_Freedom,reg_avg$Investment_Freedom,reg_avg$Financial_Freedom))
    plot_ly()%>%
      add_bars(x=metrics,y=cv,name=cnt,marker=list(color="#c9a84c",line=list(color="#0d1b2a",width=1)))%>%
      add_bars(x=metrics,y=rv,name=paste(row$Region,"Avg"),marker=list(color="rgba(93,173,226,.5)",line=list(color="#0d1b2a",width=1)))%>%
      layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
             font=list(family="IBM Plex Sans",color="#8b9eb0",size=11),barmode="group",bargap=0.3,
             xaxis=list(showgrid=FALSE,color="#8b9eb0",title=""),
             yaxis=list(showgrid=TRUE,gridcolor="rgba(30,58,90,.5)",color="#8b9eb0",title="Score (0-100)",range=c(0,110)),
             legend=list(font=list(color="#8b9eb0",size=10),bgcolor="rgba(0,0,0,0)",orientation="h",x=0,y=1.18,xanchor="left",yanchor="bottom"),
             margin=list(t=55,b=40,l=50,r=20)) %>% config(displayModeBar=FALSE)
  })
  
  country_detail_selected_row <- reactive({
    req(input$sel_country_detail)
    df_scored()%>%filter(Country==input$sel_country_detail)%>%slice(1)
  })
  
  country_detail_b_selected_row <- reactive({
    b <- input$krish_sel_country_compare
    if (is.null(b) || identical(b, "None") || !nzchar(as.character(b))) {
      return(df_scored()[0, , drop = FALSE])
    }
    df_scored() %>% filter(Country == b) %>% slice(1)
  })
  
  observeEvent(input$btn_country_detail_copy,{
    row <- country_detail_selected_row()
    if(nrow(row)==0){showNotification("Select a valid country first.",type="warning",duration=3);return()}
    session$sendCustomMessage("copyTsvToClipboard",list(text=df_to_clipboard_tsv(build_country_detail_export(row))))
    showNotification(paste("Copied",input$sel_country_detail,"to clipboard (tab-separated)."),type="message",duration=3)
  })
  
  output$dl_country_detail_csv <- downloadHandler(
    filename=function(){safe<-gsub("[^A-Za-z0-9._-]+","_",input$sel_country_detail);paste0("country_detail_",safe,"_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv")},
    content=function(file){row<-country_detail_selected_row();req(nrow(row)>0);utils::write.csv(build_country_detail_export(row),file,row.names=FALSE,fileEncoding="UTF-8")}
  )
  
  observeEvent(input$btn_country_detail_b_copy,{
    row <- country_detail_b_selected_row()
    if (nrow(row) == 0) {
      showNotification("Select Country B in the comparison row below (not “None”).", type = "warning", duration = 4)
      return()
    }
    session$sendCustomMessage("copyTsvToClipboard", list(text = df_to_clipboard_tsv(build_country_detail_export(row))))
    showNotification(paste("Copied", row$Country[[1]], "to clipboard (tab-separated)."), type = "message", duration = 3)
  })
  
  output$dl_country_detail_b_csv <- downloadHandler(
    filename = function() {
      row <- country_detail_b_selected_row()
      nm <- if (nrow(row) > 0) as.character(row$Country[[1]]) else "country_B"
      safe <- gsub("[^A-Za-z0-9._-]+", "_", nm)
      paste0("country_detail_B_", safe, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      row <- country_detail_b_selected_row()
      req(nrow(row) > 0)
      utils::write.csv(build_country_detail_export(row), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  ai_text <- reactiveVal(div(style="color:#4a6a8a;font-size:12px;","Click ✦ Generate Gemini Country Summary to create a real-estate-investor summary for the selected country."))
  output$ai_insight_output <- renderUI({ ai_text() })
  
  observeEvent(input$btn_ai,{
    cnt <- input$sel_country_detail
    row <- df_scored()%>%filter(Country==cnt)%>%slice(1)
    if(nrow(row)==0) return()
    ai_text(div(class="ai-loading","⟳  Generating insight for ",cnt,"…"))
    gemini_res <- call_gemini_summary(cnt,row)
    if(isTRUE(gemini_res$ok)) {
      ai_text(HTML(gemini_res$text))
    } else {
      ai_text(HTML(paste0(get_fallback_ai_insight(cnt,row),"<div style='margin-top:12px;color:#8b9eb0;font-size:11px;'>Gemini fallback used because: ",htmltools::htmlEscape(gemini_res$text),"</div>")))
    }
  })
  
  output$scatter_compare <- renderPlotly({
    d <- df_filtered()%>%filter(!is.na(.data[[input$compare_x]]),!is.na(.data[[input$compare_y]]))
    plot_ly(d,x=~.data[[input$compare_x]],y=~.data[[input$compare_y]],type="scatter",mode="markers",
            color=~Tier,colors=tier_colors,size=~pmax(RE_Score,10),sizes=c(8,35),
            text=~paste0("<b>",Country,"</b><br>",input$compare_x,": ",round(.data[[input$compare_x]],2),"<br>",input$compare_y,": ",round(.data[[input$compare_y]],2),"<br>Region: ",Region),
            hoverinfo="text",marker=list(line=list(color="#07111c",width=0.5),opacity=0.85)) %>%
      layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
             font=list(family="IBM Plex Sans",color="#8b9eb0",size=11),
             xaxis=list(showgrid=TRUE,gridcolor="rgba(30,58,90,.5)",color="#8b9eb0",title=input$compare_x),
             yaxis=list(showgrid=TRUE,gridcolor="rgba(30,58,90,.5)",color="#8b9eb0",title=input$compare_y),
             legend=list(font=list(color="#8b9eb0",size=10),bgcolor="rgba(0,0,0,0)",orientation="h",x=0,y=1.18,xanchor="left",yanchor="bottom"),
             margin=list(t=55,b=60,l=70,r=20)) %>% config(displayModeBar=FALSE)
  })
  
  output$top15_bar <- renderPlotly({
    d <- df_filtered()%>%filter(!is.na(RE_Score))%>%arrange(desc(RE_Score))%>%head(15)%>%mutate(Country=factor(Country,levels=rev(Country)))
    plot_ly(d,x=~RE_Score,y=~Country,type="bar",orientation="h",
            marker=list(color=~RE_Score,colorscale=list(c(0,"#1a5276"),c(.5,"#2e86c1"),c(1,"#c9a84c")),line=list(width=0)),
            text=~paste0(round(RE_Score,1)),textposition="outside",hoverinfo="y+text") %>%
      layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
             font=list(family="IBM Plex Sans",color="#8b9eb0",size=11),
             xaxis=list(showgrid=TRUE,gridcolor="rgba(30,58,90,.5)",color="#8b9eb0",title="RE Score",range=c(0,115)),
             yaxis=list(showgrid=FALSE,color="#8b9eb0",title=""),
             margin=list(t=20,b=40,l=20,r=50),bargap=0.3) %>% config(displayModeBar=FALSE)
  })
  
  render_news_cards <- function(data_list,empty_msg){
    if(is.null(data_list)||nrow(data_list)==0) return(div(style="padding:10px;color:#888;",empty_msg))
    lapply(seq_len(nrow(data_list)),function(i){
      row<-data_list[i,]
      div(class="box",style="border-top:1px solid #555;background-color:#2b2b2b;margin-bottom:15px;padding:10px;",
          div(style="font-size:1.1em;font-weight:bold;color:#E0E0E0;",icon("newspaper"),row$headline),
          div(style="font-size:.9em;color:#999;",paste(row$date,"|",row$source)),
          div(style="color:#CCC;",row$summary),
          a(href=row$url,target="_blank","Read Source",style="color:#4285F4;"))
    })
  }
  
  output$news_pulse_ui   <- renderUI({ render_news_cards(news_pulse_data(),  "No pulse news found.") })
  output$news_warning_ui <- renderUI({ render_news_cards(news_warning_data(),"No warning signals found.") })
  output$news_summary    <- renderUI({ news_summary_rv() })
  
  observeEvent(input$btn_news,{
    target <- input$news_market_select
    if(is.null(target)||identical(target,"")) return()
    ai_brief_running(TRUE); on.exit(ai_brief_running(FALSE), add = TRUE)
    news_summary_rv(div(class="ai-loading","⟳ Fetching Google News and generating Gemini summary..."))
    tryCatch({
      d_news <- fetch_google_news("Country",target,max_items=50)
      news_data(d_news)
      gem <- summarize_news_with_gemini("Country",target,d_news)
      news_summary_rv(HTML(gem$summary_html))
    }, error=function(e){
      news_data(data.frame())
      news_summary_rv(div(class="ai-insight-error",paste0("News fetch failed: ",conditionMessage(e))))
    })
  })
  
} # end base server

# =============================================================================
# KRISH COMPARISON HELPERS
# =============================================================================

krish_build_comparison_chart <- function(labels,a_vals,b_vals,a_name,b_name,y_title="",pct_fmt=FALSE,digits=1){
  labels_f <- factor(labels,levels=rev(labels))
  fmt_val  <- function(v) ifelse(is.na(v),"N/A",if(pct_fmt)paste0(round(v*100,digits),"%") else as.character(round(v,digits)))
  a_text   <- sapply(a_vals,fmt_val); b_text <- sapply(b_vals,fmt_val)
  a_plot   <- ifelse(is.na(a_vals),0,a_vals); b_plot <- ifelse(is.na(b_vals),0,b_vals)
  if(pct_fmt){a_plot<-a_plot*100;b_plot<-b_plot*100}
  plot_ly()%>%
    add_bars(y=labels_f,x=a_plot,name=a_name,orientation="h",marker=list(color="#c9a84c",line=list(color="#0d1b2a",width=.5)),text=a_text,textposition="outside",hovertemplate=paste0(a_name,": %{text}<extra></extra>"))%>%
    add_bars(y=labels_f,x=b_plot,name=b_name,orientation="h",marker=list(color="#5dade2",line=list(color="#0d1b2a",width=.5)),text=b_text,textposition="outside",hovertemplate=paste0(b_name,": %{text}<extra></extra>"))%>%
    layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
           font=list(family="IBM Plex Sans",color="#8b9eb0",size=11),barmode="group",bargap=0.3,
           xaxis=list(showgrid=TRUE,gridcolor="rgba(30,58,90,.5)",color="#8b9eb0",title=y_title,zeroline=FALSE,tickfont=list(size=10)),
           yaxis=list(showgrid=FALSE,color="#c4d3df",title="",tickfont=list(size=11,color="#c4d3df")),
           legend=list(font=list(color="#c4d3df",size=11),bgcolor="rgba(0,0,0,0)",orientation="h",x=0,y=1.18,xanchor="left",yanchor="bottom"),
           margin=list(t=55,b=40,l=10,r=60)) %>% config(displayModeBar=FALSE)
}

krish_build_comparison_chart_single <- function(labels,a_vals,a_name,y_title="",pct_fmt=FALSE,digits=1){
  labels_f <- factor(labels,levels=rev(labels))
  fmt_val  <- function(v) ifelse(is.na(v),"N/A",if(pct_fmt)paste0(round(v*100,digits),"%") else as.character(round(v,digits)))
  a_text   <- sapply(a_vals,fmt_val)
  a_plot   <- ifelse(is.na(a_vals),0,a_vals); if(pct_fmt) a_plot<-a_plot*100
  plot_ly()%>%
    add_bars(y=labels_f,x=a_plot,name=a_name,orientation="h",marker=list(color="#c9a84c",line=list(color="#0d1b2a",width=.5)),text=a_text,textposition="outside",hovertemplate=paste0(a_name,": %{text}<extra></extra>"))%>%
    layout(paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
           font=list(family="IBM Plex Sans",color="#8b9eb0",size=11),bargap=0.3,
           xaxis=list(showgrid=TRUE,gridcolor="rgba(30,58,90,.5)",color="#8b9eb0",title=y_title,zeroline=FALSE,tickfont=list(size=10)),
           yaxis=list(showgrid=FALSE,color="#c4d3df",title="",tickfont=list(size=11,color="#c4d3df")),
           legend=list(font=list(color="#c4d3df",size=11),bgcolor="rgba(0,0,0,0)",orientation="h",x=0,y=1.18,xanchor="left",yanchor="bottom"),
           margin=list(t=55,b=40,l=10,r=60)) %>% config(displayModeBar=FALSE)
}

# Build a normalized-score comparison chart for a bucket of sub-factors.
# Bars are on a 0-100 normalized scale (same logic as compute_re_score);
# bar labels show the raw value with appropriate units/formatting.
krish_plot_bucket_normalized <- function(sf_ids, r_a, r_b, df_src) {
  sfs <- lapply(sf_ids, function(id) {
    for (bkt in BUCKETS) for (sf in bkt$subfactors) if (sf$id == id) return(sf)
    NULL
  })
  labels <- vapply(sfs, function(sf) sf$name, character(1))
  
  ranges <- lapply(sfs, function(sf) {
    if (!is.null(sf$scale)) return(sf$scale)
    vals <- suppressWarnings(as.numeric(df_src[[sf$col]]))
    lo <- min(vals, na.rm = TRUE); hi <- max(vals, na.rm = TRUE)
    if (!is.finite(lo) || !is.finite(hi) || lo == hi) return(c(0, 1))
    c(lo, hi)
  })
  
  raw_of <- function(row, sf) {
    if (nrow(row) == 0) return(NA_real_)
    v <- row[[sf$col]]; if (is.null(v) || length(v) == 0) NA_real_ else suppressWarnings(as.numeric(v))
  }
  
  fmt_raw <- function(val, sf) {
    if (is.na(val)) return("N/A")
    if (sf$col %in% c("Unemployment", "Inflation")) return(paste0(round(val * 100, 1), "%"))
    if (sf$col %in% c("GDP_B", "FDI_B"))           return(paste0("$", format(round(val, 0), big.mark = ",", scientific = FALSE), "B"))
    if (identical(sf$col, "HDI"))                   return(formatC(val, digits = 3, format = "f"))
    if (sf$col %in% c("Reg_Quality", "Country_Risk_Z")) return(formatC(val, digits = 2, format = "f"))
    if (grepl("TROR|Cap_Rate|Drawdown|StDev", sf$col))  return(formatC(val, digits = 3, format = "f"))
    formatC(val, digits = 1, format = "f")
  }
  
  raw_a <- vapply(sfs, function(sf) raw_of(r_a, sf), numeric(1))
  raw_b <- vapply(sfs, function(sf) raw_of(r_b, sf), numeric(1))
  
  norm_a <- vapply(seq_along(sfs), function(i) {
    normalize_to_100(raw_a[i], ranges[[i]][1], ranges[[i]][2], sfs[[i]]$higher_better)
  }, numeric(1))
  norm_b <- vapply(seq_along(sfs), function(i) {
    normalize_to_100(raw_b[i], ranges[[i]][1], ranges[[i]][2], sfs[[i]]$higher_better)
  }, numeric(1))
  
  a_text <- mapply(fmt_raw, raw_a, sfs, USE.NAMES = FALSE)
  b_text <- mapply(fmt_raw, raw_b, sfs, USE.NAMES = FALSE)
  
  labels_f <- factor(labels, levels = rev(labels))
  a_plot <- ifelse(is.na(norm_a), 0, norm_a)
  b_plot <- ifelse(is.na(norm_b), 0, norm_b)
  
  a_name <- if (nrow(r_a) > 0) r_a$Country[[1]] else "Country A"
  has_b  <- nrow(r_b) > 0
  b_name <- if (has_b) r_b$Country[[1]] else NULL
  
  p <- plot_ly() %>%
    add_bars(y = labels_f, x = a_plot, name = a_name, orientation = "h",
             showlegend = FALSE,
             marker = list(color = "#c9a84c", line = list(color = "#0d1b2a", width = 0.5)),
             text = a_text, textposition = "outside",
             hovertemplate = paste0(a_name, ": %{text} (score %{x:.1f})<extra></extra>"))
  if (has_b) {
    p <- p %>% add_bars(y = labels_f, x = b_plot, name = b_name, orientation = "h",
                        showlegend = FALSE,
                        marker = list(color = "#5dade2", line = list(color = "#0d1b2a", width = 0.5)),
                        text = b_text, textposition = "outside",
                        hovertemplate = paste0(b_name, ": %{text} (score %{x:.1f})<extra></extra>"))
  }
  p %>% layout(
    paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
    showlegend = FALSE,
    font = list(family = "IBM Plex Sans", color = "#8b9eb0", size = 11),
    barmode = "group", bargap = 0.3,
    xaxis = list(
      showgrid = TRUE, gridcolor = "rgba(30,58,90,.5)", color = "#8b9eb0",
      title = "Normalized Score (0-100)", range = c(-5, 115), tickfont = list(size = 10),
      zeroline = FALSE, tickmode = "array", tickvals = c(0, 25, 50, 75, 100)
    ),
    yaxis = list(showgrid = FALSE, color = "#c4d3df", title = "",
                 tickfont = list(size = 11, color = "#c4d3df"),
                 ticklabelstandoff = 8),
    margin = list(t = 8, b = 40, l = 18, r = 60)
  ) %>% config(displayModeBar = FALSE)
}

# Compute the six bucket scores (0-100) for a single country row, using the
# current user weights. Mirrors the per-bucket subtotal logic inside compute_re_score.
compute_bucket_scores_for_row <- function(row, df_src, weights) {
  bkt_names <- vapply(BUCKETS, function(b) b$name, character(1))
  if (nrow(row) == 0) return(setNames(rep(NA_real_, length(BUCKETS)), bkt_names))
  
  global_ranges <- list()
  for (bkt in BUCKETS) {
    for (sf in bkt$subfactors) {
      if (is.null(sf$scale)) {
        vals <- suppressWarnings(as.numeric(df_src[[sf$col]]))
        lo <- min(vals, na.rm = TRUE); hi <- max(vals, na.rm = TRUE)
        if (!is.finite(lo) || !is.finite(hi) || lo == hi) { lo <- 0; hi <- 1 }
        global_ranges[[sf$id]] <- c(lo, hi)
      }
    }
  }
  
  bkt_scores <- setNames(rep(NA_real_, length(BUCKETS)), bkt_names)
  for (k in seq_along(BUCKETS)) {
    bkt <- BUCKETS[[k]]
    bkt_score <- 0; bkt_w_used <- 0
    for (sf in bkt$subfactors) {
      sf_w <- weights$sf_weights[[bkt$id]][sf$id]
      if (is.na(sf_w) || sf_w <= 0) next
      raw_val <- suppressWarnings(as.numeric(row[[sf$col]]))
      if (length(raw_val) == 0 || is.na(raw_val)) next
      norm_val <- if (!is.null(sf$scale)) {
        normalize_to_100(raw_val, sf$scale[1], sf$scale[2], sf$higher_better)
      } else {
        rng <- global_ranges[[sf$id]]
        normalize_to_100(raw_val, rng[1], rng[2], sf$higher_better)
      }
      if (!is.na(norm_val)) {
        bkt_score  <- bkt_score  + sf_w * norm_val
        bkt_w_used <- bkt_w_used + sf_w
      }
    }
    bkt_scores[k] <- if (bkt_w_used > 0) bkt_score / bkt_w_used else NA_real_
  }
  bkt_scores
}

# =============================================================================
# SERVER — KRISH COMPARISON LAYER (wraps base server)
# =============================================================================

.base_server <- server
server <- function(input, output, session) {
  .base_server(input, output, session)
  
  session$onFlushed(function() {
    insertUI(selector="head",where="beforeEnd",ui=tags$script(HTML("
      if(!window.__krishHandlerRegistered){
        window.__krishHandlerRegistered=true;
        window.__krishPatchDetailUi=function(){
          function removeEnclosingBox(id){
            var el=document.getElementById(id); if(!el)return;
            var box=el.closest('.box');
            var col=box?box.closest('.col-sm-6,.col-sm-4,.col-sm-8,[class*=col-sm-]'):null;
            var tgt=col||box;
            if(tgt&&tgt.parentNode)tgt.parentNode.removeChild(tgt);
          }
          removeEnclosingBox('radar_chart'); removeEnclosingBox('freedom_bar');
          var selA=document.getElementById('sel_country_detail');
          var countryA='Country A';
          if(selA){var t=(selA.value||'').trim();if(!t&&selA.selectedIndex>=0){var opt=selA.options[selA.selectedIndex];if(opt)t=(opt.text||'').trim();}if(t)countryA=t;}
          if(selA){var lc=selA.closest('.shiny-input-container')||selA.parentElement;if(lc){var lb=lc.querySelector('label');if(lb)lb.textContent='Country A';}}
        };
        Shiny.addCustomMessageHandler('krishPatchDetailUi',function(msg){window.__krishPatchDetailUi();});
        var pending=false;
        function runPatchSoon(){if(pending)return;pending=true;setTimeout(function(){pending=false;try{window.__krishPatchDetailUi();}catch(e){}},50);}
        function attachObserver(){var root=document.body;if(!root){setTimeout(attachObserver,100);return;}new MutationObserver(function(){runPatchSoon();}).observe(root,{childList:true,subtree:true});runPatchSoon();}
        if(document.readyState==='loading')document.addEventListener('DOMContentLoaded',attachObserver);else attachObserver();
        if(!window.__krishLegendToggleBound){
          window.__krishLegendToggleBound=true;
          document.body.addEventListener('click',function(ev){
            var el=ev.target.closest('.krish-legend-item'); if(!el)return;
            var pid=el.getAttribute('data-krish-plot'); var tix=parseInt(el.getAttribute('data-krish-trace'),10);
            if(!pid||isNaN(tix))return;
            ev.preventDefault(); ev.stopPropagation();
            var gd=document.getElementById(pid);
            if(!gd||typeof Plotly==='undefined'||!gd.data||!gd.data[tix])return;
            var cur=gd.data[tix].visible;
            var show=(cur===false);
            Plotly.restyle(gd,{visible:show},[tix]);
          },true);
        }
      }
    ")),immediate=TRUE)
    
    session$sendCustomMessage("krishPatchDetailUi",list())
    
    insertUI(selector=".detail-gemini-insight-wrap",where="beforeBegin",
             ui=tags$div(id="krish_compare_wrap",style="margin-top:0;margin-bottom:8px;",
                         fluidRow(
                           box(title=uiOutput("krish_title_radar"),       width=12,status="primary",withSpinner(plotlyOutput("krish_radar_chart",    height="480px"),type=4,color="#c9a84c",size=.7))
                         ),
                         fluidRow(
                           box(title=uiOutput("krish_title_econ"),        width=4,status="primary",uiOutput("krish_legend_econ"),        withSpinner(plotlyOutput("krish_comp_econ",        height="300px"),type=4,color="#c9a84c",size=.7)),
                           box(title=uiOutput("krish_title_freedom"),     width=4,status="primary",uiOutput("krish_legend_freedom"),     withSpinner(plotlyOutput("krish_comp_freedom",     height="300px"),type=4,color="#c9a84c",size=.7)),
                           box(title=uiOutput("krish_title_judicial"),    width=4,status="primary",uiOutput("krish_legend_judicial"),    withSpinner(plotlyOutput("krish_comp_judicial",    height="300px"),type=4,color="#c9a84c",size=.7))
                         ),
                         fluidRow(
                           box(title=uiOutput("krish_title_reperf"),      width=4,status="primary",uiOutput("krish_legend_reperf"),      withSpinner(plotlyOutput("krish_comp_reperf",      height="300px"),type=4,color="#c9a84c",size=.7)),
                           box(title=uiOutput("krish_title_transparency"),width=4,status="primary",uiOutput("krish_legend_transparency"),withSpinner(plotlyOutput("krish_comp_transparency",height="300px"),type=4,color="#c9a84c",size=.7)),
                           box(title=uiOutput("krish_title_risk"),        width=4,status="primary",uiOutput("krish_legend_risk"),        withSpinner(plotlyOutput("krish_comp_risk",        height="300px"),type=4,color="#c9a84c",size=.7))
                         ),
                         fluidRow(
                           box(title=uiOutput("krish_title_freedom_bar"), width=12,status="primary",uiOutput("krish_legend_freedom_bar"),withSpinner(plotlyOutput("krish_freedom_bar",    height="340px"),type=4,color="#c9a84c",size=.7))
                         )
             ),immediate=TRUE)
    
    session$sendCustomMessage("krishPatchDetailUi",list())
  })
  
  observeEvent(input$sel_country_detail,{ session$sendCustomMessage("krishPatchDetailUi",list()) },ignoreNULL=TRUE,ignoreInit=FALSE)
  
  # ── build_model_weights reads input directly — no cross-scope reference
  krish_model_weights <- reactive({ build_model_weights(input) })
  krish_df_scored     <- reactive({ compute_re_score(df_all_raw, krish_model_weights()) })
  
  output$krish_country_compare_ui <- renderUI({
    countries <- sort(unique(na.omit(krish_df_scored()$Country)))
    default_b <- if("United Kingdom"%in%countries)"United Kingdom" else {pool<-setdiff(countries,"United States");if(length(pool)>0)pool[[1]] else countries[[1]]}
    selectInput("krish_sel_country_compare","Country B",choices=c("None",countries),selected=default_b,width="100%")
  })
  
  krish_sel_row <- reactive({
    req(input$sel_country_detail)
    krish_df_scored()%>%filter(Country==input$sel_country_detail)%>%slice(1)
  })
  
  krish_cmp_row <- reactive({
    req(input$krish_sel_country_compare)
    if(identical(input$krish_sel_country_compare,"None")) return(krish_df_scored()[0,,drop=FALSE])
    krish_df_scored()%>%filter(Country==input$krish_sel_country_compare)%>%slice(1)
  })
  
  output$krish_detail_header_info <- renderUI({
    r<-krish_sel_row(); c_<-krish_cmp_row()
    if(nrow(r)==0) return(NULL)
    has_b<-nrow(c_)>0
    tags$div(style="margin:0;margin-top:10px;text-align:left;",
             tags$div(style="font-family:'Playfair Display',serif;font-size:18px;color:#f0f4f8;font-weight:600;",
                      tags$span(style="color:#c9a84c;",r$Country),
                      if(has_b)tags$span(style="color:#8b9eb0;font-size:13px;"," vs "),
                      if(has_b)tags$span(style="color:#5dade2;",c_$Country)),
             tags$div(style="margin-top:2px;",
                      tags$span(style="color:#c9a84c;font-size:11px;",paste0("A: RE ",round(r$RE_Score,1))),
                      if(has_b)tags$span(style="color:#8b9eb0;","  \u2022  "),
                      if(has_b)tags$span(style="color:#5dade2;font-size:11px;",paste0("B: RE ",round(c_$RE_Score,1))))
    )
  })
  
  # Compact swatch legend above each bucket chart (Plotly legend hidden to save top margin).
  # data-krish-plot / data-krish-trace pair with JS handler for Plotly.restyle toggle (same as legend click).
  krish_bucket_legend_ids <- c(
    "krish_legend_econ", "krish_legend_freedom", "krish_legend_judicial",
    "krish_legend_reperf", "krish_legend_transparency", "krish_legend_risk",
    "krish_legend_freedom_bar"
  )
  krish_bucket_plotly_ids <- c(
    "krish_comp_econ", "krish_comp_freedom", "krish_comp_judicial",
    "krish_comp_reperf", "krish_comp_transparency", "krish_comp_risk",
    "krish_freedom_bar"
  )
  for (i in seq_along(krish_bucket_legend_ids)) {
    local({
      oid <- krish_bucket_legend_ids[[i]]
      plot_id <- krish_bucket_plotly_ids[[i]]
      output[[oid]] <- renderUI({
        r <- krish_sel_row()
        c_ <- krish_cmp_row()
        if (nrow(r) == 0) return(NULL)
        leg_item <- function(color, label, trace_idx) {
          tags$span(
            class = "krish-legend-item",
            title = "Click to show or hide this series",
            style = "display:inline-flex;align-items:center;gap:5px;margin-right:12px;cursor:pointer;user-select:none;",
            `data-krish-plot` = plot_id,
            `data-krish-trace` = as.character(trace_idx),
            tags$span(style = paste0(
              "display:inline-block;width:10px;height:10px;border-radius:2px;",
              "background:", color, ";border:1px solid #0d1b2a;"
            )),
            tags$span(style = "color:#c4d3df;font-size:10px;font-weight:600;", label)
          )
        }
        tags$div(
          style = "display:flex;flex-wrap:wrap;align-items:center;margin:0 0 4px 0;padding:0;min-height:16px;line-height:1.1;",
          leg_item("#c9a84c", r$Country[[1]], 0L),
          if (nrow(c_) > 0) leg_item("#5dade2", c_$Country[[1]], 1L)
        )
      })
    })
  }
  
  krish_make_title_ui <- function(base) {
    renderUI({
      a <- input$sel_country_detail
      b <- input$krish_sel_country_compare
      if (is.null(a)) {
        return(tags$span(base))
      }
      sub <- if (is.null(b) || identical(b, "None")) {
        tags$div(
          style = "margin-top:5px;font-weight:500;font-size:11px;line-height:1.35;color:#c9a84c;",
          a
        )
      } else {
        tags$div(
          style = "margin-top:5px;font-weight:500;font-size:11px;line-height:1.35;",
          tags$span(style = "color:#c9a84c;", a),
          tags$span(style = "color:#8b9eb0;margin:0 5px;font-weight:400;", "vs"),
          tags$span(style = "color:#5dade2;", b)
        )
      }
      tags$div(
        style = "line-height:1.35;",
        tags$span(style = "display:block;", base),
        sub
      )
    })
  }
  
  # ── Bucket-aligned comparison chart titles (match Overview buckets) ──
  output$krish_title_econ         <- krish_make_title_ui("Economic Indicators")
  output$krish_title_freedom      <- krish_make_title_ui("Business Friendliness")
  output$krish_title_judicial     <- krish_make_title_ui("Judicial System")
  output$krish_title_reperf       <- krish_make_title_ui("RE Performance")
  output$krish_title_transparency <- krish_make_title_ui("Market Transparency")
  output$krish_title_risk         <- krish_make_title_ui("Risk Factor")
  output$krish_title_radar        <- krish_make_title_ui("Market Profile — Bucket Scores (0-100)")
  output$krish_title_freedom_bar  <- krish_make_title_ui("Freedom Scores")
  
  # ── Bucket-aligned comparison charts (0-100 normalized; raw as labels) ──
  output$krish_comp_econ <- renderPlotly({
    r <- krish_sel_row(); c_ <- krish_cmp_row(); if (nrow(r) == 0) return(NULL)
    krish_plot_bucket_normalized(
      c("sf_hdi","sf_gdp","sf_fdi","sf_unemp","sf_infl"),
      r, c_, krish_df_scored())
  })
  output$krish_comp_freedom <- renderPlotly({
    r <- krish_sel_row(); c_ <- krish_cmp_row(); if (nrow(r) == 0) return(NULL)
    krish_plot_bucket_normalized(
      c("sf_biz","sf_trade","sf_invest","sf_fin","sf_tax"),
      r, c_, krish_df_scored())
  })
  output$krish_comp_judicial <- renderPlotly({
    r <- krish_sel_row(); c_ <- krish_cmp_row(); if (nrow(r) == 0) return(NULL)
    krish_plot_bucket_normalized(
      c("sf_prop","sf_corr_percep","sf_reg"),
      r, c_, krish_df_scored())
  })
  output$krish_comp_reperf <- renderPlotly({
    r <- krish_sel_row(); c_ <- krish_cmp_row(); if (nrow(r) == 0) return(NULL)
    krish_plot_bucket_normalized(
      c("sf_tror10","sf_tror_py","sf_tror_vol","sf_drawdown","sf_caprate"),
      r, c_, krish_df_scored())
  })
  output$krish_comp_transparency <- renderPlotly({
    r <- krish_sel_row(); c_ <- krish_cmp_row(); if (nrow(r) == 0) return(NULL)
    krish_plot_bucket_normalized(
      c("sf_tr_overall","sf_tr_invest","sf_tr_fund","sf_tr_legal","sf_tr_sustain"),
      r, c_, krish_df_scored())
  })
  output$krish_comp_risk <- renderPlotly({
    r <- krish_sel_row(); c_ <- krish_cmp_row(); if (nrow(r) == 0) return(NULL)
    krish_plot_bucket_normalized(
      c("sf_country_risk"),
      r, c_, krish_df_scored())
  })
  
  output$krish_radar_chart <- renderPlotly({
    r <- krish_sel_row(); c_ <- krish_cmp_row()
    if (nrow(r) == 0) return(NULL)
    w      <- krish_model_weights()
    df_src <- krish_df_scored()
    
    cats <- vapply(BUCKETS, function(b) b$name, character(1))
    va   <- as.numeric(compute_bucket_scores_for_row(r, df_src, w))
    va_plot <- ifelse(is.na(va), 0, va)
    hover_a <- paste0(r$Country, "<br>", cats, ": ",
                      ifelse(is.na(va), "N/A", sprintf("%.1f", va)), "/100")
    
    p <- plot_ly(type = "scatterpolar", mode = "lines+markers",
                 r = c(va_plot, va_plot[1]), theta = c(cats, cats[1]),
                 name = r$Country, fill = "toself",
                 fillcolor = "rgba(201,168,76,.22)",
                 line = list(color = "#c9a84c", width = 2.5),
                 marker = list(color = "#c9a84c", size = 7),
                 text = c(hover_a, hover_a[1]), hoverinfo = "text")
    
    if (nrow(c_) > 0) {
      vb <- as.numeric(compute_bucket_scores_for_row(c_, df_src, w))
      vb_plot <- ifelse(is.na(vb), 0, vb)
      hover_b <- paste0(c_$Country, "<br>", cats, ": ",
                        ifelse(is.na(vb), "N/A", sprintf("%.1f", vb)), "/100")
      p <- p %>% add_trace(type = "scatterpolar", mode = "lines+markers",
                           r = c(vb_plot, vb_plot[1]), theta = c(cats, cats[1]),
                           name = c_$Country, fill = "toself",
                           fillcolor = "rgba(93,173,226,.20)",
                           line = list(color = "#5dade2", width = 2.5),
                           marker = list(color = "#5dade2", size = 7),
                           text = c(hover_b, hover_b[1]), hoverinfo = "text")
    }
    
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      polar = list(
        bgcolor     = "rgba(15,30,45,.5)",
        angularaxis = list(color = "#8b9eb0",
                           tickfont = list(size = 12, color = "#c4d3df")),
        radialaxis  = list(color = "#4a6a8a",
                           gridcolor = "rgba(30,58,90,.7)",
                           range = c(0, 100),
                           tickvals = c(25, 50, 75, 100),
                           tickfont = list(size = 9, color = "#4a6a8a"))
      ),
      legend = list(font = list(color = "#c4d3df", size = 12),
                    bgcolor = "rgba(0,0,0,0)", orientation = "h",
                    x = 0, y = 1.18, xanchor = "left", yanchor = "bottom"),
      margin = list(t = 55, b = 30, l = 90, r = 90)
    ) %>% config(displayModeBar = FALSE)
  })
  
  output$krish_freedom_bar <- renderPlotly({
    r <- krish_sel_row(); c_ <- krish_cmp_row(); if (nrow(r) == 0) return(NULL)
    metrics <- c("Business", "Tax Burden", "Trade", "Investment", "Financial")
    cv <- as.numeric(c(r$Business_Freedom, r$Tax_Burden, r$Trade_Freedom, r$Investment_Freedom, r$Financial_Freedom))
    p <- plot_ly() %>%
      add_bars(x = metrics, y = cv, name = r$Country, showlegend = FALSE,
               marker = list(color = "#c9a84c", line = list(color = "#0d1b2a", width = 1)))
    if (nrow(c_) > 0) {
      bv <- as.numeric(c(c_$Business_Freedom, c_$Tax_Burden, c_$Trade_Freedom, c_$Investment_Freedom, c_$Financial_Freedom))
      p <- p %>% add_bars(x = metrics, y = bv, name = c_$Country, showlegend = FALSE,
                          marker = list(color = "#5dade2", line = list(color = "#0d1b2a", width = 1)))
    }
    p %>% layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", showlegend = FALSE,
      font = list(family = "IBM Plex Sans", color = "#8b9eb0", size = 11),
      barmode = "group", bargap = 0.3,
      xaxis = list(showgrid = FALSE, color = "#8b9eb0", title = ""),
      yaxis = list(showgrid = TRUE, gridcolor = "rgba(30,58,90,.5)", color = "#8b9eb0",
                   title = "Score (0-100)", range = c(0, 110)),
      margin = list(t = 8, b = 40, l = 50, r = 20)
    ) %>% config(displayModeBar = FALSE)
  })
}
# =============================================================================
# End Krish comparison layer
# =============================================================================

runApp(
  shinyApp(ui = ui, server = server),
  host = "127.0.0.1",
  port = 4952,
  launch.browser = FALSE
)
