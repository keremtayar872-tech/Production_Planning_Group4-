# R/utils_aggregate.R

# Calculate aggregate planning cost breakdown
calc_agg_costs <- function(df, costs) {
  c_hire <- sum(df$Hired, na.rm = TRUE) * costs$hire
  c_fire <- sum(df$Fired, na.rm = TRUE) * costs$fire
  c_hold <- sum(pmax(0, df$Inventory), na.rm = TRUE) * costs$hold
  c_reg  <- sum(df$Workers * df$Days, na.rm = TRUE) * costs$regular_day
  
  c_ot  <- if ("Overtime" %in% names(df)) sum(df$Overtime, na.rm = TRUE) * costs$overtime else 0
  c_sub <- if ("Subcontracting" %in% names(df)) sum(df$Subcontracting, na.rm = TRUE) * costs$subcontract else 0
  
  total <- c_hire + c_fire + c_hold + c_reg + c_ot + c_sub
  
  return(data.frame(
    Cost_Component = c("Hiring Cost", "Firing Cost", "Holding Cost", "Regular Labor Cost", "Overtime Cost", "Subcontracting Cost", "TOTAL COST"),
    Amount = round(c(c_hire, c_fire, c_hold, c_reg, c_ot, c_sub, total), 2)
  ))
}

# Run Chase (Zero-Inventory) Plan
run_chase_plan <- function(data, W0, I0, K, round_up = FALSE) {
  n <- nrow(data)
  df <- data
  df$Workers <- 0; df$Hired <- 0; df$Fired <- 0; df$Production <- 0; df$Inventory <- 0
  
  curr_W <- W0; curr_I <- I0
  for (t in 1:n) {
    req_prod <- max(0, df$Demand[t] - curr_I)
    req_W <- req_prod / (K * df$Days[t])
    if (round_up) req_W <- ceiling(req_W)
    
    if (req_W > curr_W) {
      df$Hired[t] <- req_W - curr_W; df$Fired[t] <- 0
    } else {
      df$Hired[t] <- 0; df$Fired[t] <- curr_W - req_W
    }
    df$Workers[t] <- req_W
    df$Production[t] <- req_W * K * df$Days[t]
    df$Inventory[t] <- curr_I + df$Production[t] - df$Demand[t]
    curr_W <- req_W; curr_I <- df$Inventory[t]
  }
  return(df)
}

# Run Level (Constant-Workforce) Plan
run_level_plan <- function(data, W0, I0, Target_IN, K, round_up = FALSE) {
  n <- nrow(data)
  df <- data
  total_net_demand <- sum(df$Demand) + Target_IN - I0
  total_days <- sum(df$Days)
  req_W <- total_net_demand / (K * total_days)
  if (round_up) req_W <- ceiling(req_W)
  
  df$Workers <- req_W; df$Hired <- 0; df$Fired <- 0; df$Production <- 0; df$Inventory <- 0
  curr_W <- W0; curr_I <- I0
  for (t in 1:n) {
    if (t == 1) {
      if (req_W > curr_W) df$Hired[t] <- req_W - curr_W else df$Fired[t] <- curr_W - req_W
    }
    df$Production[t] <- req_W * K * df$Days[t]
    df$Inventory[t] <- curr_I + df$Production[t] - df$Demand[t]
    curr_I <- df$Inventory[t]
  }
  return(df)
}

# Run Mixed Plan (Custom Workforce per Period)
run_mixed_plan <- function(data, W0, I0, K, workforce_vec, round_up = FALSE) {
  n <- nrow(data)
  df <- data
  if (length(workforce_vec) < n) {
    workforce_vec <- rep(workforce_vec, length.out = n)
  } else {
    workforce_vec <- workforce_vec[1:n]
  }
  if (round_up) workforce_vec <- ceiling(workforce_vec)
  
  df$Workers <- workforce_vec; df$Hired <- 0; df$Fired <- 0; df$Production <- 0; df$Inventory <- 0
  curr_W <- W0; curr_I <- I0
  for (t in 1:n) {
    w_t <- df$Workers[t]
    if (w_t > curr_W) {
      df$Hired[t] <- w_t - curr_W; df$Fired[t] <- 0
    } else {
      df$Hired[t] <- 0; df$Fired[t] <- curr_W - w_t
    }
    df$Production[t] <- w_t * K * df$Days[t]
    df$Inventory[t] <- curr_I + df$Production[t] - df$Demand[t]
    curr_W <- w_t; curr_I <- df$Inventory[t]
  }
  return(df)
}