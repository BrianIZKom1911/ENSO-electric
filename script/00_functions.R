# Required packages
pkg_needed <- c("tidyverse", "AER", "urca", "here", "data.table", "broom")

# Check if your computer has installed these packages
install_missing <- function(pkgnames) {
  installed <- pkgnames %in% rownames(installed.packages())
  if (any(!installed)) {
    cat("Installing packages:\n")
    cat(pkgnames[!installed], sep = ", ")
    cat("\n")
    install.packages(pkgnames[!installed])
  } else {
    cat("All required packages already exist.\n")
  }
}

install_missing(pkd_needed)

# Other self-defined functions
GetMonthcol <- function(data){
  # Input data has a column of month
  data %>%
    mutate(m01 = as.numeric(Month==1),
           m02 = as.numeric(Month==2),
           m03 = as.numeric(Month==3),
           m04 = as.numeric(Month==4),
           m05 = as.numeric(Month==5),
           m06 = as.numeric(Month==6),
           m07 = as.numeric(Month==7),
           m08 = as.numeric(Month==8),
           m09 = as.numeric(Month==9),
           m10 = as.numeric(Month==10),
           m11 = as.numeric(Month==11),
           m12 = as.numeric(Month==12)) -> data
  return(data)
}

# Functions to create a table of bt values for graphs
create_sim_mx <- function(x_min, x_max, tmin, tmax, order, intercept){
  # order=c(p, q) the numbers of pairs and poly; ncol = 2q+p+1
  p <- order[1]; q <- order[2]
  r <- seq(x_min, x_max, 0.5)
  s <- (r - tmin)/(tmax - tmin)
  if (p < 1) {
    mx1 <- NULL
  } else {
    mx1 <- matrix(sapply(1:p, function(i) s^i), ncol=p, byrow=FALSE)
  }
  if (q < 1) {
    mx2 <- NULL
  } else {
    mx2 <- matrix(sapply(1:q, function(i) c(sin(2*i*pi*s), cos(2*i*pi*s))), ncol=2*q, byrow=FALSE)
  }
  ones <- matrix(1, nrow=length(s), ncol=1)
  if (intercept){
    sim_x <- cbind(ones, mx1, mx2)
  } else {
    sim_x <- cbind(mx1, mx2)
  }
  return(sim_x)
}

fill_table <- function(list, x_min, x_max, sim_mx, round){
  # The function requires the output of create_sim_mx().
  # Build an empty dataframe
  df_trf_bt <- data.frame(
    matrix(NA, nrow = ((x_max-x_min)/0.5+1), ncol=(round+1))
  )
  names <- do.call(paste0, expand.grid("round_", 1:round))
  names <- c("temperature", names)
  colnames(df_trf_bt) <- names
  
  # Fill in temp and add columns
  df_trf_bt$temperature <- seq(x_min, x_max, 0.5)
  df_trf_bt$CI_lb <- NA
  df_trf_bt$fv_median <- NA
  df_trf_bt$CI_ub <- NA
  
  # Loop: every round has a set of fitted values
  for (rd in 1:round){
    coef_vec <- list[[rd]] # the coef in the list must match the length 2q+p+1
    ghat_x <- sim_mx %*% coef_vec
    j <- rd + 1
    df_trf_bt[, j] <- ghat_x[, 1]
    print(rd)
  }
  # Loop: quantile at each temperature
  for (i in 1:nrow(df_trf_bt)) {
    fv_r <- as.numeric(df_trf_bt[i, 2:(round+1)])
    df_trf_bt[i, (round+2):(round+4)] <- quantile(fv_r, probs = c(25, 500, 975)/1000)
  }
  return(df_trf_bt)
}

bootstrap_ctrf <- function(data, y_term, x_term, round){
  # dependent on predefined vars0, vars1, ...
  #i) original regression
  y <- log(data[[y_term]])
  Xa <- as.matrix(data[, x_term])
  ones <- matrix(1, nrow=length(y), ncol=1)
  X <- cbind(ones, Xa)
  rgr <- lm(y ~ Xa, na.action = "na.exclude")
  uo <- resid(rgr) # unchanged, original residuals
  tib_coef <- broom::tidy(rgr)
  betao <- as.matrix(tib_coef[, 2]) # unchanged, original coefficients
  
  #ii) bootstrap regression
  list_bt_beta0 <- vector("list", 1000)
  list_bt_beta1 <- vector("list", 1000)
  list_bt_beta2 <- vector("list", 1000)
  
  for (rd in 1:1000){
    print(rd)
    list_bt_beta0[rd] <- tib_coef[which(tib_coef$term %in% vars0), 2]
    list_bt_beta1[rd] <- tib_coef[which(tib_coef$term %in% vars1), 2]
    list_bt_beta2[rd] <- tib_coef[which(tib_coef$term %in% vars2), 2]
    u_ <- sample(uo, replace = TRUE)
    y_ <- X %*% betao + u_
    rgr_ <- lm(y_ ~ Xa)
    tib_coef <- broom::tidy(rgr_)
  }
  lists_bt_beta <- list(list_bt_beta0, list_bt_beta1, list_bt_beta2)
  return(lists_bt_beta)
}
