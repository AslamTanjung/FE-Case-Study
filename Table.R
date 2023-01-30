library(dplyr)
library(xts)
library(forecast)

kernel_cov <- read.csv("Returns & RV/RV_kernel2.csv", sep = " ")
kernel_cov$Index <- as.POSIXct(kernel_cov$Index,format="%Y-%m-%d %H:%M:%S")
kernel_cov <- xts(kernel_cov$V1, kernel_cov$Index)
n_test <- 252*2
true_value <- tail(kernel_cov, n_test)

MAE <- function(forc, true_value) {
  return(as.double(abs(forc-true_value)))
}
return_name <- "CC"
ret <- read.csv(file = "Returns & RV/Close_to_close_log_returns.csv", sep = " ")
ret$Index <- as.POSIXct(ret$Index,format="%Y-%m-%d %H:%M:%S")
ret <- xts(ret$V1, ret$Index)
## MAE opnieuw aanmaken
files <- tibble(File = list.files("Forecasts", pattern = paste0("^", return_name, "vol"), full.names = TRUE)) %>%
  mutate(name =  gsub('.*vol_(.*)_Forecast.*','\\1',File))
for(i in 1:nrow(files)){
  name <- files$name[i]
  file <- files$File[i]
  forc <- read.csv(file, sep = " ")$x
  
  if(grepl("GARCH", name)){
    print(name)
    forc <- forc**2
  }
  
  if(grepl("TEGARCH", name)){
    print(name)
    forc <- forc[2:length(forc)]
  }
  
  if(return_name == "CC"){
    factor <- var(ret[2:(length(ret)-n_test)])/mean(kernel_cov[2:(length(kernel_cov)-n_test)])
    k <- true_value %*% factor
  } else {
    k <- true_value
  }

  
  MAE_value <- MAE(forc, k)
  write.csv(MAE_value, file = paste0("MAE/", return_name, "vol_", name, "_MAE.csv"), row.names = FALSE)
}

## Plots
return_name <- "CC"
files <- tibble(File = list.files("Forecasts", pattern = paste0("^", return_name, "vol"), full.names = TRUE)) %>%
  mutate(name =  gsub('.*vol_(.*)_Forecast.*','\\1',File)) %>%
  filter(name != "RealGARCHTEST")
par(mfrow = c(3, 3), mar = numeric(4))
for(i in 1:nrow(files)){
  name <- files$name[i]
  file <- files$File[i]
  forc <- read.csv(file, sep = " ")$x
  
  if(grepl("GARCH", name)){
    print(name)
    forc <- forc**2
  }
  
  plot(as.numeric(true_value), type = "l", xlab = "", ylab = "Vol", xaxt= 'n', yaxt = 'n')
  lines(forc, col = "red")
  legend(x = 'topleft', legend = name, col = "red", lty = 1)
}

return_name <- "CC"
files <- tibble(File = list.files("MAE", pattern = paste0("^", return_name, "vol"), full.names = TRUE)) %>%
  mutate(name =  gsub('.*vol_(.*)_MAE.*','\\1',File)) %>%
  filter(name != "RealGARCHTEST")
for(i in 1:nrow(files)){
  for(j in 1:nrow(files)) {
  name2 <- files$name[j]
  file2 <- files$File[j]
  MAE2 <- read.csv(file2, sep = " ")$x
  
  name1 <- files$name[i]
  file1 <- files$File[i]
  MAE1 <- read.csv(file1, sep = " ")$x

  if(all(MAE1 == MAE2)) {
    cat(paste0(name1, " - ", name2, ": -\n"))
    next
  }
  
  dm <- dm.test(MAE1, MAE2)
  stat <- round(dm$statistic,2)
  p <- round(dm$p.value, 5)
  if(p < 0.05){
    stat <- paste0(stat, "*")
  }
  cat(paste0(name1, " - ", name2, ":", stat, "\n"))
  }
}

## MEAN of MSE 
files <- list.files(path = "MSE", full.names = TRUE)
for(file in files){
  k <- read.csv(file = file)$x
  print(paste0(file, round(mean(k), 2)))
}

