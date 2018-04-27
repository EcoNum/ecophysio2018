# Functions respirometry

# Correction des valeurs enregistrées en O2 dans les repsiromètres en fonction
# des valeurs mesurées manuellement
correct_monitoring <- function(dates, values, calib.dates, calib.values, extrapolate = FALSE) {
  if (isTRUE(extrapolate)) rule <- 2 else rule <- 1
  # Approximate the values in the series to the manual dates
  series.values <- approx(dates, values, xout = calib.dates, rule = rule)$y
  # Calculate deltas between series measurements and manual values
  deltas <- calib.values - series.values
  corr <- data.frame(dates = calib.dates, values = calib.values,
    measures = series.values, deltas = deltas)
  # Interpolate linearly these deltas
  all_deltas <- approx(calib.dates, deltas, xout = dates, rule = rule)$y
  # Apply the correction
  res <- values + all_deltas
  structure(res, correction = corr, dates = dates, deltas = all_deltas, class = c("corrected", "numeric"))
}

plot.corrected <- function(x, y, ...) {
  if (missing(y)) y <- x - attr(x, "deltas")
  dates <- attr(x, "dates")
  range <- range(x, y, na.rm = TRUE)
  plot(dates, y, type = "l", col = "gray", ylim = range)
  lines(dates, x, type = "l", col = "red")
  points(attr(x, "correction")$dates, attr(x, "correction")$values, col = "red")
}

# Exemple
## Correction du pH mesuré en continu à partir de mesures quotidiennes
#pH <- rep(8.1, 24 * 3 + 1) + sin((1:(24 * 3 + 1)) / 2)/10 +
#  rnorm(24 * 3 + 1, mean = 0, sd = 0.02)
## Transformation en série temporelle, en tenant compte du temps écoulé
## Mesures toutes les heures:
#dates <- Sys.time() + 1:(24 * 3 + 1) * 3600
## Unité temporelle = 1 jour
#pH.ts <- ts(pH, start = 0, frequency = 24)
#plot(pH.ts)
#
## Etalonnage quotidien
#pH2 <- c(8.12, 8.07, 8.05, 7.98)
#dates2 <- Sys.time() + 0:3 * (24 * 3600)
#pH2.ts <- ts(pH2, start = 0, frequency = 1)
#plot(pH2.ts)
#
#plot(dates, pH, type = "l")
#lines(dates2, pH2, col = "red")
#
#pHcorr <- correct_monitoring(dates, pH, dates2, pH2, extrapolate = TRUE)
#plot(pHcorr)



# Oxygen balance on a chosen time interval from respirometer data
respirometry <- function(data, series, pos, n = 1, mass = 1, vol.respi = 1.3,
  ref.respi = 0, main = "Variation d'oxygène en respiromètre", ...) {
  if (!is.integer(pos))
    stop("'pos' must be a vector of integers")
  if ((length(pos) %% 2) != 0)
    stop("'pos' must be a vector of even length (pairs of starts and stops)")
  if (length(pos) < n * 2)
    stop("Cannot take 'n' = ", n, " item when only ", length(pos), " elements in 'pos'")
  posn <- pos[c(n * 2 - 1, n * 2)]
  measure <- data[posn[1]:posn[2], c("Time", series)]
  names(measure) <- c("Time", "O2")

  plot(measure$Time, measure$O2, type = "l", xlab = "Temps", ylab = "[O2] (mg/L)",
    main = main, ...)

  reg <- lm(O2 ~ Time, data = measure)
  print(summary(reg))
  res <- coef(reg)["Time"] * 3600 * vol.respi

  abline(coef = coef(reg), col = "red", lwd = 2)

  res_corr <- res - ref.respi

  res_corr_mass <- res_corr / mass
  res <- data.frame(
    time0 = as.POSIXct(measure$Time[1]),
    time1 = as.POSIXct(measure$Time[nrow(measure)]),
    respi = res_corr_mass,
    ref.respi = ref.respi,
    ref = main)
  attr(res, "metadata") <- list(n = n, pos = posn, mass = mass,
    vol.respi = vol.respi, reg = reg)
  res
}

# Example of use: read data for two consecutive days
#datarespi <- rbind(readRDS("IKS.Data.2017-05-01.rds"),
#                   readRDS("IKS.Data.2017-05-02.rds"))

# Choice of the series, and indication of mass and ref.respi
#series <- "O2mgL4"
#mass <- 74 #g
#ref.respi <- -0.01
#title <- "Etoile de mer"
#n1 <- 1

# Graph over the whole time
#plot(datarespi$Time, datarespi[[series]], type = "l", xlab = "Time",
#  ylab = "[O2] mg/L"); grid()

# Identify the time period to use
#pos <- identify(datarespi$Time, datarespi[[series]])

# Calculate respirometry for the various phases
#res <- respirometry(datarespi, series, pos, n = n1, mass = mass, ref.respi = ref.respi, main = title)
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+1, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+2, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+3, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+4, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+5, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+6, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+7, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+8, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+9, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+10, mass = mass, ref.respi = ref.respi, main = title))
#res <- rbind(res, respirometry(datarespi, series, pos, n = n1+11, mass = mass, ref.respi = ref.respi, main = title))

#res
# Attention! Eliminer les valeurs abberantes + séparer jour/nuit
#mean(res$respi)
#sd(res$respi)
