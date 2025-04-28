#' @title Correlation matrix to Venn diagram
#' @description This function transforms correlation matrices into Venn diagrams. The shared surface area of circles corresponds to the shared variance (R squared) or to another metric (e.g. Pearson / Spearman correlation). The algorithm is an approximation based on a quasi-Newton algorithm.
#' @param cormat A (square) correlation matrix (or other n times n numeric matrix)
#' @param squared Shared surface areas between circles correspond to the squared correlation matrix (defaults to TRUE)
#' @param cor2dist Euclidean distances between data points are proportional to correlations / signed R squared.
#' @param fillmode Coloring of circles ("Eigen", "mclust", "manual")
#' @param threshold Drop variables with a fit below threshold (default -1 = none dropped)
#' @param autorecode Automatically recodes the correlation matrix (defaults to FALSE)
#' @param maxit Maximum number of iterations for the optimizer (defaults to 100)
#' @param startingvalues Optional start values
#' @return A list with optimized coordinates, model fit, CPU time, etc.
#' @export
cor2Venn <- function(cormat, Rsquared=TRUE, cor2dist=TRUE, autorecode=FALSE, maxit=100, threshold=-1, startingvalues=list()) {

  start_time <- Sys.time()

  c <- completeCormat(cormat)
  cold <- c

  if (autorecode) {
    c <- autorecode(c)
  }

  e <- eigen(c)
  mc <- Mclust(c)

  print("Optimizing... Abort pressing ESC")

  n <- ncol(c)

  if (length(startingvalues) == 0) {
    startx <- e$vectors[,1]
    starty <- e$vectors[,2]
  } else {
    startx <- startingvalues$x
    starty <- startingvalues$y
  }

  l <- rep(-1 * sqrt(length(startx)), length(startx) * 2)
  u <- rep(1 * sqrt(length(startx)), length(startx) * 2)

  exclude <- logical(0)
  belows <- NA

  while (length(belows) > 0) {

    if (length(exclude) > 0) {
      startx <- startx[-exclude]
      starty <- starty[-exclude]
      lower_bounds <- l[-exclude]
      upper_bounds <- u[-exclude]
      c_use <- c[-exclude, -exclude]
    } else {
      lower_bounds <- l
      upper_bounds <- u
      c_use <- c
    }

    # Wrapped optim call
    o <- optim(
      par = c(startx, starty),
      fn = function(par) fit(par, cormat = c_use, Rsq = Rsquared, cor2d = cor2dist),
      method = "L-BFGS-B",
      lower = lower_bounds,
      upper = upper_bounds,
      control = list(fnscale = 1, maxit = maxit, trace = 3, REPORT = 1)
    )

    # Extract coordinates
    x <- o$par[1:length(startx)]
    y <- o$par[(length(startx) + 1):(length(startx) * 2)]
    s <- rep(1, length(startx))  # radius fixed

    # Create full vector with NAs for dropped variables
    se <- seq(1, n, 1)
    fill <- setdiff(se, exclude)
    xna <- rep(NA, n)
    yna <- rep(NA, n)

    xna[fill] <- x
    yna[fill] <- y

    end_time <- Sys.time()
    totaltime <- as.numeric(end_time - start_time)

    if (!is.na(paste(startingvalues, collapse = ""))) {
      totaltime <- totaltime + startingvalues$cputime
    }

    optimization <- matrix(c(Rsquared, cor2dist), ncol = 2)
    colnames(optimization) <- c("Rsquared", "cor2dist")

    result <- list(optimization, x, y, s, NA, as.numeric(totaltime), c, exclude, xna, yna)
    names(result) <- c("optimization", "x", "y", "radius", "modelfit", "cputime", "cormat", "exclude", "xna", "yna")

    goodness <- ov(result, Rsquared = Rsquared)

    result <- list(optimization, x, y, s, goodness, as.numeric(totaltime), c, exclude, xna, yna)
    names(result) <- c("optimization", "x", "y", "radius", "modelfit", "cputime", "cormat", "exclude", "xna", "yna")

    vf <- varfit(result, Rsquared=Rsquared)
    print(exclude)
    print(vf)

    belows <- which(vf[,1] < threshold)
    mi <- belows

    exclude <- c(exclude, mi)

    # ⛔ Corrected here: no result$x anymore!
    startx <- x
    starty <- y
  }

  # Final result
  result <- list(optimization, x, y, s, goodness, as.numeric(totaltime), c, cold, exclude, xna, yna, vf)
  names(result) <- c("optimization", "x", "y", "radius", "modelfit", "cputime", "cormat", "cormat_not_recoded", "exclude", "xna", "yna", "varfit")

  return(result)
}
