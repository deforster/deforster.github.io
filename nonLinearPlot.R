# Made by Danny Forster (forster.danny@gmail.com)
# Last Updated October 7, 2023

## How to use:

# nonLinearPlot(model = c(lm, glm, lmer, glmer), data = data_name, type = c("quad", "piece"), x1 = "x1", x2 = "x2", mod = "moderator", scale = 0, piece.sep = NULL, binary = FALSE, ylab = NULL, xlab = NULL, main = NULL, legend = NULL)

# model: a model of class 'lm', 'glm', 'lmer', or 'glmer'
# data: name of a data.frame that contains the data used in the model
# type: whether your model includes a quadratic ("quad") or piecewise predictor ("piece")
# x1: For quadratic, name of the linear variable. For piecewise, name of the first piece.
# x2: For quadratic, name of the squared variable. For piecewise, name of the second piece.
# mod: name of the moderator variable
# scale: By how much to adjust the x-axis to undo mean- or min-centering
# piece.sep: The knot value that ties x1 to x2 in a piecewise regression.
# binary: whether the moderator is a binary variable.
# ylab: label for the y-axis
# xlab: label for the x-axis
# main: main title
# legend: legend title

nonLinearPlot <- function(model, data, type, x1, x2, mod = NULL, scale = 0, piece.sep = NULL, binary = FALSE, ylab = NULL, xlab = NULL, main = NULL, legend = NULL) {
  data <- as.data.frame(data)
  if (!is.null(summary(model)$isLmer)) {
    if (class(model)[1] == "lmerMod" | class(model)[1] == "glmerMod") {
      # Get positions of lmer coefficients in fixef()
      b1PosFE <- which(attributes(fixef(model))$name == x1)
      b2PosFE <- which(attributes(fixef(model))$name == x2)
      modPosFE <- which(attributes(fixef(model))$name == mod)
      b1mPosFE <- which(attributes(fixef(model))$name == paste(x1, mod, sep = ":") | attributes(fixef(model))$name == paste(mod, x1, sep = ":")) 
      b2mPosFE <- which(attributes(fixef(model))$name == paste(x2, mod, sep = ":") | attributes(fixef(model))$name == paste(mod, x2, sep = ":"))
      }
      # Get Coefficients
      int <- fixef(model)[1]
      b1 <- fixef(model)[b1PosFE]
      b2 <- fixef(model)[b2PosFE]
      m <- ifelse(length(b1mPosFE) == 0 & length(b2mPosFE) == 0, 0, fixef(model)[modPosFE])
      b1m <- ifelse(length(b1mPosFE) == 0, 0, fixef(model)[b1mPosFE])
      b2m <- ifelse(length(b2mPosFE) == 0, 0, fixef(model)[b2mPosFE])
    }
  else {
    if (!is.null(attributes(model)$class)) {
      if ("lm" %in% attributes(model)$class) {
        # Get positions of lm coefficients in model$coefficients
        b1PosFE <- which(attributes(model$coefficients)$names == x1)
        b2PosFE <- which(attributes(model$coefficients)$names == x2)
        modPosFE <- which(attributes(model$coefficients)$names == mod)
        b1mPosFE <- which(attributes(model$coefficients)$names == paste(x1, mod, sep = ":") | attributes(model$coefficients)$names == paste(mod, x1, sep = ":"))
        b2mPosFE <- which(attributes(model$coefficients)$names == paste(x2, mod, sep = ":") | attributes(model$coefficients)$names == paste(mod, x2, sep = ":"))
        # Get Coefficients
        int <- model$coefficients[1]
        b1 <- model$coefficients[b1PosFE]
        b2 <- model$coefficients[b2PosFE]
        if (is.null(mod)) {
          m <- 0
        }
        else {
          m <- model$coefficients[modPosFE] 
        }
        if (length(b1mPosFE) == 0) {
          b1m <- 0
        }
        else {
          b1m <- model$coefficients[b1mPosFE]
        }
        if (length(b2mPosFE) == 0) {
          b2m <- 0
        }
        else {
          b2m <- model$coefficients[b2mPosFE]
        }
      }
    }
  }
  # Get positions of variables in data set
  b1PosVAR <- which(colnames(data) == x1)
  b2PosVAR <- which(colnames(data) == x2)
  modPosVAR <- which(colnames(data) == mod)
  # Make independent vectors from data set
  b1RAW <- data[, b1PosVAR]
  b2RAW <- data[, b2PosVAR]
  if (is.null(mod)) {
    modRAW <- 0
  }
  else {
    modRAW <- data[, modPosVAR]
  }
  if (type == "quad") {
    # For raw plotting
    xNew <- seq(min(b1RAW, na.rm = T), max(b1RAW, na.rm = T), length = 1000)
    # For quadratic
    x1New <- xNew
    x2New <- xNew^2
  }
  if (type == "piece") {
    # For raw plotting
    xNew <- seq(min(b1RAW, na.rm = T), max((b1RAW + b2RAW), na.rm = T), length = 1000)
    # For piecewise
    x1New <- ifelse(xNew <= piece.sep, xNew, piece.sep)
    x2New <- ifelse(xNew > piece.sep, xNew - piece.sep, 0)
  }
  if (binary == TRUE) {
    # Get fixed values of the moderator
    if (is.null(mod)) {
      modLow <- 0
      modHigh <- 1
    }
    else {
      modLow <- min(modRAW, na.rm = T)
      modHigh <- max(modRAW, na.rm = T)
    }
    # Make prediction lines
    pred.low <- int + b1*x1New + b2*x2New + m*modLow + b1m*x1New*modLow + b2m*x2New*modLow
    pred.high <- int + b1*x1New + b2*x2New + m*modHigh + b1m*x1New*modHigh + b2m*x2New*modHigh
    # Create three vectors
    if (is.null(mod)) {
      modLevel <- rep(NA, 2000)
    }
    else {
      modLevel <- c(rep(0, 1000), rep(1, 1000))
    }
    pred.Y <- c(pred.low, pred.high)
    xNew3 <- rep(xNew + scale, 2)
    # Create data frame to work with
    nonLinearData <- data.frame(modLevel, pred.Y, xNew3)
    nonLinearData$modLevel <- as.factor(nonLinearData$modLevel)
  }
  else {
    # Get fixed values of the moderator
    if (is.null(mod)) {
      modLow <- 0
      modAvg <- 0
      modHigh <- 0
    }
    else {
      modLow <- mean(modRAW, na.rm = T) - sd(modRAW, na.rm = T)
      modAvg <- mean(modRAW, na.rm = T)
      modHigh <- mean(modRAW, na.rm = T) + sd(modRAW, na.rm = T)
    }
    # Make prediction lines
    pred.low <- int + b1*x1New + b2*x2New + m*modLow + b1m*x1New*modLow + b2m*x2New*modLow
    pred.avg <- int + b1*x1New + b2*x2New + m*modAvg + b1m*x1New*modAvg + b2m*x2New*modAvg
    pred.high <- int + b1*x1New + b2*x2New + m*modHigh + b1m*x1New*modHigh + b2m*x2New*modHigh
    # Create three vectors
    if (is.null(mod)) {
      modLevel <- rep(NA, 3000)
    }
    else {
      modLevel <- c(rep("Mean -1SD", 1000), rep("Mean", 1000), rep("Mean +1SD", 1000))
    }
    pred.Y <- c(pred.low, pred.avg, pred.high)
    xNew3 <- rep(xNew + scale, 3)
    # Create data frame to work with
    nonLinearData <- data.frame(modLevel, pred.Y, xNew3)
    if (!is.null(mod)) {
      nonLinearData$modLevel <- factor(nonLinearData$modLevel, levels = c("Mean -1SD", "Mean", "Mean +1SD"))
    }
  }
  if (is.null(ylab)) {
    ylab <- summary(model)$call$formula[2]
  }
  if (is.null(xlab)) {
    xlab <- x1
  }
  if (is.null(main)) {
    main <- summary(model)$call$formula
  }
  if (is.null(legend)) {
    legend <- mod
  }
  # Use ggplot2 to create plot (update for v4 to that group is distinguished by color)
  if (is.null(mod)) {
    interactionPlot <- ggplot(nonLinearData, aes(x = xNew3, y = pred.Y)) +
      geom_line(aes(y = pred.Y)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = main, x = xlab, y = ylab)
  }
  else {
    interactionPlot <- ggplot(nonLinearData, aes(x = xNew3, y = pred.Y, linetype = modLevel)) +
      geom_line(aes(y = pred.Y)) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title = main, x = xlab, y = ylab, linetype = legend)
  }
  
  interactionPlot
}
