#' One-way sensitivity analysis for CEA
#'
#' @description This is a function that provides an automatic calculation for an one-way sensitivity analysis used in CEA.
#'
#' @param data A data frame with four columns: 1) parameters 2) base value 3) lower bound and 4) upper bound.
#' @param unit The unit of the outcome variable, e.g. 2022 USD.
#'
#' @return A tornado diagram and a result data frame for one-way sensitivity analysis
#' @import ggplot2 dplyr tidyverse tidyr
#' @export
#'
#' @examples
#' mydata = data.frame(cbind("Parameter" = c("Faculty", "Student", "Doctor"),
#'                           "Base" = c(2000, 7200, 3000),
#'                           "Low" = c(1000,6000,2000),
#'                           "High" = c(3000, 9000, 5000)))
#' senanal(data = mydata, unit = "USD")
#'
senanal <- function(data, unit){

  data <- as.matrix(data)
  Parameter <- data[,1]
  base <- as.numeric(data[,2])
  low <- as.numeric(data[,3])
  high <- as.numeric(data[,4])

  ################### Calculation  ###################

  n <- length(base)
  input <- base
  Lower_Bound <- rep(0, n)
  Upper_Bound <- rep(0, n)

  for (i in 1:n) {
    input[i] <-  low[i]
    Lower_Bound[i] <-  sum(input)

    input <- base
  }

  for (i in 1:n) {
    input[i] <-  high[i]
    Upper_Bound[i] <-  sum(input)

    input <- base
  }

  UL_Difference <- Upper_Bound-Lower_Bound
  df <- data.frame(cbind(Parameter, Lower_Bound, Upper_Bound, UL_Difference))

  ################### Sort  ###################

  df$Lower_Bound <- as.numeric(df$Lower_Bound)
  df$Upper_Bound <- as.numeric(df$Upper_Bound)
  df$UL_Difference <- as.numeric(df$UL_Difference)

  base.value <- sum(base)
  outcome <- df[order(df[,4], decreasing = T),]

  if(nrow(df) < 20){
    df <- df
  } else {
    df <- outcome[c(1:20),]
  }

  ################### Graph  ###################

  # get order of parameters according to size of intervals
  order.parameters <- df %>% arrange(UL_Difference) %>%
    mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
    select(Parameter) %>% unlist() %>% levels()

  # width of columns in plot (value between 0 and 1)
  width <- 0.95

  # get data frame in shape for ggplot and geom_rect
  df.2 <- df %>%
    # gather columns Lower_Bound and Upper_Bound into a single column using gather
    gather(key='type', value='output.value', Lower_Bound:Upper_Bound) %>%
    # reordering columns
    select(Parameter, type, output.value, UL_Difference) %>%
    # create the columns for geom_rect
    mutate(Parameter=factor(Parameter, levels=order.parameters),
           ymin=pmin(output.value, base.value),
           ymax=pmax(output.value, base.value),
           xmin=as.numeric(Parameter)-width/2,
           xmax=as.numeric(Parameter)+width/2)

  # create plot
  plot <- ggplot() +
    geom_rect(data = df.2,
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
    theme_bw() +
    theme(axis.title.y=element_blank(),
          legend.position = 'right',
          legend.title = element_blank(),
          text = element_text(family = "serif"),
          plot.title = element_text(face = "bold")) +
    geom_hline(yintercept = base.value) +
    labs(y = unit,
         subtitle = paste0("The base value is: ",base.value)) +
    scale_x_continuous(breaks = c(1:length(order.parameters)),
                       labels = order.parameters) +
    # scale_y_continuous(breaks = c(outcome[1,2], base.value, outcome[1,3])) +
    scale_fill_discrete(labels = c("Lower Bound", "Upper Bound")) +
    coord_flip()

  invisible(list(outcome = outcome, tornado = plot))
}
