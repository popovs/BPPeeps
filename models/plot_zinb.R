plot_zinb <- function(m, 
                      fl = c("Shorebird abundance", "Shorebird absence"), 
                      order.terms = TRUE, order_by = "conditional",
                      free_x = TRUE) {
  # Plot label setup
  ef_facet_labels <- c("conditional" = fl[1], "zero_inflated" = fl[2])
  
  # Calculate incidence rate ratios
  # Based on effect sizes (exponentiated coefficients https://stats.stackexchange.com/questions/311556/help-interpreting-count-data-glmm-using-lme4-glmer-and-glmer-nb-negative-binom)
  ef <- as.data.frame(effectsize::effectsize(m))
  ef <- ef[ef$Component != "dispersion", ] 
  ef <- ef[ef$Parameter != "(Intercept)", ]
  ef[,c("IRR", "IRR_lwr", "IRR_upr")] <- exp(ef[,c("Std_Coefficient", "CI_low", "CI_high")])
  
  # Reorder factor levels of plot based on conditional component
  if (order.terms) {
    if (order_by == "conditional") {
      lvl <- ef %>% 
        dplyr::filter(Component == "conditional") %>% 
        dplyr::arrange(IRR) %>%
        dplyr::mutate(Parameter = factor(Parameter, levels = Parameter))
      ef$Parameter <- factor(ef$Parameter, levels = levels(lvl$Parameter))
    } else if (order_by == "zi") {
      lvl <- ef %>% 
        dplyr::filter(Component == "zero_inflated") %>% 
        dplyr::arrange(IRR) %>%
        dplyr::mutate(Parameter = factor(Parameter, levels = Parameter))
      ef$Parameter <- factor(ef$Parameter, levels = levels(lvl$Parameter))
    } else {
      stop("Please provide valid order_by argument ('conditional' or 'zi').")
    }
  }
  
  # Plot
  p <- ef %>% 
    ggplot(aes(x = Parameter, y = IRR)) + 
    geom_pointrange(aes(ymin = IRR_lwr, 
                        ymax = IRR_upr, 
                        colour = cut(IRR, c(-Inf, 1, Inf)))) +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,5)]) +
    # scale_color_manual(values = c("(-Inf,1]" = "red",
    #                               "(1, Inf]" = "blue")) +
    scale_y_continuous(trans = "log10") +
    facet_wrap(~ Component, nrow = 2, scales = ifelse(isTRUE(free_x), "free_x", "fixed"), labeller = labeller(Component = ef_facet_labels)) + 
    coord_flip() + 
    labs(y = "Incidence Rate Ratios") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          legend.position = "none")
  
  return(p)
}

plot_dual_zinb <- function(m1, m2, 
                           shape = c("WESA", "DUNL"),
                           fl = c("Shorebird abundance", "Shorebird absence"), 
                           order.terms = TRUE, order_by = "conditional",
                           free_x = TRUE) {
  # Plot label setup
  ef_facet_labels <- c("conditional" = fl[1], "zero_inflated" = fl[2])
  
  # Calculate incidence rate ratios
  # Based on effect sizes (exponentiated coefficients https://stats.stackexchange.com/questions/311556/help-interpreting-count-data-glmm-using-lme4-glmer-and-glmer-nb-negative-binom)
  ef1 <- as.data.frame(effectsize::effectsize(m1))
  ef1$shape <- shape[[1]]
  ef2 <- as.data.frame(effectsize::effectsize(m2))
  ef2$shape <- shape[[2]]
  ef <- rbind(ef1, ef2)
  
  ef <- ef[ef$Component != "dispersion", ] 
  ef <- ef[ef$Parameter != "(Intercept)", ]
  ef[,c("IRR", "IRR_lwr", "IRR_upr")] <- exp(ef[,c("Std_Coefficient", "CI_low", "CI_high")])
  
  # Reorder factor levels of plot based on conditional component
  if (order.terms) {
    if (order_by == "conditional") {
      lvl <- ef %>% 
        dplyr::filter(Component == "conditional", shape == shape[[1]]) %>% 
        dplyr::arrange(IRR) %>%
        dplyr::mutate(Parameter = factor(Parameter, levels = Parameter))
      ef$Parameter <- factor(ef$Parameter, levels = levels(lvl$Parameter))
    } else if (order_by == "zi") {
      lvl <- ef %>% 
        dplyr::filter(Component == "zero_inflated", shape == shape[[1]]) %>% 
        dplyr::arrange(IRR) %>%
        dplyr::mutate(Parameter = factor(Parameter, levels = Parameter))
      ef$Parameter <- factor(ef$Parameter, levels = levels(lvl$Parameter))
    } else {
      stop("Please provide valid order_by argument ('conditional' or 'zi').")
    }
  }
  
  # Plot
  p <- ef %>% 
    ggplot(aes(x = Parameter, y = IRR)) + 
    geom_pointrange(aes(ymin = IRR_lwr, 
                        ymax = IRR_upr, 
                        colour = cut(IRR, c(-Inf, 1, Inf)),
                        shape = shape)) +
    guides(colour = "none") +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,5)]) +
    # scale_color_manual(values = c("(-Inf,1]" = "red",
    #                               "(1, Inf]" = "blue")) +
    scale_y_continuous(trans = "log10") +
    facet_wrap(~ Component, nrow = 2, scales = ifelse(isTRUE(free_x), "free_x", "fixed"), labeller = labeller(Component = ef_facet_labels)) + 
    coord_flip() + 
    labs(y = "Incidence Rate Ratios") +
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank())
  
  return(p)
}
