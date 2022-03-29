mclust.plot <- function(mod){
  
  means <- data.frame(mod$parameters$mean, stringsAsFactors = FALSE) %>%
    rownames_to_column() %>%
    rename(Variable = rowname) %>%
    melt(id.vars = "Variable", variable.name = "Class", value.name = "Mean") 
  
  means$Class = as.character(paste0(1:mod$G))
  
  #Plot profiles
  means%>%
    ggplot(aes(Variable, Mean, group = Class, color = Class)) +
    geom_point(size = 2.25) +
    geom_line(size = 1.25) +
    scale_x_discrete(limits = rownames(mod$parameter$mean)) +
    labs(x = NULL, y = "Z-score") +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
}
