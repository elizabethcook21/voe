plot_volcano_and_find_vibrations <- function(initial_association_output,generate_volcano){
  initial_association_output = as_tibble(initial_association_output)
  if(generate_volcano==TRUE){
    plot=ggplot(initial_association_output, aes(x = estimate, y = -log10(pvalue_adjusted), label = feature)) +
      ggtitle('Output of initial associations')+
      geom_point(size=1) +
      cowplot::theme_cowplot(12)+
      geom_hline(yintercept = -log10(0.05)) +
      ylab('-log10(adjusted p-value)')+
      xlab('Esimate Size')
    ggsave('fdr_volcano_plot_initial_associations.pdf')
  }
  to_vibrate = initial_association_output %>% filter(pvalue_adjusted < 0.05) %>% select(feature) %>% unname %>% unlist
  write.csv(to_vibrate,'features_for_vibration')
  return(to_vibrate)
}

