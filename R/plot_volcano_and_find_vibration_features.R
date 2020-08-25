plot_volcano_and_find_vibrations <- function(association_output,generate_volcano){
  association_output = as_tibble(association_output)
  if(generate_volcano==TRUE){
    plot=ggplot(association_output, aes(x = estimate, y = -log10(by.p.val), label = feature)) +
      ggtitle('Output of initial associations')+
      geom_point(size=1) +
      cowplot::theme_cowplot(12)+
      geom_hline(yintercept = -log10(0.05)) +
      ylab('-log10(adjusted p-value)')+
      xlab('Esimate Size')
    ggsave('fdr_volcano_plot_initial_associations.pdf')
  }
  to_vibrate = association_output %>% filter(by.p.val < 0.05) %>% select(feature) %>% unname %>% unlist
  write.csv(to_vibrate,'features_for_vibration')
  return(to_vibrate)
}

