
plot_volcano_and_find_vibrations <- function(cleanMetaanalysis){
  data= as_tibble(cleanMetaanalysis)

  plot=ggplot(data, aes(x = estimate, y = -log10(fdr.p.val), label = feature)) +
    ggtitle('Karlsson Data vs. species abundances meta-analytic results')+
    geom_point(size=1) +
    cowplot::theme_cowplot(12)+
    geom_hline(yintercept = -log10(0.05)) +
    xlim(-2, 2)+
    ylab('-log10(p.value)')+
    xlab('Effect Size')
  ggsave('fdr_volcano_plot.png')

  to_vibrate = data %>% filter(by.p.val < 0.05) %>% select(feature) %>% unname %>% unlist
  saveRDS(to_vibrate,'data_from_plot_volcano.rds')
}

