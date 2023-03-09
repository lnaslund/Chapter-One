
periods <- unique(deans_pts_16$trip)
var.lst <- list()

dean_16_var_co2 <- vgm(psill = 1, "Sph", range = 18)

for(i in 1:length(periods)){
  var.lst$data[[i]] <- deans_pts_16 %>% filter(trip == periods[i])

  # quantile-quantile normal score transform data
  var.lst$data[[i]]$norm_co2_g_hr <- qqnorm(var.lst$data[[i]]$flux_co2_g_hr, plot.it = F)$x

  var.lst$nscore.tab[[i]] <- nscore(var.lst$data[[i]]$flux_co2_g_hr)

  var.lst$var_df[[i]] <- variogram(norm_co2_g_hr~1, var.lst$data[[i]] %>% filter(is.na(norm_co2_g_hr) == F), cloud = F)
  
  preds <= variogramLine(dean_16_var_co2, maxdist = max(var.lst$var_df[[i]]$dist))

  var.lst$var_plot[[i]] <- ggplot(var.lst$var_df[[i]], aes(dist, gamma))+geom_point(size =2)+ geom_line(data = preds, color = "red") +theme_bw()+labs(title = periods[i])

}

# make a pdf of all of the experimental variograms
#pdf("5-Results/1-kriged-estimates/variograms_deans_20220816_co2.pdf", paper="a4", width=9, height=12)
marrangeGrob(grobs=compact(var.lst$var_plot), nrow=4, ncol = 2)
#dev.off()
