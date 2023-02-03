###############################################
# Make simple plots from SDA experiment summary
###############################################

# package imports, function definitions, and globals including directory
library(rstudioapi)
script_dir <- dirname(getSourceEditorContext()$path)
source(paste0(script_dir, "/00_packages_functions_globals.R"))

# sda summary calculated by hand
sum_df <- read.csv(paste0(wd$raw_data, "2023_02_03_sda_experiment_summary.csv"))
colnames(sum_df)[1] <- 'm_flag'
sum_df$diff_growth <- sum_df$growth_C1 - sum_df$growth_c2  # C1 always has higher growth than C2
sum_df$diff_byint <- sum_df$b_y_int_C2 - sum_df$b_y_int_C1  # C2 always has higher by int than C1
sum_df$diff_budget <- sum_df$budget_C2 - sum_df$budget_C1

# reshape for facetting
plot_df <- sum_df[, c('m_flag', 'diff_growth', 'diff_byint', 'diff_budget')] %>%
  pivot_longer(cols=c('diff_growth', 'diff_byint'),
               names_to='driver', values_to='difference')

plot_df$driver <- factor(plot_df$driver, levels=c('diff_byint', 'diff_growth'),
                         labels=c("Driver: base year intensity",
                                  "Driver: activity growth"))
plot_df$m_flag <- factor(plot_df$m_flag, levels=c('strict', 'default'),
                         labels=c('Strict implementation', 'Default m'))
p <- ggplot(plot_df, aes(x=difference, y=diff_budget))
p <- p + geom_point()
p <- p + facet_grid(m_flag~driver, scales='free')
p <- p + xlab("Difference in driver") + ylab("Difference in budget")
print(p)

filename <- (paste0(wd$figs, "SDA_experiment_summary.png"))
png(filename, width=6, height=4, units='in', res=300)
print(p)
dev.off()

