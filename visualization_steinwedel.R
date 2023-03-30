# Libraries and inputs ----

library(decisionSupport)
library(tidyverse)
library(ggpubr)
library("readxl")
library(bayesplot)
library(ggplot2)

# Used in Script ----

# Total Profit Version 1
plot_distributions(mcSimulation_object = model_function_final,
                   vars = "total_a",
                   x_axis_name = "Profit in €",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Total Profit - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha    Added Values over 30 Years    10.000 model runs",
       caption = "Version 1:   Mean: 90.912 €     5% Quantile: -16.636 €     95% Quantile: 211.690 €") +
  scale_fill_discrete(name = "Decision Option", labels = c("Version 1")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))+
  geom_vline(aes(xintercept = 0))


# Cashflow Total 1
plot_cashflow(mcSimulation_object = model_function_final,
              cashflow_var_name = "vec_total_a",
              x_axis_name = "Years",
              y_axis_name = "Annual cashflow in €",
              facet_labels = "",
              color_25_75 = "green4",
              color_5_95 = "green1",
              color_median = "red") +
  labs(title = "Annual Cashflow - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha    30 Years     10.000 model runs")+
  scale_y_continuous(limits = c(-50000, 30000)) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Income vs Expenses Version 1
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("expenses_total_a", "income_total_a"),
                   x_axis_name = "in €",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Total Expenses and Income - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha    Added Values over 30 Years    10.000 model runs",
       caption = "Expenses:   Mean: 318.523 €     5% Quantile: 283.830 €     95% Quantile: 354.560 €\n Income:   Mean: 409.434 €     5% Quantile: 279.720 €     95% Quantile: 548.916 €") +
  scale_fill_discrete(name = "Legend", labels = c("Expenses","Income")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))

# Yield Field and Yield Selling
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("mean_yearly_yield_total", "mean_yearly_yield_inshell_a"),
                   x_axis_name = "in tons",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Yield before and after Processing - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha    Mean Values over Full Yield Period    10.000 model runs",
       caption = "Before Processing:   Mean: 6,44 t     5% Quantile: 4,28 t     95% Quantile: 8,68 t\n After Processing:   Mean: 4,99 t     5% Quantile: 3,3 t     95% Quantile: 6,76 t") +
  scale_fill_discrete(name = "Legend", labels = c("After Processing","Before Processing")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))

# Annual Expenses per Categorie
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("mean_investments_a", "mean_costs_a", "mean_workingcosts_a"),
                   x_axis_name = "Expenses in €",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Annual Expenses per Categorie - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha    Mean per Year    10.000 model runs",
       caption = "General Costs:   Mean: 2.047 €     5% Quantile: 1.492 €     95% Quantile: 2.611 €\n Investments:   Mean: 3.106 €     5% Quantile: 2.671 €     95% Quantile: 3.461 €\n Working Costs:   Mean: 5.450 €     5% Quantile: 4.513 €     95% Quantile: 6.414 €")+
  scale_fill_discrete(name = "Legend", labels = c("General Costs","Investments","Working Costs")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))

# Investments
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("investments_fixed_a", "investments_flexibel_a"),
                   x_axis_name = "Expenses in €",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Investments - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha    Added Values over 30 Years    10.000 model runs",
       caption = "Immovable Investments:   Mean: 28.460 €     5% Quantile: 23.254 €     95% Quantile: 33.479 €\n Moveable Investments:   Mean: 64.557 €     5% Quantile: 52.833 €     95% Quantile: 73.082 €")+
  scale_fill_discrete(name = "Legend", labels = c("Immovable Investments","Moveable Investments")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold")) + 
  scale_x_continuous(limits = c(0,90000), expand = c(0,0))

# Initial Investments
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("initial_expenses_total_a", "initial_expenses_nowork_a","initial_expenses_work_a"),
                   x_axis_name = "Expenses in €",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Initial Investments - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha    Added Values Year 1 until First Harvest    10.000 model runs",
       caption = "Total:   Mean: 69.875 €     5% Quantile: 65.508 €     95% Quantile: 74.483 €\n Without Working Costs:   Mean: 53.322 €     5% Quantile: 50.024 €     95% Quantile: 56.719 €\n Only Working Costs:   Mean: 16.474 €     5% Quantile: 14.250 €     95% Quantile: 19.041 €")+
  scale_fill_discrete(name = "Legend", labels = c("Without Working Costs","Total","Only Working Costs")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold")) + 
  scale_x_continuous(limits = c(0,90000), expand = c(0,0))

# Workingtime
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("mean_yearly_workingtime_harvest_stabilization",
                            "mean_yearly_workingtime_processing_a",
                            "mean_yearly_workingtime_plantationcare"),
                   x_axis_name = "Hours",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Annual Working Time - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha    Mean Values over Full Yield Period    10.000 model runs",
       caption = "Harvest + Stabilization:   Mean: 70,6 h     5% Quantile: 52,3 h     95% Quantile: 91,2 h\n Orchard Care:   Mean: 221,9 h     5% Quantile: 198,6 h     95% Quantile: 245,5 h\n Processing:   Mean: 219,8 h     5% Quantile: 141,1 h     95% Quantile: 303,1 h")+
  scale_fill_discrete(name = "Legend", labels = c("Harvest + Stabilization","Orchard Care","Processing")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))

# Monthly Workingtime Total 1
plot_cashflow(mcSimulation_object = model_function_final,
              cashflow_var_name = "vec_mean_monthly_working_a",
              x_axis_name = "Years",
              y_axis_name = "Working hours",
              facet_labels = "",
              color_25_75 = "green4",
              color_5_95 = "green1",
              color_median = "red") +
  labs(title = "Monthly Working Time - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha     30 years     10.000 model runs") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Total Profit Comarison
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("total_a",
                            "total_b",
                            "total_c"),
                   x_axis_name = "Hours",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Total Profit - Hazelnut Orchard Steinwedel",
       subtitle = "All Versions    2,5 ha    Added Values over 30 years    10.000 model runs",
       caption = "Version 1:   Mean: 90.912 €      5% Quantile: -16.636 €     95% Quantile: 211.690 €\n Version 2:   Mean: 366.146 €     5% Quantile: 159.644 €     95% Quantile: 593.019 €\n Version 3:   Mean: 136.208 €     5% Quantile: -19.507 €     95% Quantile: 324.058 €")+
  scale_fill_discrete(name = "Legend", labels = c("Version 1","Version 2","Version 3")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))+
  geom_vline(aes(xintercept = 0))

# Working time Processing Comparison
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("mean_yearly_workingtime_processing_a",
                            "mean_yearly_workingtime_processing_b",
                            "mean_yearly_workingtime_processing_c"),
                   x_axis_name = "Hours",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Annual Working Time Processing - Hazelnut Orchard Steinwedel",
       subtitle = "All Versions    2,5 ha    Mean Values over Full Yield Period    10.000 model runs",
       caption = "Version 1:   Mean: 219,8 h      5% Quantile: 141,1 h     95% Quantile: 303,1 h\n Version 2:   Mean: 430,5 h     5% Quantile: 279,3 h     95% Quantile: 585,9 h\n Version 3:   Mean: 485,6 h     5% Quantile: 312,0 h     95% Quantile: 668,4 h")+
  scale_fill_discrete(name = "Legend", labels = c("Version 1","Version 2","Version 3")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))

# Income per kg Harvested Hazelnuts
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("income_per_yield_a",
                            "income_per_yield_b",
                            "income_per_yield_c"),
                   x_axis_name = "€",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Income per kg Harvested Hazelnuts - Hazelnut Orchard Steinwedel",
       subtitle = "All Versions    2,5 ha    Mean Values over Full Yield Period    10.000 model runs",
       caption = "Version 1:   Mean: 2,64 €      5% Quantile: 2,22 €     95% Quantile: 3,07 €\n Version 2:   Mean: 4,86 €     5% Quantile: 4,22 €     95% Quantile: 5,54 €\n Version 3:   Mean: 3,45 €     5% Quantile: 2.63 €     95% Quantile: 4,32 €")+
  scale_fill_discrete(name = "Legend", labels = c("Version 1","Version 2","Version 3")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))+
  xlim(0,7)

# Expenses per kg Harvested Hazelnuts
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("expenses_per_yield_a",
                            "expenses_per_yield_b",
                            "expenses_per_yield_c"),
                   x_axis_name = "€",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Expenses per kg Harvested Hazelnuts - Hazelnut Orchard Steinwedel",
       subtitle = "All Versions    2,5 ha    Mean Values over Full Yield Period    10.000 model runs",
       caption = "Version 1:   Mean: 1,64 €      5% Quantile: 1,28 €     95% Quantile: 2,17 €\nVersion 2:   Mean: 2,10 €     5% Quantile: 1,68 €     95% Quantile: 2,60 €\nVersion 3:   Mean: 2,16 €     5% Quantile: 1,77 €     95% Quantile: 2,70 €")+
  scale_fill_discrete(name = "Legend", labels = c("Version 1","Version 2","Version 3")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))+
  xlim(0,7)

# Comparison different Runtimes
plot_distributions(mcSimulation_object = model_function_years,
                   vars = c("total_a_30",
                            "total_a_50",
                            "total_a_70"),
                   x_axis_name = "€",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Total Profit of Different Runtimes - Hazelnut Orchard Steinwedel",
       subtitle = "Version 1    2,5 ha    Added Values over different Runtimes    10.000 model runs",
       caption = "30 Years:   Mean: 90.912 €      5% Quantile: -16.636 €     95% Quantile: 211.690 €\n 50 Years:   Mean: 202.958 €     5% Quantile: 511 €     95% Quantile: 428.443 €\n 70 Years:   Mean: 304.904 €     5% Quantile: 3.029 €     95% Quantile: 637.879 €")+
  scale_fill_discrete(name = "Legend", labels = c("30 Years","50 Years","70 Years")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))+
  geom_vline(aes(xintercept = 0))

# Min Price
plot_distributions(mcSimulation_object = model_function_final,
                   vars = c("min_price_a",
                            "min_price_inshell_b",
                            "min_price_cracked_b",
                            "min_price_c"),
                   x_axis_name = "€",
                   method = "smooth_simple_overlay",
                   base_size = 12) +
  labs(title = "Minimum Required Prices to Break Even - Hazelnut Orchard Steinwedel",
       subtitle = "All Versions    2,5 ha    Mean Over 30 Years    10.000 model runs",
       caption = "Version 1 In-Shell:   Mean: 2,74 €      5% Quantile: 2,08 €     95% Quantile: 3,73 €\nVersion 3 Cracked:   Mean: 9,13 €     5% Quantile: 6,97 €     95% Quantile: 12,20 €\nVersion 2 Cracked:   Mean: 9,96 €     5% Quantile: 7,71 €     95% Quantile: 13,16 €\nVersion 2 In-Shell:   Mean: 2,86 €     5% Quantile: 2,20 €     95% Quantile: 3,79 €")+
  scale_fill_discrete(name = "Legend", labels = c("Version 1 In-Shell","Version 3 Cracked","Version 2 Cracked", "Version 2 In-Shell")) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 11, face = "bold"))+
  geom_vline(aes(xintercept = 0))





# PLS ----

pls_result_1 <- plsr.mcSimulation(
  object = model_function_final,
  resultName = names(model_function_final$y)[2])
pls_plot_1 <-plot_pls(pls_result_1,
         input_table = input_estimates,
         threshold = 0.6,
         base_size = 14)+
  labs(title = "Variable Importance Version 1") + 
  scale_x_continuous(limits = c(0,8), expand = c(0,0))

pls_result_2 <- plsr.mcSimulation(
  object = model_function_final,
  resultName = names(model_function_final$y)[1])
pls_plot_2 <- plot_pls(pls_result_2,
         input_table = input_estimates,
         threshold = 0.6,
         base_size = 14)+
  labs(title = "Variable Importance Version 2") + 
  scale_x_continuous(limits = c(0,8), expand = c(0,0))

pls_result_3 <- plsr.mcSimulation(
  object = model_function_final,
  resultName = names(model_function_final$y)[3])
 pls_plot_3 <-plot_pls(pls_result_3,
         input_table = input_estimates,
         threshold = 0.6,
         base_size = 14)+
  labs(title = "Variable Importance Version 3") + 
   scale_x_continuous(limits = c(0,8), expand = c(0,0))

Pls_combined <- ggarrange(pls_plot_1, pls_plot_2, pls_plot_3, 
                          labels = c(" ", " ", " "),
                          ncol = 1, nrow = 3)
annotate_figure(Pls_combined,
                top = text_grob("Projection to Latent Structures analysis",
                                face = "bold", size = 16),
)

# EVPI ----

mcSimulation_table_1 <- data.frame(model_function_final$x, model_function_final$y[2])
evpi_1 <- multi_EVPI(mc = mcSimulation_table_1, write_table = F, first_out_var = "total_a")
mcSimulation_table_1 <- data.frame(model_function_final$x, model_function_final$y[1])
evpi_2 <- multi_EVPI(mc = mcSimulation_table_1, write_table = F, first_out_var = "total_b")
mcSimulation_table_1 <- data.frame(model_function_final$x, model_function_final$y[3])
evpi_3 <- multi_EVPI(mc = mcSimulation_table_1, write_table = F, first_out_var = "total_c")
evpi_3_subset <- subset(evpi_3, variable == "years")

evpi_plot_1 <- plot_evpi(evpi_1, 
                         decision_vars = "total_a",
                         input_table = input_estimates,
                         unit = "€")+
  labs(title = "Version 1")

evpi_plot_2 <- plot_evpi(evpi_2, 
                         decision_vars = "total_b",
                         input_table = input_estimates,
                         unit = "€")+
  labs(title = "Version 2")

evpi_plot_3 <- plot_evpi(evpi_3, 
                         decision_vars = "total_c",
                         input_table = input_estimates,
                         unit = "€")+
  labs(title = "Version 3")



evpi_combined <- ggarrange(evpi_plot_1, evpi_plot_2, evpi_plot_3, 
                          labels = c(" ", " ", " "),
                          ncol = 1, nrow = 3)
annotate_figure(evpi_combined,
                top = text_grob("Value of Perfect Information Analysis",
                                face = "bold", size = 14),
)
