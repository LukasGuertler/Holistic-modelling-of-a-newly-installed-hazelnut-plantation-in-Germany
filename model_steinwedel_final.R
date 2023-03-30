# Libraries and inputs ----

library(decisionSupport)
library("readxl")


input_estimates <- read_excel("input_model_steinwedel_final.xlsx")
discount_rate = 3

make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}
make_variables(as.estimate(input_estimates))

model_steinwedel_final <- function() {
  
  # Functions: ----
  # To calculate time and height of replacement costs for machinery
  replace <- function(replacement_time, investment, start = 0, vec = investments_yearly, limit = 5) {
    if ((years - limit) > (replacement_time + start)){
      vec[seq((replacement_time + start), length(vec), by = replacement_time)] <- 
        vec[seq((replacement_time + start), length(vec), by = replacement_time)] + investment
    }
    return(vec)
  }
  
  # Monthly workingtime irrigation
  monthly_workingtime_irrigation <- function(monthly_irrigations, 
                                             irrigation_month, 
                                             max_irri = max_irrigations, 
                                             min_irri = min_irrigations, 
                                             length = years, 
                                             varCV = irrigations_varCV, 
                                             workingtime = workingtime_irrigation, 
                                             vec = vec_monthly_workingtime_irrigation){
    vec_irrigation <- vv(var_mean = monthly_irrigations,
                         var_CV = varCV,
                         n = length,
                         lower_limit = min_irri,
                         upper_limit = max_irri)
    vec_irrigation <- vec_irrigation * workingtime
    vec[irrigation_month,] <- vec[irrigation_month,] + vec_irrigation
    return(vec)
  }
  
  # Post Harvest for three different versions
  post_harvest <- function(version, 
                           percentage_cracked = percentage_partly_cracked, 
                           vec_yield = vec_hazelnut_yield_total, 
                           pest = vec_pest_chance, 
                           cores = core_content, 
                           sorting_losses = sorting_losses_nuts, 
                           investments = post_harvest_investments, 
                           replacements = post_harvest_replacements, 
                           workingtime = post_harvest_workingtimes, 
                           length = years, 
                           start_year = first_harvest,
                           varCV = varCV_cleaning,
                           drying_factor = yield_factor_drying){
    
    vec_investment <- rep(0, length)
    vec_monthly_workingtime_postharvest <- matrix(0, nrow = 12, ncol = length, byrow = TRUE)
    vec_kernels <- rep(0, length)
    
    # Investments post harvest
    if (version == 1 || version == 3){
      vec_investment[start_year] <- sum(unlist(investments))
      vec_investment <- replace(replacements$cleaner, investments$cleaner,
                                start_year, vec_investment)
      vec_investment <- replace(replacements$dryer, investments$dryer,
                                start_year, vec_investment)
      vec_investment <- replace(replacements$cracker, (investments$cracker + investments$hopper + investments$blower),
                                start_year, vec_investment)
      vec_investment <- replace(replacements$storage, investments$storage,
                                start_year, vec_investment)
      vec_investment <- replace(replacements$calibration, investments$calibration,
                                start_year, vec_investment)
    }
    if (version == 2){
      vec_investment[start_year] <- sum(unlist(investments[1:7]))
      vec_investment <- replace(replacements$cleaner, investments$cleaner,
                                start_year, vec_investment)
      vec_investment <- replace(replacements$dryer, investments$dryer,
                                start_year, vec_investment)
      vec_investment <- replace(replacements$cracker, investments$hopper,
                                start_year, vec_investment)
      vec_investment <- replace(replacements$storage, investments$storage,
                                start_year, vec_investment)
      vec_investment <- replace(replacements$calibration, investments$calibration,
                                start_year, vec_investment)
    }
    # Working time post harvest
    vec_workingtime_cleaning <- vv(var_mean = workingtime_cleaning,
                                   var_CV = varCV,
                                   n = length)
    vec_workingtime_drying <- vv(var_mean = workingtime_drying,
                                 var_CV = 0,
                                 n = length)
    vec_workingtime_storing <- vv(var_mean = workingtime_storing,
                                  var_CV = 0,
                                  n = length)
    vec_workingtime_stabilization <- (vec_workingtime_cleaning + vec_workingtime_drying + vec_workingtime_storing) * vec_yield
    vec_yield_dryed <- vec_yield * (1 - drying_factor)
    
    if (version == 1){
      vec_inshell <- vec_yield_dryed * (1 - percentage_cracked)
      vec_cracking <- vec_yield_dryed * percentage_cracked
    }
    if (version == 2){
      vec_inshell <- vec_yield_dryed
      vec_cracking <- rep(0, length)
    } 
    if (version == 3){
      vec_inshell <- rep(0, length)
      vec_cracking <- vec_yield_dryed
    }
    
    vec_workingtime_cal_cracked <- vec_cracking * workingtime$calibration
    vec_workingtime_cracking <- vec_cracking * workingtime$cracking
    vec_workingtime_sorting_inshell <- vec_inshell * workingtime$sorting_inshell
    vec_inshell_sorted <- vec_inshell * (1 - sorting_losses$inshell)
    for (i in 1:length(pest)){
      if (pest[i] == 1) {
        vec_kernels[i] <- (vec_cracking[i] * cores) * (1 - sorting_losses$cracked_pest)
      }
      if (pest[i] == 0){
        vec_kernels[i] <- (vec_cracking[i] * cores) * (1 - sorting_losses$cracked_normal)
      }
    }
    if (version == 1){
      vec_workingtime_packaging <- (vec_inshell_sorted + vec_kernels) * workingtime$packaging
      vec_workingtime_cal_inshell <- rep(0, length)
    }
    if (version == 2) {
      vec_workingtime_packaging <- rep(0, length)
      vec_workingtime_cal_inshell <- vec_inshell * workingtime$calibration
    }
    if (version == 3){
      vec_workingtime_packaging <- rep(0, length)
      vec_workingtime_cal_inshell <- rep(0, length)
    }
    
    vec_shells <- vec_cracking * (1 - cores)
    
    #Summary post harvest working time
    vec_monthly_workingtime_postharvest[9,] <- vec_workingtime_stabilization /2
    vec_monthly_workingtime_postharvest[10,] <- vec_workingtime_stabilization /2
    
    vec_workingtime_rest <- vec_workingtime_cal_cracked + vec_workingtime_cracking + 
      vec_workingtime_sorting_inshell + vec_workingtime_packaging + vec_workingtime_cal_inshell
    
    vec_monthly_workingtime_postharvest[10,] <- vec_workingtime_rest / 4
    vec_monthly_workingtime_postharvest[11,] <- vec_workingtime_rest / 4
    vec_monthly_workingtime_postharvest[12,] <- vec_workingtime_rest / 4
    vec_monthly_workingtime_postharvest[1,] <- vec_workingtime_rest / 4
    
    return(list(investments = vec_investment,
                workingtime = vec_monthly_workingtime_postharvest,
                workingtime_processing = vec_workingtime_rest,
                cracked = vec_kernels,
                inshell = vec_inshell_sorted,
                shells = vec_shells,
                packaging = vec_workingtime_packaging))
  }
  
  #General ----
  
  #rounding years:
  first_harvest <- round(first_harvest)
  full_harvest <- round(full_harvest)
  replacement_time_harvester <- round(replacement_time_harvester)
  replacement_time_cleaner <- round(replacement_time_cleaner)
  replacement_time_dryer  <- round(replacement_time_dryer)
  replacement_time_cracker <- round(replacement_time_cracker)
  replacement_time_drip_irrigation <- round(replacement_time_drip_irrigation)
  replacement_time_pump <- round(replacement_time_pump)
  replacement_time_solarsystem <- round(replacement_time_solarsystem)
  
  # Universaly used values:
  total_trees <- number_grafted_hazelnut_trees + number_treehazel_trees
  vec_years_empty <- rep(0, years)
  vec_monthly_workingtime_empty <- matrix(0, nrow = 12, ncol = years, byrow = TRUE)
  
  # Implementation ----
  vec_investment_implementation <- vec_years_empty
  vec_monthly_workingtime_implementation <- vec_monthly_workingtime_empty
  
  # Planting, Fence building etc
  costs_hazelnut_trees <- (number_grafted_hazelnut_trees * cost_grafted_hazelnut_tree) +
    (number_treehazel_trees * cost_treehazel_tree)
  
  vec_investment_implementation[1] <- sum(c(grass_seeds,fence_material,
                                            cost_plantingposts,cost_auger,
                                            intitial_compost,cost_strings,
                                            cost_truffles_inocolation,
                                            registration_fee,
                                            costs_hazelnut_trees))

  vec_monthly_workingtime_implementation[1,1] <- sum(c((workingtime_planting * total_trees),
                                                       workingtime_fence,
                                                       workingtime_seeding,
                                                       workingtime_planning,
                                                       workingtime_gamedefence))
  
  # Orchard Care ----
  vec_investment_replacingtrees <- vec_years_empty
  vec_monthly_workingtime_replacingtrees <- vec_monthly_workingtime_empty
  
  vec_costs_mulching <- vec_years_empty
  vec_monthly_workingtime_mulching <- vec_monthly_workingtime_empty
  
  vec_costs_fertilization <- vec_years_empty
  
  vec_monthly_workingtime_pruning <- vec_monthly_workingtime_empty
  
  vec_monthly_workingtime_controlling <- vec_monthly_workingtime_empty
  
  # Land Lease
  vec_land_lease <- c(rep(land_lease_1, runtime_land_lease_1), rep(land_lease_2, (years - runtime_land_lease_1)))
  
  # Replacing dead trees
  vec_investment_replacingtrees[1] <- (replacing_trees * total_trees) * cost_grafted_hazelnut_tree
  vec_investment_replacingtrees[2] <- (replacing_trees * total_trees) * cost_grafted_hazelnut_tree
  vec_monthly_workingtime_replacingtrees[12,1] <- (replacing_trees * total_trees) * workingtime_planting
  vec_monthly_workingtime_replacingtrees[12,2] <- (replacing_trees * total_trees) * workingtime_planting
  
  # Mulching
  number_mulching <- vv(var_mean = 3, #steinwedel
                        var_CV = 50,
                        n = years,
                        lower_limit = number_mulching_min,
                        upper_limit = number_mulching_max)
  number_mulching <- round(number_mulching)
  
  for (i in 1:years) {
    if (number_mulching[i] == 2) {
      vec_monthly_workingtime_mulching[6,i] <- vec_monthly_workingtime_mulching[6,i] + workingtime_mulching
      vec_monthly_workingtime_mulching[9,i] <- vec_monthly_workingtime_mulching[9,i] + workingtime_mulching
      vec_costs_mulching[i] <- (2 * workingtime_mulching) * cost_tractor
      vec_monthly_workingtime_mulching[5,i] <- vec_monthly_workingtime_mulching[5,i] + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[7,i] <- vec_monthly_workingtime_mulching[7,i] + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[9,i] <- vec_monthly_workingtime_mulching[9,i] + workingtime_mulching_treestrip
    }
    if (number_mulching[i] == 3){
      vec_monthly_workingtime_mulching[5,i] <- vec_monthly_workingtime_mulching[5,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[7,i] <- vec_monthly_workingtime_mulching[7,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[9,i] <- vec_monthly_workingtime_mulching[9,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_costs_mulching[i] <- (3 * workingtime_mulching) * cost_tractor
    }
    if (number_mulching[i] == 4){
      vec_monthly_workingtime_mulching[4,i] <- vec_monthly_workingtime_mulching[4,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[6,i] <- vec_monthly_workingtime_mulching[6,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[8,i] <- vec_monthly_workingtime_mulching[8,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[9,i] <- vec_monthly_workingtime_mulching[9,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_costs_mulching[i] <- (4 * workingtime_mulching) * cost_tractor
    }
    if (number_mulching[i] == 5){
      vec_monthly_workingtime_mulching[4,i] <- vec_monthly_workingtime_mulching[4,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[6,i] <- vec_monthly_workingtime_mulching[6,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[8,i] <- vec_monthly_workingtime_mulching[8,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[9,i] <- vec_monthly_workingtime_mulching[9,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_monthly_workingtime_mulching[10,i] <- vec_monthly_workingtime_mulching[10,i] + workingtime_mulching + workingtime_mulching_treestrip
      vec_costs_mulching[i] <- (5 * workingtime_mulching) * cost_tractor
    }
  }
  
  # Fertilizer
  vec_costs_fertilization <- vv(var_mean = cost_fertilizer,
                                var_CV = 20,
                                n = years,
                                relative_trend = 2)
  
  vec_costs_fertilization <- vec_costs_fertilization * area
  
  # Pruning
  vec_workingtime_pruning <- gompertz_yield(max_harvest = workingtime_pruning,
                                            time_to_first_yield_estimate = 2, #erster Schnitt
                                            time_to_second_yield_estimate = 10,
                                            first_yield_estimate_percent = 60,
                                            second_yield_estimate_percent = 90,
                                            n_years = years,
                                            var_CV = 5)
  vec_monthly_workingtime_pruning[2,] <- (vec_workingtime_pruning * total_trees)/2
  vec_monthly_workingtime_pruning[3,] <- (vec_workingtime_pruning * total_trees)/2
  
  # Workingtime Controlling
  vec_monthly_workingtime_controlling[1:12,] <- workingtime_control
  
  # Summery Tree Care
  vec_costs_treecare <- vec_costs_fertilization + vec_costs_mulching + vec_land_lease
  vec_investment_treecare <- vec_investment_replacingtrees
  vec_monthly_workingtime_treecare <- vec_monthly_workingtime_replacingtrees + vec_monthly_workingtime_mulching + 
    vec_monthly_workingtime_pruning + vec_monthly_workingtime_controlling
  
  # Irrigation ----
  vec_costs_irrigation  <- rep(cost_water, years)
  vec_investment_irrigation <- vec_years_empty
  vec_monthly_workingtime_irrigation <- vec_monthly_workingtime_empty
  
  # Investments Irrigation
  vec_investment_irrigation[1] <- investment_drip_irrigation + investment_pump + investment_solarsystem
  vec_investment_irrigation <- replace(replacement_time_drip_irrigation,
                                       investment_drip_irrigation,
                                       0,
                                       vec_investment_irrigation)
  vec_investment_irrigation <- replace(replacement_time_pump,
                                       investment_pump,
                                       0,
                                       vec_investment_irrigation)
  vec_investment_irrigation <- replace(replacement_time_solarsystem,
                                       investment_solarsystem,
                                       0,
                                       vec_investment_irrigation,
                                       limit = 0)
  
  # Workingtime Irrigations
  vec_monthly_workingtime_irrigation[4,] <- vec_monthly_workingtime_irrigation[4,] + workingtime_control_irrigation
  vec_monthly_workingtime_irrigation[7,] <- vec_monthly_workingtime_irrigation[7,] + workingtime_control_irrigation
  vec_monthly_workingtime_irrigation[10,] <- vec_monthly_workingtime_irrigation[10,] + workingtime_winterizing_irrigation
  
  for (i in 3:10) { # Calculate monthly working time for operating the irrigation
    if (i == 3 || i == 4){mean_irri <- mean_irrigations_spring}
    if (i %in% 4:8){mean_irri <- mean_irrigations_summer}
    if (i == 9 || i == 10){mean_irri <- mean_irrigations_fall}
    vec_monthly_workingtime_irrigation <- monthly_workingtime_irrigation(mean_irri, i)
  }
  
  # Plant Protection ----
  vec_monthly_workingtime_plantprotection <- vec_monthly_workingtime_empty
  
  vec_pest_chance <- chance_event(chance = chance_pest, n = years)
  vec_pest_yield_reduction <- vv(var_mean = pest_yield_reduction_mean,
                                 var_CV = pest_varCV,
                                 n = years,
                                 lower_limit = pest_yield_reduction_lower,
                                 upper_limit = pest_yield_reduction_upper)
  
  vec_frost_chance <- chance_event(chance = chance_frost,
                                   n = years)
  vec_frost_yield_reduction <- vv(var_mean = frost_yield_reduction_mean,
                                  var_CV = frost_varCV,
                                  n = years,
                                  lower_limit = frost_yield_reduction_lower,
                                  upper_limit = frost_yield_reduction_upper)
  
  vec_yield_reduction_pest <- vec_pest_chance * vec_pest_yield_reduction 
  vec_yield_reduction_frost <- vec_frost_chance * vec_frost_yield_reduction
  vec_yield_reduction <- ifelse(vec_yield_reduction_pest == 0 & vec_yield_reduction_frost == 0, 0, 
                                ifelse(vec_yield_reduction_pest == 0, vec_yield_reduction_frost, 
                                       ifelse(vec_yield_reduction_frost == 0, vec_yield_reduction_pest, 
                                              vec_yield_reduction_pest * vec_yield_reduction_frost)))
  
  vec_costs_plantprotection <- c(rep(0, first_harvest), rep(fungus_prevention, (years - first_harvest)))
  vec_monthly_workingtime_plantprotection[1:12,] <- (prevention_workingtime / 12)
  
  # Harvest ----
  vec_costs_harvest <- vec_years_empty
  vec_investment_harvest <- vec_years_empty
  vec_monthly_workingtime_harvest <- vec_monthly_workingtime_empty
  
  # Yield:
  vec_hazelnut_yield <- gompertz_yield(max_harvest = hazelnut_yield,
                                       time_to_first_yield_estimate = first_harvest,
                                       time_to_second_yield_estimate = full_harvest,
                                       first_yield_estimate_percent = percentage_first_yield,
                                       second_yield_estimate_percent = 90,
                                       n_years = years,
                                       var_CV = 5)
  vec_hazelnut_yield_total <- (vec_hazelnut_yield * area) * (1 - vec_yield_reduction)
  vec_hazelnut_yield_total <- ifelse(vec_hazelnut_yield_total < min_harvest, 0, vec_hazelnut_yield_total)
  
  # Investment Harvester
  vec_investment_harvest[first_harvest] <- investment_harvester
  vec_investment_harvest <- replace(replacement_time_harvester, investment_harvester,
                                    first_harvest, vec_investment_harvest)
  
  # Workingtime Harvest
  workingtime_harvest <- workingtime_harvester * area
  vec_workingtime_harvest <- gompertz_yield(max_harvest = workingtime_harvest,
                                            time_to_first_yield_estimate = first_harvest,
                                            time_to_second_yield_estimate = full_harvest,
                                            first_yield_estimate_percent = 80,
                                            second_yield_estimate_percent = 100,
                                            n_years = years)
  vec_workingtime_harvest <- ifelse(vec_hazelnut_yield_total < min_harvest, 0, vec_workingtime_harvest)
  vec_monthly_workingtime_harvest[9,] <- (vec_workingtime_harvest / 2)
  vec_monthly_workingtime_harvest[10,] <- (vec_workingtime_harvest / 2)
  
  # Post Harvest ----
  
  post_harvest_investments <- list(cleaner = investment_cleaner, 
                                   dryer = investment_dryer, 
                                   storage = investment_storage, 
                                   calibration = investment_calibration,
                                   belt = investment_belt, 
                                   sortingtable = investment_sortingtable,
                                   hopper = investment_hopper, 
                                   blower = investment_blower,
                                   cracker = investment_cracker)
  post_harvest_replacements <- list(cleaner = replacement_time_cleaner, 
                                    dryer = replacement_time_dryer, 
                                    cracker = replacement_time_cracker, 
                                    storage = replacement_time_storage, 
                                    calibration = replacement_time_calibration)
  post_harvest_workingtimes <- list(cleaning = workingtime_cleaning, 
                                    drying = workingtime_drying, 
                                    storing = workingtime_storing, 
                                    calibration = workingtime_calibration, 
                                    cracking = workingtime_cracking, 
                                    sorting_inshell = workingtime_sorting_inshell, 
                                    packaging = workingtime_packaging)
  sorting_losses_nuts <- list(inshell = sorting_losses_inshell, 
                              cracked_normal = sorting_losses_cracked_normal,
                              cracked_pest = sorting_losses_cracked_pests)
  
  # 3 Version: 
  # A: Partly Cracked, direct Sales
  # B: All In-Shell, sold to retailer
  # C: All cracked, sold to retailer
  
  post_harvest_1 <- post_harvest(1)
  post_harvest_2 <- post_harvest(2)
  post_harvest_3 <- post_harvest(3)
  
  # Income ----
  
  vec_land_subvention <- rep((land_subvention*area), years)
  
  # Selling nuts:
  # Version 1:
  vec_income_inshell_1 <- (post_harvest_1$inshell * 1000) * price_inshell_1
  vec_income_cracked_1 <- (post_harvest_1$cracked * 1000) * price_cracked_1
  vec_income_shells_1 <- (post_harvest_1$shells * 1000) * price_shells
  
  #Version 2
  vec_income_inshell_2 <- (post_harvest_2$inshell * 1000) * price_inshell_2
  
  #Version 3
  vec_income_cracked_3 <- (post_harvest_3$cracked * 1000) * price_cracked_3
  vec_income_shells_3 <- ((post_harvest_3$shells * (1 - core_content)) * 1000) * price_shells
  

  # Summery: ----
  
  # Version 1: ----
  # Yearly 1:
  vec_costs_total_1 <- vec_costs_treecare + vec_costs_plantprotection + vec_costs_irrigation + vec_costs_harvest
  vec_investment_total_1 <- vec_investment_implementation + vec_investment_treecare + vec_investment_irrigation + 
    vec_investment_harvest + post_harvest_1$investments
  
  vec_monthly_workingtime_total_1 <- vec_monthly_workingtime_implementation + vec_monthly_workingtime_treecare + 
    vec_monthly_workingtime_irrigation + vec_monthly_workingtime_plantprotection + 
    vec_monthly_workingtime_harvest + post_harvest_1$workingtime
  
  vec_yearly_workingtime_total_1 <- colSums(vec_monthly_workingtime_total_1) #per year
  vec_mean_monthly_working_hours_total_1 <- rowMeans(vec_monthly_workingtime_total_1[,-(1)]) # mean per month without year of planting
  vec_mean_monthly_working_hours_fullyield_total_1 <-rowMeans(vec_monthly_workingtime_total_1[,full_harvest:years]) #mean per month only full yield
  vec_costs_workingtime_total_1 <- vec_yearly_workingtime_total_1 * salary
  
  vec_expenses_total_1 <- vec_costs_total_1 + vec_investment_total_1 + vec_costs_workingtime_total_1
  vec_income_total_1 <- vec_land_subvention + vec_income_inshell_1 + vec_income_cracked_1 + vec_income_shells_1
  
  vec_total_1 <- vec_income_total_1 - vec_expenses_total_1
  
  # Sum 1
  costs_total_1 <- sum(vec_costs_total_1)
  investment_total_1 <- sum(vec_investment_total_1)
  costs_workingtime_total_1 <- sum(vec_costs_workingtime_total_1)
  expenses_total_1 <- sum(vec_expenses_total_1)
  income_total_1 <- sum(vec_income_total_1)
  total_1 <- sum(vec_total_1)
  
  min_price_inshell_1 <- (expenses_total_1 / (sum(post_harvest_1$cracked)*1000)) / (ratio_price_1 + ((sum(post_harvest_1$inshell)*1000)/(sum(post_harvest_1$cracked)*1000)))
  min_price_cracked_1 <- ratio_price_1 * min_price_inshell_1
  
  # Version 2: ----
  # Yearly 2:
  vec_costs_total_2 <- vec_costs_treecare + vec_costs_plantprotection + vec_costs_irrigation + vec_costs_harvest
  vec_investment_total_2 <- vec_investment_implementation + vec_investment_treecare + vec_investment_irrigation + 
    vec_investment_harvest + post_harvest_2$investments
  
  vec_monthly_workingtime_total_2 <- vec_monthly_workingtime_implementation + vec_monthly_workingtime_treecare + 
    vec_monthly_workingtime_plantprotection + vec_monthly_workingtime_irrigation + 
    vec_monthly_workingtime_harvest + post_harvest_2$workingtime
  vec_yearly_workingtime_total_2 <- colSums(vec_monthly_workingtime_total_2) #per year
  vec_mean_monthly_working_hours_total_2 <- rowMeans(vec_monthly_workingtime_total_2[,-(1)]) # mean per month without year of planting
  vec_mean_monthly_working_hours_fullyield_total_2 <-rowMeans(vec_monthly_workingtime_total_2[,-(1:full_harvest)]) #mean per month only full yield
  vec_costs_workingtime_total_2 <- vec_yearly_workingtime_total_2 * salary
  
  vec_expenses_total_2 <- vec_costs_total_2 + vec_investment_total_2 + vec_costs_workingtime_total_2
  vec_income_total_2 <- vec_land_subvention + vec_income_inshell_2
  
  vec_total_2 <- vec_income_total_2 - vec_expenses_total_2
  
  # Sum 2
  costs_total_2 <- sum(vec_costs_total_2)
  investment_total_2 <- sum(vec_investment_total_2)
  costs_workingtime_total_2 <- sum(vec_costs_workingtime_total_2)
  expenses_total_2 <- sum(vec_expenses_total_2)
  income_total_2 <- sum(vec_income_total_2)
  total_2 <- sum(vec_total_2)
  
  min_price_2 <- expenses_total_2 / (sum(post_harvest_2$inshell) * 1000)
  
  # Version 3: ----
  # Yearly 3:
  vec_costs_total_3 <- vec_costs_treecare + vec_costs_plantprotection + vec_costs_irrigation + vec_costs_harvest 
  vec_investment_total_3 <- vec_investment_implementation + vec_investment_treecare + vec_investment_irrigation + 
    vec_investment_harvest + post_harvest_3$investments
  
  vec_monthly_workingtime_total_3 <- vec_monthly_workingtime_implementation + vec_monthly_workingtime_treecare + 
    vec_monthly_workingtime_plantprotection + vec_monthly_workingtime_irrigation + 
    vec_monthly_workingtime_harvest + post_harvest_3$workingtime
  vec_yearly_workingtime_total_3 <- colSums(vec_monthly_workingtime_total_3) #per year
  vec_mean_monthly_working_hours_total_3 <- rowMeans(vec_monthly_workingtime_total_3[,-(1)]) # mean per month without year of planting
  vec_mean_monthly_working_hours_fullyield_total_3 <-rowMeans(vec_monthly_workingtime_total_3[,-(1:full_harvest)]) #mean per month only full yield
  vec_costs_workingtime_total_3 <- vec_yearly_workingtime_total_3 * salary
  
  vec_expenses_total_3 <- vec_costs_total_3 + vec_investment_total_3 + vec_costs_workingtime_total_3
  vec_income_total_3 <- vec_land_subvention + vec_income_cracked_3 + vec_income_shells_3
  
  vec_total_3 <- vec_income_total_3 - vec_expenses_total_3
  
  # Sum 3
  costs_total_3 <- sum(vec_costs_total_3)
  investment_total_3 <- sum(vec_investment_total_3)
  costs_workingtime_total_3 <- sum(vec_costs_workingtime_total_3)
  expenses_total_3 <- sum(vec_expenses_total_3)
  income_total_3 <- sum(vec_income_total_3)
  total_3 <- sum(vec_total_3)
  
  min_price_3 <- expenses_total_3 / (sum(post_harvest_3$cracked) * 1000)
  
  # Additional values to return----
  mean_yearly_yield_reduction_pest <- 100 * mean(vec_yield_reduction_pest) # Or only value of one certain year?
  mean_yearly_yield_total <- mean(vec_hazelnut_yield_total[full_harvest:years])
  mean_yearly_yield_bp <- mean(vec_hazelnut_yield[full_harvest:years] * area)
  mean_yearly_yield_cracked_1 <- mean(post_harvest_1$cracked[full_harvest:years])
  mean_yearly_yield_inshell_1 <- mean(post_harvest_1$inshell[full_harvest:years])
  mean_yearly_yield_shells_1 <- mean(post_harvest_1$shells[full_harvest:years])
  mean_yearly_yield_inshell_2 <- mean(post_harvest_2$inshell[full_harvest:years])
  mean_yearly_yield_cracked_3 <- mean(post_harvest_3$cracked[full_harvest:years])
  mean_yearly_yield_shells_3 <- mean(post_harvest_3$shells[full_harvest:years])
  mean_yearly_workingtime_irrigation <- mean(colSums(vec_monthly_workingtime_irrigation)[full_harvest:years])
  mean_yearly_workingtime_pruning <- mean(colSums(vec_monthly_workingtime_pruning)[full_harvest:years])
  mean_yearly_workingtime_mulching <- mean(colSums(vec_monthly_workingtime_mulching)[full_harvest:years])
  mean_yearly_workingtime_harvest <- mean(colSums(vec_monthly_workingtime_harvest)[full_harvest:years])
  mean_yearly_workingtime_plantprotection <- mean(colSums(vec_monthly_workingtime_plantprotection)[full_harvest:years])
  mean_yearly_workingtime_controlling <- mean(colSums(vec_monthly_workingtime_controlling)[full_harvest:years])
  mean_yearly_workingtime_plantationcare <- mean_yearly_workingtime_controlling + mean_yearly_workingtime_plantprotection+
    mean_yearly_workingtime_mulching + mean_yearly_workingtime_pruning + mean_yearly_workingtime_irrigation
  mean_yearly_workingtime_processing_1 <- mean(post_harvest_1$workingtime_processing[full_harvest:years])
  mean_yearly_workingtime_stabilization_1 <- mean(colSums(post_harvest_1$workingtime)[full_harvest:years] - post_harvest_1$workingtime_processing[full_harvest:years])
  mean_yearly_workingtime_processing_2 <- mean(post_harvest_2$workingtime_processing[full_harvest:years])
  mean_yearly_workingtime_processing_3 <- mean(post_harvest_3$workingtime_processing[full_harvest:years])
  mean_yearly_workingtime_postharvest_1 <- mean(colSums(post_harvest_1$workingtime)[full_harvest:years])
  mean_yearly_workingtime_postharvest_2 <- mean(colSums(post_harvest_2$workingtime)[full_harvest:years])
  mean_yearly_workingtime_postharvest_3 <- mean(colSums(post_harvest_3$workingtime)[full_harvest:years])
  initial_expenses_2 <- sum(vec_expenses_total_2[1:first_harvest]) - sum(vec_land_subvention[1:first_harvest])
  initial_expenses_nowork_2 <- initial_expenses_2 - sum(vec_costs_workingtime_total_2[1:first_harvest])
  initial_expenses_work_2 <- sum(vec_costs_workingtime_total_2[1:first_harvest])
  initial_workingtime_2 <- sum(vec_yearly_workingtime_total_2[1:first_harvest])
  investments_fixed_2 <- sum(vec_investment_implementation+ vec_investment_treecare + vec_investment_irrigation)
  investments_flexibel_2 <- sum(vec_investment_harvest + post_harvest_2$investments)
  mean_yearly_workingtime_postharvest_1a <- mean(colSums(post_harvest_1$workingtime)[full_harvest:years]) -
    mean(post_harvest_1$packaging[full_harvest:years])
  mean_yearly_workingtime_postharvest_1_perton <- mean_yearly_workingtime_postharvest_1 / mean(vec_hazelnut_yield_total[full_harvest:years])
  mean_yearly_workingtime_postharvest_2_perton <- mean_yearly_workingtime_postharvest_2 / mean(vec_hazelnut_yield_total[full_harvest:years])
  mean_yearly_workingtime_postharvest_3_perton <- mean_yearly_workingtime_postharvest_3 / mean(vec_hazelnut_yield_total[full_harvest:years])
  mean_investments_2 <- mean(vec_investment_total_2)
  mean_costs_2 <- mean(vec_costs_total_2)
  mean_workingcosts_2 <- mean(vec_costs_workingtime_total_2)
  vec_sorting_losses_cracked <- vec_years_empty
  for (i in 1:length(vec_pest_chance)){
    if (vec_pest_chance[i] == 1){vec_sorting_losses_cracked[i] <- sorting_losses_cracked_pests}
    if (vec_pest_chance[i] == 0){vec_sorting_losses_cracked[i] <- sorting_losses_cracked_normal}}
  mean_yield_reduction_sorting_cracked_1 <- mean(vec_sorting_losses_cracked) * 100
  mean_yield_reduction_sorting_inshell_1 <- sorting_losses_inshell * 100
  
  income_per_yield_1 <- mean(vec_income_total_1[full_harvest:years]/(1000 * vec_hazelnut_yield_total[full_harvest:years]))
  income_per_yield_2 <- mean(vec_income_total_2[full_harvest:years]/(1000 * vec_hazelnut_yield_total[full_harvest:years]))
  income_per_yield_3 <- mean(vec_income_total_3[full_harvest:years]/(1000 * vec_hazelnut_yield_total[full_harvest:years]))
  workingtime_per_yield_1 <- mean(vec_yearly_workingtime_total_1[full_harvest:years]/(1000 * vec_hazelnut_yield_total[full_harvest:years]))
  workingtime_per_yield_2 <- mean(vec_yearly_workingtime_total_2[full_harvest:years]/(1000 * vec_hazelnut_yield_total[full_harvest:years]))
  workingtime_per_yield_3 <- mean(vec_yearly_workingtime_total_3[full_harvest:years]/(1000 * vec_hazelnut_yield_total[full_harvest:years]))
  expenses_per_yield_1 <- mean(vec_expenses_total_1[full_harvest:years]/(1000 * vec_hazelnut_yield_total[full_harvest:years]))
  expenses_per_yield_2 <- mean(vec_expenses_total_2[full_harvest:years]/(1000 * vec_hazelnut_yield_total[full_harvest:years]))
  expenses_per_yield_3 <- mean(vec_expenses_total_3[full_harvest:years]/(1000 * vec_hazelnut_yield_total[full_harvest:years]))
  
  
  # return ----
  return(list(total_b = total_1,
              total_a = total_2,
              total_c = total_3,
              costs_total_b = costs_total_1,
              costs_total_a = costs_total_2,
              costs_total_c = costs_total_3,
              investment_total_b = investment_total_1,
              investment_total_a = investment_total_2,
              investment_total_c = investment_total_3,
              costs_workingtime_total_b = costs_workingtime_total_1,
              costs_workingtime_total_a = costs_workingtime_total_2,
              costs_workingtime_total_c = costs_workingtime_total_3,
              expenses_total_b = expenses_total_1,
              expenses_total_a = expenses_total_2,
              expenses_total_c = expenses_total_3,
              income_total_b = income_total_1,
              income_total_a = income_total_2,
              income_total_c = income_total_3,
              vec_total_b = vec_total_1,
              vec_total_a = vec_total_2,
              vec_total_c = vec_total_3,
              vec_income_total_b = vec_income_total_1,
              vec_income_total_a = vec_income_total_2,
              vec_income_total_c = vec_income_total_3,
              vec_expenses_total_b = vec_expenses_total_1,
              vec_expenses_total_a = vec_expenses_total_2,
              vec_expenses_total_c = vec_expenses_total_3,
              vec_costs_total_b = vec_costs_total_1,
              vec_costs_total_a = vec_costs_total_2,
              vec_costs_total_c = vec_costs_total_3,
              vec_investment_total_b = vec_investment_total_1,
              vec_investment_total_a = vec_investment_total_2,
              vec_investment_total_c = vec_investment_total_3,
              vec_mean_monthly_working_b = vec_mean_monthly_working_hours_fullyield_total_1,
              vec_mean_monthly_working_a = vec_mean_monthly_working_hours_fullyield_total_2,
              vec_mean_monthly_working_c = vec_mean_monthly_working_hours_fullyield_total_3,
              vec_yearly_workingtime_total_b = vec_yearly_workingtime_total_1,
              vec_yearly_workingtime_total_a = vec_yearly_workingtime_total_2,
              vec_yearly_workingtime_total_c = vec_yearly_workingtime_total_3,
              vec_hazelnut_yield_total = vec_hazelnut_yield_total,
              vec_inshell_yield_b = post_harvest_1$inshell,
              vec_cracked_yield_b = post_harvest_1$cracked,
              vec_inshell_yield_a = post_harvest_2$inshell,
              vec_cracked_yield_c = post_harvest_3$cracked,
              vec_yield_reduction_pest = vec_yield_reduction_pest,
              mean_yield_reduction_pest = mean_yearly_yield_reduction_pest,
              mean_yield_reduction_sorting_cracked_b = mean_yield_reduction_sorting_cracked_1,
              mean_yield_reduction_sorting_inshell_b = mean_yield_reduction_sorting_inshell_1,
              mean_yearly_yield_total = mean_yearly_yield_total,
              mean_yearly_yield_bp = mean_yearly_yield_bp,
              mean_yearly_yield_cracked_b = mean_yearly_yield_cracked_1,
              mean_yearly_yield_inshell_b = mean_yearly_yield_inshell_1,
              mean_yearly_yield_shells_b = mean_yearly_yield_shells_1,
              mean_income_inshell_b = mean(vec_income_inshell_1[full_harvest:years]),
              mean_income_cracked_b = mean(vec_income_cracked_1[full_harvest:years]),
              mean_income_shells_b = mean(vec_income_shells_1[full_harvest:years]),
              mean_yearly_yield_inshell_a = mean_yearly_yield_inshell_2,
              mean_yearly_yield_cracked_c = mean_yearly_yield_cracked_3,
              mean_yearly_yield_shells_c = mean_yearly_yield_shells_3,
              mean_yearly_workingtime_irrigation = mean_yearly_workingtime_irrigation,
              mean_yearly_workingtime_pruning = mean_yearly_workingtime_pruning,
              mean_yearly_workingtime_mulching = mean_yearly_workingtime_mulching,
              mean_yearly_workingtime_controlling = mean_yearly_workingtime_controlling,
              mean_yearly_workingtime_plantprotection = mean_yearly_workingtime_plantprotection,
              mean_yearly_workingtime_plantationcare = mean_yearly_workingtime_plantationcare,
              mean_yearly_workingtime_harvest = mean_yearly_workingtime_harvest,
              mean_yearly_workingtime_stabilization_b = mean_yearly_workingtime_stabilization_1,
              mean_yearly_workingtime_harvest_stabilization = mean_yearly_workingtime_stabilization_1 + mean_yearly_workingtime_harvest,
              mean_yearly_workingtime_processing_b = mean_yearly_workingtime_processing_1,
              mean_yearly_workingtime_processing_a = mean_yearly_workingtime_processing_2,
              mean_yearly_workingtime_processing_c = mean_yearly_workingtime_processing_3,
              mean_yearly_workingtime_postharvest_b = mean_yearly_workingtime_postharvest_1,
              mean_yearly_workingtime_postharvest_a = mean_yearly_workingtime_postharvest_2,
              mean_yearly_workingtime_postharvest_c = mean_yearly_workingtime_postharvest_3,
              mean_yearly_workingtime_postharvest_ba = mean_yearly_workingtime_postharvest_1a,
              mean_yearly_workingtime_postharvest_b_perton = mean_yearly_workingtime_postharvest_1_perton,
              mean_yearly_workingtime_postharvest_a_perton = mean_yearly_workingtime_postharvest_2_perton,
              mean_yearly_workingtime_postharvest_c_perton = mean_yearly_workingtime_postharvest_3_perton,
              mean_investments_a = mean_investments_2,
              mean_costs_a = mean_costs_2,
              mean_workingcosts_a = mean_workingcosts_2,
              initial_expenses_total_a = initial_expenses_2,
              initial_expenses_nowork_a = initial_expenses_nowork_2,
              initial_expenses_work_a = initial_expenses_work_2,
              investments_fixed_a = investments_fixed_2,
              investments_flexibel_a = investments_flexibel_2,
              income_per_yield_b = income_per_yield_1,
              income_per_yield_a = income_per_yield_2,
              income_per_yield_c = income_per_yield_3,
              workingtime_per_yield_b = workingtime_per_yield_1,
              workingtime_per_yield_a = workingtime_per_yield_2,
              workingtime_per_yield_c = workingtime_per_yield_3,
              expenses_per_yield_b = expenses_per_yield_1,
              expenses_per_yield_a = expenses_per_yield_2,
              expenses_per_yield_c = expenses_per_yield_3,
              min_price_inshell_b = min_price_inshell_1,
              min_price_cracked_b = min_price_cracked_1,
              min_price_a = min_price_2,
              min_price_c = min_price_3))
}


# Monte Carlo ----
# Run the Monte Carlo simulation using the model function
model_function_final <- mcSimulation(
  estimate = as.estimate(input_estimates),
  model_function = model_steinwedel_final,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)






