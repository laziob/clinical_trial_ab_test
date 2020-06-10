library("gsDesign")

agile_sim = function(p_a, alpha, beta) {
  muestras_agile = seq(1:100)
  z_total = seq(1:100)
  lifts = seq(1:100)
  muestras_fixed = seq(1:100)
  power_4_test = 1 - beta
  for(a in 1:100) {
    mei = runif(1,min=0, max=0.25)
    p_b = sample(c(p_a/(1+mei), p_a*(1+mei)),1)
    lifts[a] = (p_b-p_a)/p_a
    power_test_fixed_sample = power.prop.test(p1=p_a, p2=p_b, sig.level = alpha, power = power_4_test, alternative = "one.sided")
    muestras_fixed[a] = power_test_fixed_sample$n
    testDesign = gsDesign(k=5, test.type = 4,alpha = alpha, beta = beta, delta = 0, n.fix = power_test_fixed_sample$n, sfu = sfPower, sfupar = 3, sfl = sfPower, sflpar = 2)
    for (i in 1:testDesign$k) {
      base = rbinom(round(testDesign$n.I[i]), size=1, prob = p_a)
      test = rbinom(round(testDesign$n.I[i]), size=1, prob = p_b)
      p1 = sum(base)/length(base)
      n1 = length(base)
      p2 = sum(test)/length(test)
      n2 = length(test)
      p_pooled = (p1*n1 + p2*n2)/(n1 + n2)
      dev_pooled = sqrt(p_pooled*(1-p_pooled)*((1/n1)+(1/n2)))
      z = (p2-p1)/dev_pooled  
      if(z > testDesign$upper$bound[i] | z < testDesign$lower$bound[i]) {
        #print(paste("Experimento terminó en interim", i, "con un Z score crítico de", zscore[i], "y una muestra total de", testDesign$n.I[i], sep = " "))
        muestras_agile[a] = as.numeric(testDesign$n.I[i])
        z_total[a] = z
        #test_all <- rbind(p$data, results)
        #full_plot <- ggplot(data = test_all, aes(x=N, y=Z, group=Bound)) + geom_line(aes(color=Bound)) + geom_point() + geom_text(aes(label=Ztxt), vjust="outward")
        break
      }
    }
  }
  df_results = data.frame("Experimento"=seq(1:100), "Muestras"=muestras, "Z_score"=z_total, "Fixed Sample"=power_test_fixed_sample$n, "True Lift"=lifts, "Savings"=(muestras/power_test_fixed_sample$n))  
  return(df_results)
}

agile_sim_plot = function(p_a, min_lift, alpha, beta, hay_efecto) {
  power_4_test = 1 - beta
  p_b = p_a * (1+min_lift)
  power_test_fixed_sample = power.prop.test(p1=p_a, p2=p_b, sig.level = alpha, power = power_4_test, alternative = "one.sided")
  testDesign = gsDesign(k=5, test.type = 4,alpha = alpha, beta = beta, delta = 0, n.fix = power_test_fixed_sample$n, sfu = sfPower, sfupar = 3, sfl = sfPower, sflpar = 2)
  p = plot(testDesign)
  zscore = numeric()
  samples = numeric()
  if(hay_efecto != TRUE) {
    p_b = p_a
  }
  for (i in 1:testDesign$k) {
    base = rbinom(round(testDesign$n.I[i]), size=1, prob = p_a)
    test = rbinom(round(testDesign$n.I[i]), size=1, prob = p_b)
    p1 = sum(base)/length(base)
    n1 = length(base)
    p2 = sum(test)/length(test)
    n2 = length(test)
    p_pooled = (p1*n1 + p2*n2)/(n1 + n2)
    dev_pooled = sqrt(p_pooled*(1-p_pooled)*((1/n1)+(1/n2)))
    z = (p2-p1)/dev_pooled  
    zscore[i] = z
    samples[i] = testDesign$n.I[i]
    if(z > testDesign$upper$bound[i] | z < testDesign$lower$bound[i]) {
      #print(paste("Experimento terminó en interim", i, "con un Z score crítico de", zscore[i], "y una muestra total de", testDesign$n.I[i], sep = " "))
      results = data.frame("N"=samples, "Z"=zscore, "Bound"="Real", "Ztxt" = factor(round(zscore,2)))
      test_all <- rbind(p$data, results)
      full_plot <- ggplot(data = test_all, aes(x=N, y=Z, group=Bound)) + 
        ggtitle("Real Z_Score progression and its boundaries") +
        geom_line(aes(color=Bound)) + 
        geom_point() + 
        geom_text(aes(label=Ztxt), vjust="top", nudge_y = 0.3)
      break
    }
  }
  return(full_plot)
}

fixed_sim = function(p_a, min_lift, alpha, beta, hay_efecto) {
  power_4_test = 1 - beta
  p_b = p_a * (1+min_lift)
  power_test_fixed_sample = power.prop.test(p1=p_a, p2=p_b, sig.level = alpha, power = power_4_test, alternative = "one.sided")
  muestras = seq(1:1000)
  z_total = seq(1:1000)
  p_diff = seq(1:1000)
  if(hay_efecto != TRUE) {
    p_b = p_a
  }
  for(a in 1:1000) {
    zscore = numeric()
    samples = numeric()
    base = rbinom(round(power_test_fixed_sample$n), size=1, prob = p_a)
    test = rbinom(round(power_test_fixed_sample$n), size=1, prob = p_b)
    p1 = sum(base)/length(base)
    n1 = length(base)
    p2 = sum(test)/length(test)
    n2 = length(test)
    abs_diff = p2-p1
    p_pooled = (p1*n1 + p2*n2)/(n1 + n2)
    dev_pooled = sqrt(p_pooled*(1-p_pooled)*((1/n1)+(1/n2)))
    z = (p2-p1)/dev_pooled  
    z_total[a] = z
    muestras[a] = round(power_test_fixed_sample$n)
    p_diff[a] = round(abs_diff,5)
  }
  df_results = data.frame("Experimento"=seq(1:n_sims), "Muestras"=muestras, "Abs_Diff"=p_diff , "Z_score"=z_total)  
  return(df_results)
}

fixed_sim_plot = function(p_a, min_lift, alpha, beta, hay_efecto) {
  power_4_test = 1 - beta
  p_b = p_a * (1+min_lift)
  power_test_fixed_sample = power.prop.test(p1=p_a, p2=p_b, sig.level = alpha, power = power_4_test, alternative = "one.sided")
  if(hay_efecto != TRUE) {
    p_b = p_a
  }
  base = rbinom(round(power_test_fixed_sample$n), size=1, prob = p_a)
  test = rbinom(round(power_test_fixed_sample$n), size=1, prob = p_b)
  p1 = sum(base)/length(base)
  n1 = length(base)
  p2 = sum(test)/length(test)
  n2 = length(test)
  abs_diff = p2-p1
  p_pooled = (p1*n1 + p2*n2)/(n1 + n2)
  dev_pooled = sqrt(p_pooled*(1-p_pooled)*((1/n1)+(1/n2)))
  z = (p2-p1)/dev_pooled
  z_crit = round(qnorm(1-alpha),2)
  plot_fixed <- ggplot(NULL, aes(c(-4,4))) +
    ggtitle(paste("Fixed Sample Test con Tamaño Muestral Total de",round(power_test_fixed_sample$n), sep=" ")) +
    geom_area(stat = "function", fun = dnorm, fill = "#00998a", xlim = c(-4, z_crit)) +
    geom_area(stat = "function", fun = dnorm, fill = "grey80", xlim = c(z_crit, 4)) +
    geom_vline(aes(xintercept=z, colour="red")) +
    labs(x = "z", y = "") +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = c(z,z_crit)) +
    theme(legend.position = "none")
  return(plot_fixed)
}
