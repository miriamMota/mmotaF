library(survival)
data(lung)
dat <- lung
dat$sex <- factor(dat$sex)
dat$ph.ecog <- factor(dat$ph.ecog)

m_log <- glm(sex~ wt.loss + ph.ecog +ph.karno, data = dat, family = binomial)
tabOR_lr(m_log, xtab = T,title='OR de los coeficientes')



mcox <- coxph(Surv(time, status) ~ sex, data = lung)
inst.all("papeR")
print(kable(prettify(summary(mcox), digits = 4)[,c(" ", "Hazard Ratio", "CI (lower)",   "CI (upper)","Pr(>|z|)" )], booktabs = T,
            caption = paste0( "Hazard Ratio Cox " ,". Surv"), longtable = TRUE,  escape = F) %>%
        kable_styling(latex_options = c("striped","hold_position", "repeat_header"), font_size = 12) %>%
        row_spec(0,background = "#993489", color = "white") )
tabOR_lr(glm(sex~ wt.loss, data = dat, family = binomial))
