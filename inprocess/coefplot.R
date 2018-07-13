##


resglm <- glm.uni(y = "am",
                  var2test = c("mpg","cyl","disp","hp","wt","qsec","vs" ) ,
                  data = mtc_bis, format = "latex", size = 10)
df <- resglm$unimod_ci_df

bk <- seq(-10,10, by = .5)
ggplot(df, aes(x = varlev, y = OR)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = UpperIC, ymin = LowerIC)) +
  scale_y_continuous(breaks = bk, labels = round(exp(bk),2),
                     name = "log (Hazard Ratio)", limits = c(-0.004,0.003))

confint(resglm$unimod_list$mpg)
coef(resglm$unimod_list$mpg)
tabOR_lr(resglm$unimod_list$mpg)

F <- runif(10,1,2)
L <- runif(10,0,1)
U <- runif(10,2,3)

inst.all("plotrix")
plotCI(1:nrow(df),df$OR, ui = df$UpperIC, li = df$LowerIC)


ggplot(df, aes(x = varlev, y = log(OR)))+
  geom_hline(aes(yintercept = 0), colour = "#F8766D") +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = log(UpperIC), ymin = log(LowerIC))) + coord_flip()+
  scale_y_continuous(breaks = bk, labels = round(exp(bk),2),
                     name = "OR")



















## coefplot

require(mmotaF)
data("aSAH")
dat <- aSAH

resglm <- stepLR("outcome", c("gender", "age", "wfns","s100b", "ndka"),
                 data = dat, trace = F)[[2]]


mod0 <- glm(outcome ~ s100b, data = dat, family = "binomial" )
coeff <- coefplot(mod0, add = T)

mod <- glm(outcome ~ gender + age + wfns, data = dat, family = "binomial" )
tabOR_lr(mod)
require(coefplot)
coeff <- coefplot(mod)

bk <- seq(-10,10, by = 2)
coeff +
  scale_x_continuous(breaks = bk, labels = round(exp(bk),2), name = "exp(coef)")


coeff + theme_bw() +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title.y  = element_text(size = 16)) +
  theme(axis.title.x = element_text(size = 16)) +
  scale_y_discrete(name = "",labels = c("NDAA","GAP","SS","PS","LL")) +
  theme(axis.text.x  = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 16)) +
  scale_x_continuous(name = "Regression Estimate", limits = c(log(-4), log(4))) +
  labs(title = "") +
  coord_flip()

uniglm <- glm.uni("outcome", var2test = c("gender", "age", "wfns","s100b", "ndka"),
                  data = dat)$unimod_list

unimod <- lapply(c("gender", "age", "wfns","s100b", "ndka"),
                 function(var) {

                   formula <- as.formula(paste("outcome ~", var))
                   glm(formula, data = dat, family = binomial)
                 })
names(unimod) <- c("gender", "age", "wfns","s100b", "ndka")

mul <- multiplot(unimod)
bk <- seq(-10,10, by = 2)
mul +
  scale_x_continuous(breaks = bk, labels = round(exp(bk),2), name = "OR") +
  scale_fill_manual(labels = names(unimod))

