library(mvtnorm)
library(tidyverse)

set.seed(104)


# data generation ----------------
mean.b = c(2,2)
Sig.b = 1^2*diag(2)
N = 10 + 1

param = round(rmvnorm(N, mean = mean.b, sigma = Sig.b),2)
param[N,] = mean.b 

x <- c(0:3) # 4 time points

dat <- cross_df(list(time=x, y=c(1:N))) %>%
  mutate(resp = param[y,1] + param[y,2]*x)


# ggplot ----------------
ggplot(data=dat %>% filter(y<N), aes(x=time, y =resp, group=y)) + 
  geom_line(size=1,linetype="solid", color="grey50") +
  geom_line(data = dat %>% filter(y==N), aes(x=time, y =resp), size = 1.5, color = "tomato") +
  theme_bw() +
  theme(text = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  # geom_abline(intercept = 2, slope = 2, color="blue", size=1) +
  scale_x_continuous(name="", breaks=c(0,1,2,3),
                     labels=sprintf("Time%s", seq(1:4))) +
  scale_y_continuous(name="", breaks=seq(0,12,by=2)) +
  geom_text(x=0.7, y=11, label="individual linear growth", color="grey50", size=5) +
  geom_text(x=0.7, y=10, label="average linear growth", color="tomato", size=5)
# ggsave("unconditional model/multiple_lines.pdf", width = 5, height = 4)
# ggsave("unconditional model/multiple_lines.png", width = 5, height = 4)
