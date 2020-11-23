library(tidyverse)


x <- c(0,1,2,3)
mu <- 1.5*x + 1

set.seed(8844)
resp = round(rnorm(4,mu, 1),2)
dat = tibble(time = c(0:3),
             resp = resp,
             mu = mu)


ggplot(data=dat, aes(x=time, y=resp)) + 
  geom_point(shape=21, size = 3, fill="black") +
  geom_line(size=1,linetype="dashed", color="grey50") +
  theme_bw() +
  theme(text = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14)) +
  geom_abline(intercept = 1, slope = 1.5, color="blue", size=1) +
  geom_point(aes(x=time, y = mu), size = 3, shape=22, fill="blue3") +
  scale_x_continuous(name="", breaks=c(0,1,2,3),
                     labels=sprintf("Time%s", seq(1:4))) +
  scale_y_continuous(name="", breaks=c(0,1,2,3,4,5,6)) +
  geom_segment(aes(x = 0, y = resp[1], xend = 0, yend = mu[1]), col="tomato", size = 1) +
  geom_segment(aes(x = 1, y = resp[2], xend = 1, yend = mu[2]), col="tomato", size = 1) +
  geom_segment(aes(x = 2, y = resp[3], xend = 2, yend = mu[3]), col="tomato", size = 1) +
  geom_segment(aes(x = 3, y = resp[4], xend = 3, yend = mu[4]), col="tomato", size = 1) +
  geom_segment(aes(x = 1.5, y = 5.3, xend = 2, yend = 4.5),lineend = "round", linejoin = "round",
               arrow = arrow(length = unit(0.5, "cm")), color="tomato", size=0.7) +
  geom_segment(aes(x = 1.2, y = 4.5, xend = 1.61, yend = 4),lineend = "round", linejoin = "round",
               arrow = arrow(length = unit(0.5, "cm")), color="grey50", size=0.7) +
  geom_segment(aes(x = 1.2, y = 3.6, xend = 1.5, yend = 3.25),lineend = "round", linejoin = "round",
               arrow = arrow(length = unit(0.5, "cm")), color="blue", size=0.7) +
  geom_text(x=1.5, y=5.5, label="measurement error", color="tomato", size=4) +
  geom_text(x=1.2, y=4.7, label="individual trajectory", color="grey50", size=4) +
  geom_text(x=1.1, y=3.85, label="fitted linear growth", color="blue3", size=4)
ggsave("unconditional model/one_individual.pdf", width = 5, height = 4)
ggsave("unconditional model/one_individual.png", width = 5, height = 4)
           

