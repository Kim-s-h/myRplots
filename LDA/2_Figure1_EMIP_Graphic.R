library(tm)
library(slam)
library(tidyverse)
library(ggtext)
library(here)
library(wesanderson)
library(ggdark)

source(file = here::here("1_data_preparation_step0.R")) # Base
source(file = here::here("1_data_preparation_step1_LDA.R")) # Base

load(file = here::here('Study1_LDA','results','lda_models.Rdata'))

K = 4-1

tp.est.all = as.data.frame(lda_models$topic_model[[K]]@gamma)
tp.names = sprintf("Topic%s", c(1:(K+1))) 
names(tp.est.all) = tp.names
tp.est.all <- as_tibble(tp.est.all)

tp.est.all %>%
  mutate(Type = "All",
         resp = resp.t1) %>%
  gather(tp.names, key = "Topic", value = "Prop") ->
  tp.est.all.long

tp.est.all.long %>%
  group_by(Topic, resp) %>%
  summarise(m_p = mean(Prop),
            sd_p = sd(Prop)) %>%
  ggplot(aes(x=Topic, y=m_p)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=m_p, ymax=m_p+sd_p),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("Prop") +
  ylim(0, 0.8) + 
  facet_wrap(.~resp, nrow = 5) +
  scale_fill_grey(start = 0, end = .8) +
  theme_bw()

# pal <- wes_palette("Darjeeling1") 
# pal <- wes_palette("GrandBudapest1")
pal <- wes_palette("Royal1")

plot_black <-tp.est.all.long %>%
  group_by(Topic, resp) %>%
  summarise(m_p = mean(Prop),
            sd_p = sd(Prop)) %>%
  ggplot(aes(x=Topic, y=m_p, fill = Topic, color = Topic)) + 
  geom_bar(stat="identity", width=.3) +
  coord_polar() +
  facet_wrap(.~resp, nrow = 2, shrink =T) +
  labs(
    title = "Analysis of Examinees' Responses to an Essay Item",
    subtitle = "Average proportion of topics appeared in written responses at different Score levels",
    y = "",
    color = "Topic",
    fill = "Topic"
  ) +
  # scale_color_brewer(palette = "Set1", type = "qual") +
  # scale_fill_brewer(palette = "Set1", type = "qual") +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
  dark_theme_minimal(20) +
  theme(
    plot.title = element_text(size = 24),
    plot.subtitle = element_textbox(size = 16),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.background = element_rect(fill = "black", color = "midnightblue"),
    legend.position = c(.85, .25)
  )


pal <- wes_palette("GrandBudapest1")

plot_white <- tp.est.all.long %>%
  group_by(Topic, resp) %>%
  summarise(m_p = mean(Prop),
            sd_p = sd(Prop)) %>%
  ggplot(aes(x=Topic, y=m_p, fill = Topic, color = Topic)) + 
  geom_bar(stat="identity", width=.3) +
  coord_polar() +
  facet_wrap(.~resp, nrow = 2, shrink =T) +
  labs(
    title = "Analysis of Examinees' Responses to an Essay Item",
    subtitle = "Average distribution of topics appeared in responses with different Score levels",
    x = "",
    y = "",
    color = "Topic",
    fill = "Topic"
  ) +
  scale_color_brewer(palette = "Set1", type = "qual") +
  scale_fill_brewer(palette = "Set1", type = "qual") +
  # scale_color_manual(values = pal) +
  # scale_fill_manual(values = pal) +
  theme_minimal(20) +
  theme(
    plot.title = element_text(size = 24),
    plot.subtitle = element_textbox(size = 16),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    # legend.background = element_rect(color = "midnightblue"),
    legend.position = c(.85, .25)
  )
ggsave(
  plot_black,
  filename = "Figures/2021-02-01-EMIP-LDA-black.png",
  width = 10,
  height = 8
)
ggsave(
  plot_white,
  filename = "Figures/2021-02-01-EMIP-LDA-white.png",
  width = 10,
  height = 8
)


# ----- tile plot ----------------------------------------------
K = 4-1
tp.beta =apply(lda_models$topic_model[[K]]@beta, 1, function(x) exp(x)/sum(exp(x))) %>% as.data.frame()
tp.names = sprintf("Topic%s", c(1:(K+1))) 
names(tp.beta) = tp.names

tp.beta <- tp.beta %>% 
  mutate(Word = row_number())
M <- 8

tp.beta %>% 
  arrange(desc(Topic1)) %>%
  slice(1:M) %>%
  pull(Word) ->
  T1.list
tp.beta %>% 
  arrange(desc(Topic2)) %>%
  slice(1:M) %>%
  pull(Word) ->
  T2.list
tp.beta %>% 
  arrange(desc(Topic3)) %>%
  slice(1:M) %>%
  pull(Word) ->
  T3.list
tp.beta %>% 
  arrange(desc(Topic4)) %>%
  slice(1:M) %>%
  pull(Word) ->
  T4.list
# tp.beta %>% 
#   arrange(desc(Topic5)) %>%
#   slice(1:M) %>%
#   pull(Word) ->
#   T5.list

# w.list = unique(c(T1.list,T2.list,T3.list,T4.list, T5.list)) 
w.list = unique(c(T1.list,T2.list,T3.list,T4.list)) 
length(w.list)

tp.beta.long <- tp.beta %>%
  filter(Word %in% w.list) %>% 
  mutate(Word = factor(Word, levels=w.list)) %>%
  pivot_longer(Topic1:Topic4,names_to = "Topic", values_to ="Prob")

tp_tile <- tp.beta.long %>%
  ggplot(aes(x=Topic, y=Word, fill=Prob)) + 
  geom_tile(colour="white", size = 1.4, show.legend = FALSE) + 
  theme(text = element_text(size=14)) +
  labs(x = "", y="") +
  scale_fill_gradient(low = "slategray1", high = "navyblue") +
  # scale_fill_gradient(low = "yellowgreen", high = "tomato3") +
  theme_minimal(16) +
  scale_y_discrete(labels=sprintf("w%s", c(1:length(w.list))))
  # theme(
  #   plot.title = element_text(size = 24),
  #   plot.subtitle = element_textbox(size = 16),
  #   axis.text.y = element_text()
  #   axis.ticks = element_blank(),
  # )
ggsave(
  tp_tile,
  filename = "Figures/2021-02-01-EMIP-LDA-tile.png",
  width = 4,
  height = 8
)
