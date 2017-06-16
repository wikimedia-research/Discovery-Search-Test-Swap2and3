library(cowplot)

## Compare ZRR 
searches %>%
  group_by(`test group`) %>%
  summarise(zero_result = sum(results=="zero"), searches = n(), zrr = sum(results=="zero")/n()) %>%
  cbind(binom:::binom.bayes(.$zero_result, n=.$searches)[, c("mean", "lower", "upper")]) 

######
# Engagement
######
# Compare overall CTR
# per session:
engagement_overall_session <- data1 %>% 
  group_by(event_subTest, event_searchSessionId) %>%
  summarise(clickthrough = "click" %in% event_action|"visitPage" %in% event_action, results = sum(event_hitsReturned, na.rm=TRUE) > 0) %>%
  filter(results == TRUE) %>%
  group_by(event_subTest) %>%
  summarise(clicks = sum(clickthrough), session = n()) %>%
  cbind(binom:::binom.bayes(.$clicks, n=.$session)[, c("mean", "lower", "upper")]) 
  #knitr::kable(engagement_overall_session, format = "markdown", align = c("l", "r", "r", "r", "r", "r"))

# per SERP:
engagement_overall_serp <- searches %>%
  filter(results == "some") %>%
  group_by(`test group`) %>%
  summarise(clicks = sum(clickthrough), searches = n()) %>%
  cbind(binom:::binom.bayes(.$clicks, n=.$searches, tol=.Machine$double.eps^0.6)[, c("mean", "lower", "upper")])
  #knitr::kable(engagement_overall_serp, format = "markdown", align = c("l", "r", "r", "r", "r", "r"))

# plot
p_ctr_ses <- engagement_overall_session %>%
  ggplot(aes(x = 1, y = mean, color = event_subTest)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Clickthrough rate",
       title = "Proportion of sessions that include at least one click/visitPage event") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * mean), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  theme(legend.position = "bottom")
p_ctr_serp <- engagement_overall_serp %>%
  ggplot(aes(x = 1, y = mean, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Clickthrough rate",
       title = "Proportion of SERPs that include at least one click event") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * mean), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  theme(legend.position = "bottom")
plot_grid(p_ctr_ses, p_ctr_serp)

# Compare position 2 and 3
# per session, among all nonzero result session:
engagement_2and3_session <- data1 %>% 
  group_by(event_subTest, event_searchSessionId) %>%
  summarise(clickthrough2 = 2 %in% event_position, clickthrough3 = 3 %in% event_position, 
            clickthrough = "click" %in% event_action|"visitPage" %in% event_action, results = sum(event_hitsReturned, na.rm=TRUE) > 0) %>%
  filter(results == TRUE) %>%
  group_by(event_subTest) %>%
  summarise(clicked2 = sum(clickthrough2), clicked3 = sum(clickthrough3), session = n()) %>%
  cbind(binom:::binom.bayes(.$clicked2, n=.$session)[, c("mean", "lower", "upper")]) %>%
  rename(ctr2 = mean, lower2 = lower, upper2 = upper) %>%
  cbind(binom:::binom.bayes(.$clicked3, n=.$session)[, c("mean", "lower", "upper")]) %>%
  rename(ctr3 = mean, lower3 = lower, upper3 = upper)
  #knitr::kable(engagement_2and3_session, format = "markdown", align = c("l", "r", "r", "r", "r", "r", "r", "r", "r", "r"))
# per session, among all clickthrough session:
data1 %>% 
  group_by(event_subTest, event_searchSessionId) %>%
  summarise(clickthrough2 = 2 %in% event_position, clickthrough3 = 3 %in% event_position, 
            clickthrough = "click" %in% event_action|"visitPage" %in% event_action, results = sum(event_hitsReturned, na.rm=TRUE) > 0) %>%
  filter(clickthrough == TRUE) %>%
  group_by(event_subTest) %>%
  summarise(clicked2 = sum(clickthrough2), clicked3 = sum(clickthrough3), session = n()) %>%
  cbind(binom:::binom.bayes(.$clicked2, n=.$session)[, c("mean", "lower", "upper")]) %>%
  rename(ctr2 = mean, lower2 = lower, upper2 = upper) %>%
  cbind(binom:::binom.bayes(.$clicked3, n=.$session, tol=.Machine$double.eps^0.6)[, c("mean", "lower", "upper")]) %>%
  rename(ctr3 = mean, lower3 = lower, upper3 = upper) %>%
  knitr::kable(., format = "markdown", align = c("l", "r", "r", "r", "r", "r", "r", "r", "r", "r"))
# per SERP, among all nonzero result session:
engagement_2and3_serp <- searches %>%
  filter(results == "some") %>%
  group_by(`test group`) %>%
  summarise(clicked2 = sum(`Clicked on position 2`), clicked3 = sum(`Clicked on position 3`), searches = n()) %>%
  cbind(binom:::binom.bayes(.$clicked2, n=.$searches, tol=.Machine$double.eps^0.7)[, c("mean", "lower", "upper")]) %>%
  rename(ctr2 = mean, lower2 = lower, upper2 = upper) %>%
  cbind(binom:::binom.bayes(.$clicked3, n=.$searches, tol=.Machine$double.eps^0.7)[, c("mean", "lower", "upper")]) %>%
  rename(ctr3 = mean, lower3 = lower, upper3 = upper)
  #knitr::kable(engagement_2and3_serp, format = "markdown", align = c("l", "r", "r", "r", "r", "r", "r", "r", "r", "r"))
# per SERP, among all clickthrough session:
searches %>%
  filter(clickthrough == TRUE) %>%
  group_by(`test group`) %>%
  summarise(clicked2 = sum(`Clicked on position 2`), clicked3 = sum(`Clicked on position 3`), searches = n()) %>%
  cbind(binom:::binom.bayes(.$clicked2, n=.$searches, tol=.Machine$double.eps^0.7)[, c("mean", "lower", "upper")]) %>%
  rename(ctr2 = mean, lower2 = lower, upper2 = upper) %>%
  cbind(binom:::binom.bayes(.$clicked3, n=.$searches, tol=.Machine$double.eps^0.7)[, c("mean", "lower", "upper")]) %>%
  rename(ctr3 = mean, lower3 = lower, upper3 = upper) %>%
  knitr::kable(., format = "markdown", align = c("l", "r", "r", "r", "r", "r", "r", "r", "r", "r"))

# plot
p_ctr23_ses <- engagement_2and3_session[, c("ctr2", "lower2", "upper2")] %>%
  rbind(setNames(engagement_2and3_session[, c("ctr3", "lower3", "upper3")], names(.))) %>%
  mutate(`test group` = c("Control_position2", "swap2and3_position2", "Control_position3", "swap2and3_position3")) %>%
  ggplot(aes(x = 1, y = ctr2, color = `test group`)) +
  geom_pointrange(aes(ymin = lower2, ymax = upper2), position = position_dodge(width = 1)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Clickthrough rate",
       title = "Proportion of sessions that include at least one click/visitPage event in position x") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * ctr2), y = upper2 + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  theme(legend.position = "bottom")
p_ctr23_serp <- engagement_2and3_serp[, c("ctr2", "lower2", "upper2")] %>%
  rbind(setNames(engagement_2and3_serp[, c("ctr3", "lower3", "upper3")], names(.))) %>%
  mutate(`test group` = c("Control_position2", "swap2and3_position2", "Control_position3", "swap2and3_position3")) %>%
  ggplot(aes(x = 1, y = ctr2, color = `test group`)) +
  geom_pointrange(aes(ymin = lower2, ymax = upper2), position = position_dodge(width = 1)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Clickthrough rate",
       title = "Proportion of SERPs that include at least one click event in position x") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * ctr2), y = upper2 + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  theme(legend.position = "bottom")
plot_grid(p_ctr23_ses, p_ctr23_serp)

######
# First Clicked Result’s Position
######
safe_ordinals <- function(x) {
  return(vapply(x, toOrdinal::toOrdinal, ""))
}
first_clicked <- searches %>%
  filter(results == "some" & clickthrough & !is.na(`first clicked result's position`)) %>%
  mutate(`first clicked result's position` = ifelse(`first clicked result's position` < 5, safe_ordinals(`first clicked result's position`), "5th or higher")) %>%
  group_by(`test group`, `first clicked result's position`) %>%
  tally %>%
  mutate(total = sum(n), prop = n/total) %>%
  ungroup
temp <- as.data.frame(binom:::binom.bayes(first_clicked$n, n = first_clicked$total)[, c("mean", "lower", "upper")])
first_clicked <- cbind(first_clicked, temp); rm(temp)
first_clicked %>%
  ggplot(aes(x = 1, y = mean, color = `test group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  geom_text(aes(label = sprintf("%.1f", 100 * prop), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  scale_y_continuous(labels = scales::percent_format(),
                     expand = c(0, 0.005), breaks = seq(0, 1, 0.01)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  facet_wrap(~ `first clicked result's position`, scale = "free_y", nrow = 1) +
  labs(x = NULL, y = "Proportion of searches",
       title = "Position of the first clicked result",
       subtitle = "With 95% credible intervals") +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        strip.background = element_rect(fill = "gray90"),
        panel.border = element_rect(color = "gray30", fill = NA))


######
# Dwell Time per Visited Page
######
# Compare overall dwell time 
clickedResults %>%
  split(.$test_group) %>% 
  map_df(function(df) {
    seconds <- c(0, 10, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210, 240, 300, 360, 420)
    seshs <- as.data.frame(do.call(cbind, lapply(seconds, function(second) {
      return(sum(df$dwell_time >= second))
    })))
    names(seshs) <- seconds
    return(cbind(test_group = head(df$test_group, 1), n = seshs$`0`, seshs, `450`=seshs$`420`))
  }) %>%
  gather(seconds, visits, -c(test_group, n)) %>%
  mutate(seconds = as.numeric(seconds)) %>%
  group_by(test_group, seconds) %>%
  mutate(proportion = visits/n) %>%
  ungroup() %>%
  ggplot(aes(group=test_group, color=test_group)) +
  geom_step(aes(x = seconds, y = proportion), direction = "hv") +
  scale_x_continuous(name = "T (Dwell Time in seconds)", breaks=c(0, 10, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210, 240, 300, 360, 420))+ 
  scale_y_continuous("Proportion of visits longer than T (P%)", labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")
# survival curve with CI
temp <- clickedResults
temp$SurvObj <- with(temp, survival::Surv(dwell_time, status == 2))
fit_all <- survival::survfit(SurvObj ~ test_group, data = temp)
survminer::ggsurvplot(fit_all, conf.int = TRUE, xlab="T (Dwell Time in seconds)", ylab="Proportion of visits longer than T (P%)", 
                      surv.scale = "percent", palette="Set1", legend="bottom", legend.title = "Test Group", legend.labs=c("Control","swap2and3"))
rm(temp)

# Compare position 2 and 3
clickedResults %>%
  filter(position %in% c(2,3)) %>%
  mutate(`Test Group` = paste(test_group, paste0("position",position), sep="_")) %>%
  split(.$`Test Group`) %>% 
  map_df(function(df) {
    seconds <- c(0, 10, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210, 240, 300, 360, 420)
    seshs <- as.data.frame(do.call(cbind, lapply(seconds, function(second) {
      return(sum(df$dwell_time >= second))
    })))
    names(seshs) <- seconds
    return(cbind(`Test Group` = head(df$`Test Group`, 1), n = seshs$`0`, seshs, `450`=seshs$`420`))
  }) %>%
  gather(seconds, visits, -c(`Test Group`, n)) %>%
  mutate(seconds = as.numeric(seconds)) %>%
  group_by(`Test Group`, seconds) %>%
  mutate(proportion = visits/n) %>%
  ungroup() %>%
  ggplot(aes(group=`Test Group`, color=`Test Group`)) +
  geom_step(aes(x = seconds, y = proportion), direction = "hv") +
  scale_x_continuous(name = "T (Dwell Time in seconds)", breaks=c(0, 10, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210, 240, 300, 360, 420))+ 
  scale_y_continuous("Proportion of visits longer than T (P%)", labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1)) +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")
# survival curve with CI
temp <- clickedResults %>% filter(position %in% c(2,3)) %>%
  mutate(Test_Group = paste(test_group, paste0("position",position), sep="_"))
temp$SurvObj <- with(temp, survival::Surv(dwell_time, status == 2))
fit_2and3 <- survival::survfit(SurvObj ~ Test_Group, data = temp)
survminer::ggsurvplot(fit_2and3, conf.int = TRUE, xlab="T (Dwell Time in seconds)", ylab="Proportion of visits longer than T (P%)", 
                      surv.scale = "percent", palette="Set1", legend="bottom", legend.title = "Test Group",
                      legend.labs=c("Control_position2","Control_position3","swap2and3_position2","swap2and3_position3"))
rm(temp)


######
# Scroll
######
# Compare overall scroll proportion
scroll_overall <- clickedResults %>%
  group_by(test_group) %>%
  summarize(scrolls=sum(scroll), visits=n(), proportion = sum(scroll)/n()) %>%
  ungroup
scroll_overall <- cbind(
  scroll_overall,
  as.data.frame(
    binom:::binom.bayes(
      scroll_overall$scrolls,
      n = scroll_overall$visits)[, c("mean", "lower", "upper")]
  )
)
scroll_overall %>%
  ggplot(aes(x = 1, y = mean, color = test_group)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Proportion of visits",
       title = "Proportion of visits with scroll by test group") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * proportion), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  theme(legend.position = "bottom")

# Compare position 2 and 3
scroll_2and3 <- clickedResults %>%
  filter(position %in% c(2,3)) %>%
  mutate(`Test Group` = paste(test_group, paste0("position",position), sep="_")) %>%
  group_by(`Test Group`) %>%
  summarize(scrolls=sum(scroll), visits=n(), proportion = sum(scroll)/n()) %>%
  ungroup
scroll_2and3 <- cbind(
  scroll_2and3,
  as.data.frame(
    binom:::binom.bayes(
      scroll_2and3$scrolls,
      n = scroll_2and3$visits)[, c("mean", "lower", "upper")]
  )
)
scroll_2and3 %>%
  ggplot(aes(x = 1, y = mean, color = `Test Group`)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  scale_color_brewer("Test Group", palette = "Set1", guide = guide_legend(ncol = 2)) +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
  labs(x = NULL, y = "Proportion of visits",
       title = "Proportion of visits with scroll by test group") +
  geom_text(aes(label = sprintf("%.1f%%", 100 * proportion), y = upper + 0.0025, vjust = "bottom"),
            position = position_dodge(width = 1)) +
  theme(legend.position = "bottom")
