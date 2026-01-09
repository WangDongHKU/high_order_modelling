calculate_RT_summary <- function(nums, province, dates, RT2) {
  RT2_df <- expand.grid(nums = nums, province = province, days = dates) %>%
    mutate(value = as.vector(RT2)) %>%
    arrange(nums, province, days)
  RT2_summary <- RT2_df %>%
    group_by(province, days) %>%
    summarize(
      mean_value = mean(value),
      lower_ci = quantile(value, 0.025),  # 2.5% 分位数
      upper_ci = quantile(value, 0.975)   # 97.5% 分位数
    )
  return(RT2_summary)
}