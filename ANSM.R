sign_median_ci <- function(x, alpha = 0.05) {
  sort_x = sort(x)
  n = length(x)
  k = 0:(n / 2)
  conf.level = setNames(1 - 2 * pbinom(k, s = n, p = 0.5), nm = k)
  conf.level_filter = conf.level[conf.level >= 1 - alpha]
  k_max = as.integer(names(conf.level_filter[length(conf.level_filter)]))
  conf.ci = c(sort_x[1 + k_max], sort_x[n - k_max])
  return(list(ci = conf.ci, conf.level = conf.level_filter[length(conf.level_filter)]))
}

walshAverages <- function(x) {
  wa <- outer(x, x, "+")/2
  wa <- sort(wa[lower.tri(wa,diag=TRUE)])
  return(wa)
}

wilcox_ci <- function(x, alpha = 0.05) {
  # 没有考虑tie
  wa <- walshAverages(x)
  alpha = 0.05
  n = length(x)
  m <- n * (n + 1) / 2 # wa的长度
  k <- 0:(m / 2) # wa长度的一半的序列, k有可能的收缩值
  conf.level <- setNames(1 - 2 * psignrank(k, n),nm=k)
  conf.level_filter = conf.level[conf.level >= 1 - alpha]
  k_max = as.integer(names(conf.level_filter[length(conf.level_filter)]))
  conf.ci = c(wa[1 + k_max], wa[m - k_max])
  return(list(ci = conf.ci, conf.level = conf.level_filter[length(conf.level_filter)]))
}

# wilcox_ci(c(-1, 6, 13, 4, 2, 3, 5, 9)) 得到了正确结果
