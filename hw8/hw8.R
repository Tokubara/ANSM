# 课件问题
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
d1 <- c(5.5, 6.0, 6.5, 7.6, 7.6, 7.7, 8.0, 8.2, 9.1, 15.1)
sign_median_ci(d1)
d2 <- c(5.6, 6.1, 6.3, 6.3, 6.5, 6.6, 7.0, 7.5, 7.9, 8.0, 8.0, 8.1, 8.1, 8.2, 8.4, 8.5, 8.7, 9.4, 14.3, 26.0)
sign_median_ci(d2)

# 3.2
pbinom(26, 77, prob = 0.1)
1 - pbinom(25, 77, prob = 0.1)

# 3.5
d = c(Margate = 443, Kew = 598, Cheltenham = 738, Cambridge = 556, Birmingham = 729, Cromer = 646, York = 654, Carlisle = 739, Newcastle = 742, Edinburgh = 699, Callander = 1596, Dundee = 867, Aberdeen = 877, Nairn = 642, Baltasound = 1142)
diff_d = diff(d, lag = floor(length(d) / 2 + 1))
size = length(diff_d)
gt_size = sum(diff_d > 0)
1 - pbinom(gt_size - 1, size = size, p = 0.5)

# 3.8
d = c(475, 483, 627, 881, 892, 924, 1077, 1224, 1783, 1942, 2013, 2719, 4650, 6915)
ks.test(d, punif, 400, 7000)
