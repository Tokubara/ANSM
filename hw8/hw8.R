pbinom(26, 77, prob = 0.1)

# 3.5
d = c(Margate = 443, Kew = 598, Cheltenham = 738, Cambridge = 556, Birmingham = 729, Cromer = 646, York = 654, Carlisle = 739, Newcastle = 742, Edinburgh = 699, Callander = 1596, Dundee = 867, Aberdeen = 877, Nairn = 642, Baltasound = 1142)
diff_d = diff(d, lag = floor(length(d) / 2 + 1))
size = length(diff_d)
gt_size = sum(diff_d > 0)
1 - pbinom(gt_size - 1, size = size, p = 0.5)

d = c(475, 483, 627, 881, 892, 924, 1077, 1224, 1783, 1942, 2013, 2719, 4650, 6915)
ks.test(d, punif, 400, 7000)

# 第一问


sign_ci <- function(x) {
  n = length(x)
  k = 0:(n / 2)
  conf.level = setNames(1 - 2 * pbinom(k, s = n, p = 0.5), nm = k)
  return(list(all = sort(x), conf.level = conf.level))
}
d <- c(5.5, 6.0, 6.5, 7.6, 7.6, 7.7, 8.0, 8.2, 9.1, 15.1)
sign_ci(d)


wilcox.test(d, conf.int = T)

wilcox_ci <- function(x) {
  wa <- outer(x, x, function(x, y)(x + y) / 2)
  wa <- sort(wa[lower.tri(wa, diag = T)])
  alpha = 0.05
  n = length(x)
  m <- n * (n + 1) / 2 # wa的长度
  k <- 0:(m / 2) # wa长度的一半的序列, k有可能的收缩值
  conf.lev <- 1 - 2 * psignrank(k, n)
  names(conf.lev) <- k
  return(list(all = conf.lev[conf.level > 0.9], wa = wa))
}

res = wilcox_ci(d)
res

d.example = c(-1, 6, 13, 4, 2, 3, 5, 9)
res.example = wilcox_ci(d.example)

