library(metafor)
source("add_emojis.r")

dat <- data.frame(yi = c(-.58, -.03, .12, .25, .89, 3.24),
                  vi = c(.053, .094, .021, .047, .030, .060))

res <- rma(yi, vi, data=dat)

forest(res, xlim=c(-6,8), psize=0, header=TRUE)
add_emojis(dat$yi, dat$vi)

forest(res, xlim=c(-6,8), psize=0, header=TRUE)
add_emojis(dat$yi, dat$vi, type="img")
