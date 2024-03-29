

data2020 <- read.csv("C:\\Users\\surya\\OneDrive\\Documents\\Purdue\\Spring 2024\\IE 490\\Project\\IE490Dataset_2020.csv")

data2020_num <- read.csv("C:\\Users\\surya\\OneDrive\\Documents\\Purdue\\Spring 2024\\IE 490\\Project\\IE490Dataset_2020_num.csv")

pr.out <- prcomp(data2020_num, scale = TRUE)
###
biplot(pr.out, scale = 0, expand = 3, xlim=c(-10,50), ylim=c(-5,18))
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)

###
pr.var <- pr.out$sdev^2
pr.var

pve <- pr.var / sum(pr.var)
pve

par(mfrow = c(1, 2))
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", ylim = c(0, 1),
     type = "b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")
###