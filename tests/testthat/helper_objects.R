set.seed(1)
a = matrix(rnorm(200),ncol=10)
b = matrix(rnorm(200),ncol=10)
col_sig = 1:10
row_sig = 1:20
col_grp = c(rep("A",5),rep("B",5))
row_grp = c(rep("A",15),rep("B",5))
col_dendro = hclust(dist(t(a)))
row_dendro = hclust(dist(a))

