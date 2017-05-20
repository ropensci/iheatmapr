set.seed(1)

a <- round(rbind(cbind(matrix(rnorm(48,-0.75,),ncol=4),
                 matrix(rnorm(72, 0.75),ncol=6)),
           cbind(matrix(rnorm(48, 0.75),ncol=4),
                 matrix(rnorm(72,-0.75),ncol=6))), digits = 3)[c(1:3,10:15,4:9,16:20),c(1:3,9:10,4:8)]
           
b <- round(rbind(cbind(matrix(rnorm(48,-0.75,),ncol=4),
                       matrix(rnorm(72, 0.75),ncol=6)),
                 cbind(matrix(rnorm(48, 0.75),ncol=4),
                       matrix(rnorm(72,-0.75),ncol=6))), digits = 3)[c(1:3,10:15,4:9,16:20),c(1,7:8,2:6,9:10)]

col_sig <- 1:10
row_sig <- 1:20
col_grp <- c(rep("A",5),rep("B",5))
row_grp <- c(rep("A",15),rep("B",5))
col_dendro <- hclust(dist(t(a)))
row_dendro <- hclust(dist(a))

