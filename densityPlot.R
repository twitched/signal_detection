library(ROCR)
n <- 1000
threshold <- 12
A.stdev = 1
A.mean = 10
B.offset = 4
B.mean = 10 + B.offset
B.stdev = 2
A <- rnorm(n, A.mean, A.stdev)
B <- rnorm(n, B.mean, B.stdev)
xlims <- range(A.mean - A.stdev * 4, B.mean + B.stdev * 4)
A.density <- density(A, bw = "SJ", adjust = 2, from = xlims[1], to = xlims[2])
B.density <- density(B, bw = "SJ", adjust = 2, from = xlims[1], to = xlims[2]) 
maxY <- max(c(A.density$y, B.density$y))
ylims <- range(0, maxY * 1.1)
plot(A.density, col = "blue", xlim = xlims, ylim = ylims, 
     main = "Probability Density of two classes with cutoff", 
     ylab = "Probability Density",
     xlab = "Criterion Response")
lines(B.density, col = "red")
#colors

cols <- list()
cols[["True Positive"]] <- "red"
cols[["True Negative"]] <- "blue"
cols[["False Positive"]] <- "lightpink"
cols[["False Negative"]] <- "lightblue"
cols[["False Positive if negative"]] <- "violetred"
cols[["False Negative if positive"]] <- "blueviolet"

legend("topright", names(cols), fill = sapply(cols, function(x){ x[1] }))
            
#Find the intersection point
between_the_means <- which(A.density$x > A.mean & B.density$x < B.mean) #intersection point is between the means
inter_point_conditional <- as.logical(abs(diff(A.density$y[between_the_means] < B.density$y[between_the_means])))
intersX <- A.density$x[between_the_means][inter_point_conditional] 
intersY <- A.density$y[between_the_means][inter_point_conditional]

#true positive
left <- if(intersX > threshold) intersX else threshold
polygon(c(B.density$x[which(B.density$x > left)], rev(A.density$x[which(A.density$x > left)])),
        c(B.density$y[which(B.density$x > left)], rev(A.density$y[which(A.density$x > left)])),
        col = cols[["True Positive"]])
#true negative
right <- if(intersX < threshold) intersX else threshold
polygon(c(A.density$x[which(A.density$x < right)], right, rev(A.density$x[which(A.density$x < right)])),
        c(A.density$y[which(A.density$x < right)], rev(B.density$y[which(B.density$x < right)])[1], rev(B.density$y[which(B.density$x < right)])),
        col = cols[["True Negative"]])
#false negative
polygon(c(rev(A.density$x[which(A.density$x > intersX & A.density$x < threshold)]), B.density$x[which(B.density$x > intersX & B.density$x < threshold)]),
        c(rev(A.density$y[which(A.density$x > intersX & A.density$x < threshold)]), B.density$y[which(B.density$x > intersX & B.density$x < threshold)]),
        col = cols[["False Negative"]])
#false positive
polygon(c(A.density$x[which(A.density$x > threshold & B.density$x < intersX)], rev(B.density$x[which(B.density$x < intersX & B.density$x > threshold)])),
        c(A.density$y[which(A.density$x > threshold & B.density$x < intersX)], rev(B.density$y[which(B.density$x < intersX & B.density$x > threshold)])),
        col = cols[["False Postiive"]])
#overlapPos
polygon(c(B.density$x[which(B.density$x > threshold & B.density$x < intersX)], A.density$x[which(A.density$x > left)], threshold), 
        c(B.density$y[which(B.density$x > threshold & B.density$x < intersX)], A.density$y[which(A.density$x > left)], 0), 
        col = cols[["False Positive if negative"]])
#overlapNeg
polygon(c(B.density$x[which(B.density$x < right)], A.density$x[which(A.density$x > intersX & A.density$x < threshold)], threshold), 
        c(B.density$y[which(B.density$x < right)], A.density$y[which(A.density$x > intersX & A.density$x < threshold)],0), 
        col = cols[["False Negative if positive"]])
abline(v = threshold)
#create the ROCR Prediction and Performance objects
pred <- prediction(c(A, B), c(rep("A", n), rep("B", n)))
perf <- performance(pred, "prbe")