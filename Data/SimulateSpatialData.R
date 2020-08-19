library(spatstat)
library(geosphere)
library(gdistance)
library(jsonlite)
library(rlist)
library(randomizr)
library(bupaR)
library(xesreadR)

#Read in eventlog
fileToLoad <- file.choose(new = TRUE)

purchase_order_log <- read_xes(fileToLoad) #BPIC 19

## To create spatial window for traffic fines dataset
it <- owin(xrange=c(6.627266,18.78447),yrange=c(35.28896,47.09215))
##TO create spatial window for BPIC 17,19 and 20 datasets
nl <- owin(xrange=c(1.9193492,7.2274985),yrange=c(50.7295671,53.7253321))

# Simulate clustered (attractive) pattern where requests originates
set.seed(123)
p_cluster <- rThomas(kappa = 1, scale = 5, mu = 200, win = nl)
plot(p_cluster)

#simulate 5 random points in the Dutch bounding box (for BPI 17,19,20) & Italian bounding box (for road fines)
lambda <- 100/area.owin(nl)
lambda1 <- 50/area.owin(nl)
lambda2 <- 2/area.owin(nl)

# Create a point pattern object
ppois <- rpoispp(lambda = lambda, win = nl)
ppois1 <- rpoispp(lambda = lambda1, win = nl)
ppois2 <- rpoispp(lambda = lambda2, win = nl)

# Plot the Poisson point pattern
plot(ppois)

purchase_order_log <- as.data.frame(filtered_log) 
purchase_order_log["RemTime"]  <- NA
purchase_order_log["ElapsedTime"] <- NA
purchase_order_log["x"] <- NA
purchase_order_log["y"] <- NA


purchase_order_log <- as.data.frame(purchase_order_log)

# for 1 to n(p), assign next coordinates to relevant activities
# calculate elapsed & remaining time. also select random processing location and assign 

grouping <- purchase_order_log$case.Purchasing.Document
split.loans <- split(purchase_order_log,grouping)
for (i in seq_along(split.loans)){
  if (i<= p_cluster$n){
    compTime <- split.loans[[i]][[which.max(split.loans[[i]][["event.time.timestamp"]]),"event.time.timestamp"]]
    startTime <- split.loans[[i]][[which.min(split.loans[[i]][["event.time.timestamp"]]),"event.time.timestamp"]]
    #do similar for startTime
    for (j in 1:nrow(split.loans[[i]])){
      split.loans[[i]][j,"RemTime"] <- difftime(compTime,split.loans[[i]][[j,"event.time.timestamp"]],units="days")
      split.loans[[i]][j,"ElapsedTime"] <- difftime(split.loans[[i]][[j,"event.time.timestamp"]],startTime,units="days")
      # spatial attributes in 2 steps
      
      # Assign elements from a vector
      if (split.loans[[i]][[j,"event.concept.name"]] %in% c("Vendor creates invoice","Vendor creates debit memo")){ #anywhere 
        split.loans[[i]][[j,"x"]] <- p_cluster$x[i]
        split.loans[[i]][[j,"y"]] <- p_cluster$y[i]
      }else if (stringr::str_detect(split.loans[[i]][[j,"event.concept.name"]],"SRM")){   #1 or 2              #(split.loans[[i]][[j,"activity_id"]] %in% c("Declaration FINAL_APPROVED by SUPERVISOR","Declaration REJECTED by SUPERVISOR","Declaration FOR_APPROVAL by SUPERVISOR ")){
        mat1 <- sample.int(ppois2$n,1,replace = TRUE) #ppois[which.max(distHaversine(cbind(ppois$x,ppois$y),cbind(p_cluster$x[i],p_cluster$y[i])))]
        split.loans[[i]][[j,"x"]] <- ppois2[mat1]$x
        split.loans[[i]][[j,"y"]] <- ppois2[mat1]$y
      }else if (split.loans[[i]][[j,"event.concept.name"]] %in% c("Record Service Entry Sheet","Record Goods Receipt","Record Invoice Receipt","Cancel Invoice Receipt","Cancel Subsequent Invoice","Record Subsequent Invoice")){ #c.100
        mat2 <- sample.int(ppois$n,1,replace = TRUE) #ppois2[which.max(distHaversine(cbind(ppois2$x,ppois2$y),cbind(p_cluster$x[i],p_cluster$y[i])))]
        split.loans[[i]][[j,"x"]] <- ppois$n[mat2]$x
        split.loans[[i]][[j,"y"]] <- ppois$n[mat2]$y
      }else{ #c.50
        mat <- sample.int(ppois1$n,1,replace = TRUE) #ppois1[which.max(distHaversine(cbind(ppois1$x,ppois1$y),cbind(p_cluster$x[i],p_cluster$y[i])))]
        split.loans[[i]][[j,"x"]] <- ppois1[mat]$x
        split.loans[[i]][[j,"y"]] <- ppois1[mat]$y
      }
      
    }
  }
}

PO_log <- do.call(rbind,split.loans[1:p_cluster$n]) 
View(PO_log)

