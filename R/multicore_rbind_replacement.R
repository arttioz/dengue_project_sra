batch_rbind <- function(x) {
  if (length(x) < 1000) {
    o <- do.call(what=rbind, args=x)
    return(o)
  } else {
    x <- split(x=x, f=factor(gl(n=length(x), k=1000, length=length(x))), drop=TRUE)
    o <- mclapply(X=x, FUN=function(x) do.call(what=rbind, args=x), mc.cores=getOption("mc.cores",12))
    o <- batch_rbind(o)
    rownames(o) <- NULL
    return(o)
  }
}




