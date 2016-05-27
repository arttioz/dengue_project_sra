read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

dat=read.excel()

fit <- lm(Actual ~ predr + predtr + prerain , dat)

abline(fit)