# Script to generate the frequency-exclusivity plots

gen.fe.plot <- function(mu.vec,logit.phi.vec,topic,
                        fe.full.dir,
                        fe.zoom.dir,
                        fe.joint.dir,
                        topic.name=NULL,
                        # Plot parameters
                        res.plot=200,
                        size.inch=8,
                        lower.quant.cut.full=0.01,
                        upper.quant.zoom=0.95){

  
  
  # Use the topic code if no topic.name given
  if(is.null(topic.name)){topic.name <- topic}

  # Get quantiles of each dimension
  quant.mu <- quantile(mu.vec,probs=c(lower.quant.cut.full,
                                upper.quant.zoom))
  quant.phi <- quantile(logit.phi.vec,probs=c(lower.quant.cut.full,
                                upper.quant.zoom))

  # Set up full plot data
  mu.keep.full <- mu.vec > quant.mu[1]
  phi.keep.full <- logit.phi.vec > quant.phi[1]
  index.keep.full <- apply(cbind(mu.keep.full,phi.keep.full),1,all)
  
  mu.vec.full <- mu.vec[index.keep.full]
  logit.phi.vec.full <- logit.phi.vec[index.keep.full]
  
  
  # Get zoom plots - all points in top 5% of both dimensions
  mu.keep.zoom <- mu.vec > quant.mu[2]
  phi.keep.zoom <- logit.phi.vec > quant.phi[2]
  index.keep.zoom <- apply(cbind(mu.keep.zoom,phi.keep.zoom),1,all)
  
  mu.vec.zoom <- mu.vec[index.keep.zoom]
  logit.phi.vec.zoom <- logit.phi.vec[index.keep.zoom]
  
  # Figure out which points to label - top 5% of either dimension
  mu.label.zoom <- mu.vec > quant.mu[2]
  phi.label.zoom <- logit.phi.vec > quant.phi[2]
  index.label.zoom1 <- apply(cbind(mu.label.zoom,phi.label.zoom),1,any)
  index.label.zoom <- apply(cbind(index.label.zoom1,index.keep.zoom),1,all)

  mu.vec.label <- mu.vec[index.label.zoom]
  logit.phi.vec.label <- logit.phi.vec[index.label.zoom]
  labels.zoom <- word.stem.table[index.label.zoom,1]

  
  # Create zoom plot
  cex.plot <- 0.6
  title.plot <- paste("Upper 5% of FE plot for",topic.name)
  title.png <- paste(fe.zoom.dir,"fe_zoom_plot_",topic,".png",sep="")
  png(title.png,width=size.inch,height=size.inch,units="in",res=res.plot)
  plot(mu.vec.zoom,logit.phi.vec.zoom,main=title.plot,
       ylab=expression(paste("Exclusivity: ",logit(phi[fk]))),
       xlab=expression(paste("Frequency: ",mu[fk])),cex=cex.plot,col="white")
  text(mu.vec.label, logit.phi.vec.label, labels = labels.zoom, adj = NULL,
       pos = NULL, offset = 0, vfont = NULL,
       cex = cex.plot, col = NULL, font = NULL)
  dev.off()
  
  # Set up side-by-side plot
  title.png <- paste(fe.joint.dir,"fe_joint_plot_",topic,".png",sep="")
  png(title.png,width=size.inch*2,height=size.inch,units="in",res=res.plot)
  title.plot <- paste("FE plot for",topic.name)
  par(mfrow=c(1,2),mar=c(5, 4, 0, 2)+0.1, oma=c(0,0,4,0))
  plot(mu.vec.full,logit.phi.vec.full,
       #main=title.plot,
       ylab=expression(paste("Exclusivity: ",logit(phi[fk]))),
       xlab=expression(paste("Frequency: ",mu[fk])),cex=0.5)
  ##################################################
  lim.mu <- range(mu.vec.zoom)
  mar.mu <- (lim.mu[2]-lim.mu[1])*0.04
  lim.phi <- range(logit.phi.vec.zoom)
  mar.phi <- (lim.phi[2]-lim.phi[1])*0.04
  ##################################################
  rect(xleft=quant.mu[2],
       ybottom=quant.phi[2],
       xright=lim.mu[2]+mar.mu,
       ytop=lim.phi[2]+mar.phi,
       density = NULL, angle = 45,
       col = NULL, border = "red", lty = par("lty"), lwd = par("lwd"))
  #title.plot <- paste("Upper 5% of Frequency-Exclusivity plot for",topic.name)
  plot(mu.vec.zoom,logit.phi.vec.zoom,
       #main=title.plot,
       ylab=expression(paste("Exclusivity: ",logit(phi[fk]))),
       xlab=expression(paste("Frequency: ",mu[fk])),cex=cex.plot,col="white")
  text(mu.vec.label, logit.phi.vec.label, labels = labels.zoom, adj = NULL,
       pos = NULL, offset = 0, vfont = NULL,
       cex = cex.plot, col = NULL, font = NULL)
  mtext(title.plot, side=3, line=2, cex=2, outer=TRUE, font=2)  
  dev.off()

  
  
  title.plot <- paste("FE plot for",topic.name)
  title.png <- paste(fe.full.dir,"fe_plot_",topic,".png",sep="")
  png(title.png,width=size.inch,height=size.inch,units="in",res=res.plot)
  plot(mu.vec.full,logit.phi.vec.full,main=title.plot,
       ylab=expression(paste("Exclusivity: ",logit(phi[fk]))),
       xlab=expression(paste("Frequency: ",mu[fk])),cex=0.5)
  rect(xleft=quant.mu[2],
       ybottom=quant.phi[2],
       xright=lim.mu[2]+mar.mu,
       ytop=lim.phi[2]+mar.phi,
       density = NULL, angle = 45,
       col = NULL, border = "red", lty = par("lty"), lwd = par("lwd"))
  dev.off()
}
