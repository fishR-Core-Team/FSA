
p <- function() {
  vb <- FSA::vbFuns("Typical")
  ages <- seq(0.1,10,length.out=999)
  clrs1 <- scico::scico(4,palette="roma")
  clrs2 <- FSA::col2rgbt(clrs1,0.5)
  Linf <- c(90,130,60,120)
  K <- c(0.3,0.4,0.4,0.2)
  t0 <- c(0,0.5,-0.5,0.5)
  SE <- 4*ages^(1/3)
  plot(NA,xlim=c(0,10),ylim=c(0,133),
       type="l",xlab="",ylab="",xaxt="n",yaxt="n")
  for (i in seq_along(clrs1)) {
    p1 <- vb(ages,Linf=Linf[i],K=K[i],t0=t0[i])
    ages1 <- ages[p1>0]
    SE1 <- SE[p1>0]
    p1 <- p1[p1>0]
    polygon(c(ages1,rev(ages1)),c(p1+SE1,rev(p1-SE1)),
            border=NA,col=clrs2[i])
    lines(ages1,p1,lwd=1,col=clrs1[i],lend=1)
  }
}

hexSticker::sticker(package="FSA",
                    p_x=1.0,p_y=1.55,p_color="#e9371c",p_size=20,
                    subplot=expression(p()),
                    s_x=0.98,s_y=0.9,s_width=1.2,s_height=1.2,
                    h_color="#00579E",h_size=1,h_fill="snow",
                    url="fishr-core-team.github.io/FSA/",
                    u_color="#00579E",u_size=5,
                    filename="man/figures/logo.png")
