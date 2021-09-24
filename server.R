fillmap2 <-function(map, figtitle, y , leg.loc="beside", y.scl=NULL,main.cex=1.5,main.line=0,map.lty=1,leg.rnd=0,leg.cex=1){
  y.uq=sort(unique(c(y,y.scl)))
  cols<-viridis(length(y.uq),direction=-1)
  shading=y
  for (i in 1:length(y)){
    shading[i]<-cols[which(y.uq==y[i])]
  }
  par(mar=c(0,0,2,0))
  if (leg.loc=="beside"){
    layout(matrix(1:2,ncol=2),width=c(.8,.2))
  } else
    if (leg.loc=="below"){
      layout(matrix(1:2,nrow=2),height=c(.6,.4))
    } else (print("leg.loc options are below or beside"))
  plot(map,col=shading,axes=F, lty=map.lty)
  title(main=figtitle,cex.main=main.cex,line=main.line)
  par(mar=c(5, 4, 4, 2) + 0.1)
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = '')
  cols.5=cols[seq(1,length(y.uq),length.out=5)]
  lab.5=cols.5
  for (i in 1:5){lab.5[i]=y.uq[which(cols==cols.5[i])[1]]}
  lab.5=round(as.numeric(lab.5),leg.rnd)
  par(mar=c(0,0,0,0))
  if (leg.loc=="beside"){
    legend_image <- as.raster(matrix(cols, ncol=1))
    text(x=1.6,
         y = seq(0,length(y.uq),length.out=5)/length(y.uq),
         labels = rev(lab.5), cex=leg.cex)
    rasterImage(legend_image, 0, 0, 1,1)
  } else{
    legend_image <- as.raster(matrix(cols, nrow=1))
    text(y=-0.25,
         x = seq(0,length(y.uq),length.out=5)/(length(y.uq)*.5),
         labels = lab.5, cex=leg.cex)
    rasterImage(legend_image, 0, 0, 2,1)
  }
}
library(rgdal)
library(INLA)
library(viridis)

getwd()

data=read.csv("data/shiny_data.csv")
data
fe=read.csv("data/fixed.csv")[,-1]
fe
data[is.na(data)]=0

NCtracts=rgdal::readOGR("data/NCTract/final_label.shp")

shinyServer(function(input,output){
  output$map <- renderPlot({
    if (input$adj=="None"){
      if (input$data=="Total Arrests"){
        MapData=data$arrests_total[seq(input$year-2009,dim(data)[1],9)]
        MapDataScl=data$arrests_total
        Caption=paste(input$year,input$data)
      } else 
        if (input$data=="White Arrests"){
          MapData=data$arrests_W[seq(input$year-2009,dim(data)[1],9)]
          MapDataScl=data$arrests_W
          Caption=paste(input$year,input$data)
        } else {
          MapData=data$arrests_B[seq(input$year-2009,dim(data)[1],9)]
          MapDataScl=data$arrests_B
          Caption=paste(input$year,input$data)
        }} else
          if (input$adj=="Standardized Incidence Ratio"){
            if (input$data=="Total Arrests"){
              MapData=data$sir_total[seq(input$year-2009,dim(data)[1],9)]
              MapDataScl=data$sir_total
              Caption=paste(input$year,input$data)
            } else 
              if (input$data=="White Arrests"){
                MapData=data$sir_white[seq(input$year-2009,dim(data)[1],9)]
                MapDataScl=data$sir_white
                Caption=paste(input$year,input$data)
              } else {
                MapData=data$sir_black[seq(input$year-2009,dim(data)[1],9)]
                MapDataScl=data$sir_black
                Caption=paste(input$year,input$data)
              }        
          } else 
            if (input$adj=="Poisson Regression") {
              if (input$data=="Total Arrests"){
                MapData=exp(data$eTot[1:44+44*(input$year-2010)])
                MapDataScl=exp(data$eTot)
                Caption=paste(input$year,input$data)
              } else 
                if (input$data=="White Arrests"){
                  MapData=exp(data$eW[1:44+44*(input$year-2010)])
                  MapDataScl=exp(data$eW)
                  Caption=paste(input$year,input$data)
                } else {
                  MapData=exp(data$eB[1:44+44*(input$year-2010)])
                  MapDataScl=exp(data$eB)
                  Caption=paste(input$year,input$data)
                }        
            } else 
              if (input$adj=="% of the Population"){
                if (input$data=="Total Arrests"){
                  MapData=(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$ct_pop[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                  MapDataScl=(data$arrests_total+.1)/(data$ct_pop+.1)*100
                  Caption=paste(input$year,input$data)
                } else 
                  if (input$data=="White Arrests"){
                    MapData=(data$arrests_W[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$ct_white[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=(data$arrests_W+.1)/(data$ct_white+.1)*100
                    Caption=paste(input$year,input$data)
                  } else {
                    MapData=(data$arrests_B[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$ct_black[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=(data$arrests_B+.1)/(data$ct_black+.1)*100
                    Caption=paste(input$year,input$data)
                  }      
              } else {
                if (input$data=="Total Arrests"){
                  MapData=(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                  MapDataScl=seq(0,100,.1)
                  Caption=paste(input$year,input$data)
                } else 
                  if (input$data=="White Arrests"){
                    MapData=(data$arrests_W[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=seq(0,100,.1)
                    Caption=paste(input$year,input$data)
                  } else {
                    MapData=(data$arrests_B[seq(input$year-2009,dim(data)[1],9)]+.1)/(data$arrests_total[seq(input$year-2009,dim(data)[1],9)]+.1)*100
                    MapDataScl=seq(0,100,.1)
                    Caption=paste(input$year,input$data)
                  }      
              }
    
    fillmap2(final_label,Caption,MapData,map.lty = 0,leg.loc = "beside",y.scl = MapDataScl,leg.rnd = 2)
  })
  
  output$table <- renderTable({
    if (input$adj=="Poisson Regression"){
      if (input$data=="Total Arrests"){
        mat=exp(fe[1:6,])
        colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
        rownames(mat)<-c("% Black",
                         "% Living in Poverty",
                         "% Bachelors degree or more",
                         "% Male",
                         "% Secondary Homes",
                         "% Aged 18-24")
        mat
      } else
        if (input$data=="White Arrests"){
          mat=exp(fe[7:12,])
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("% Black",
                           "% Living in Poverty",
                           "% Bachelors degree or more",
                           "% Male",
                           "% Secondary Homes",
                           "% Aged 18-24")
          mat
        } else {
          mat=exp(fe[13:18,])
          colnames(mat)<-c("Mean","95% CI Lower Bound","95% CI Upper Bound")
          rownames(mat)<-c("% Black",
                           "% Living in Poverty",
                           "% Bachelors degree or more",
                           "% Male",
                           "% Secondary Homes",
                           "% Aged 18-24")
          mat
        }
    }},rownames=T,colnames=T,digits=3,width="100%")
})

library(shiny)
runGitHub( "Shiny", "NolStelz")
