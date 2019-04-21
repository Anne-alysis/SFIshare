# Uses a data frame dfsum with a time step variable “time” and plotting coordinates x and y as “xm” and “ym”
# for a given range, rangex and rangey


# first, need to install ffmpeg.  if on a mac, from the terminal “brew install ffmpeg”,  using homebrew.  otherwise, i think you can download it from the web.  


# function to produce plot for one time step
gameboard<-function(i){
        g<-ggplot(filter(dfsum, time==i),aes(x=xm,y=ym))+
                geom_point(aes(col=hero,shape=team),size=4)+
                theme_light()+scale_y_continuous(limits=rangey)+
                scale_x_continuous(limits=rangex)+scale_color_brewer(palette="RdBu")+
                guides(col=guide_legend(ncol=2))+labs(x="x",y="y")
        g
}

# find all plots for each time step and save as a ggplot object list
g<-lapply(unique(dfsum$time),gameboard)

# print each ggplot object in the video
saveVideo({
        lapply(g,print)
}, video.name = "Game_time.mp4", other.opts = "-pix_fmt yuv420p -b 300k")

# i believe these options are for higher resolution, but you can google around for additional options