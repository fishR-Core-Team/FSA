d <- data.frame(id=1:5,sex=c("m","m","f","m","f"),
                first=c(1,1,1,0,0),
                second=c(0,0,1,1,1),
                third=c(1,0,0,0,1))

## A small example of 'event' format
ex1 <- data.frame(fish=c(17,18,21,17,21,18,19,20),
                  yr=c(1987,1987,1987,1988,1988,1989,1989,1990))
# convert to 'individual' format
( ex1.E2I <- capHistConvert(ex1,id="fish",in.type="event") )
# convert to 'frequency' format
( ex1.E2F <- capHistConvert(ex1,id="fish",in.type="event",out.type="frequency") )
# convert to 'MARK' format
( ex1.E2M <- capHistConvert(ex1,id="fish",in.type="event",out.type="MARK") )
# convert to 'RMark' format
( ex1.E2R <- capHistConvert(ex1,id="fish",in.type="event",out.type="RMark") )
