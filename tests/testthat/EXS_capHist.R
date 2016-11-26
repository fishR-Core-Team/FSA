d <- data.frame(id=1:5,sex=c("m","m","f","m","f"),
                first=c(1,1,1,0,0),
                second=c(0,0,1,1,1),
                third=c(1,0,0,0,1))

## A small example of 'event' format
ex1 <- data.frame(fish=c(17,18,21,17,21,18,19,20),
                  yr=c(1987,1987,1987,1988,1988,1989,1989,1990))
# convert to different formats
ex1.E2I <- capHistConvert(ex1,id="fish",in.type="event")
ex1.E2F <- capHistConvert(ex1,id="fish",in.type="event",out.type="frequency")
ex1.E2M <- capHistConvert(ex1,id="fish",in.type="event",out.type="MARK")
ex1.E2R <- capHistConvert(ex1,id="fish",in.type="event",out.type="RMark")

## convert each of these to other formats
## Individual to ...
ex1.I2E <- capHistConvert(ex1.E2I,in.type="individual",out.type="event",id="fish")
ex1.I2F <- capHistConvert(ex1.E2I,in.type="individual",out.type="frequency",id="fish")
ex1.I2M <- capHistConvert(ex1.E2I,in.type="individual",out.type="MARK",id="fish")
ex1.I2R <- capHistConvert(ex1.E2I,in.type="individual",out.type="RMark",id="fish")
## Frequency to ...
ex1.F2E <- capHistConvert(ex1.E2F,in.type="frequency",out.type="event",freq="freq")
ex1.F2I <- capHistConvert(ex1.E2F,in.type="frequency",out.type="individual",freq="freq")
ex1.F2I2 <- capHistConvert(ex1.E2F,in.type="frequency",out.type="individual",freq="freq",include.id=TRUE)
ex1.F2M <- capHistConvert(ex1.E2F,in.type="frequency",out.type="MARK",freq="freq")
ex1.F2R <- capHistConvert(ex1.E2F,in.type="frequency",out.type="RMark",freq="freq")
ex1.F2R2 <- capHistConvert(ex1.E2F,in.type="frequency",out.type="RMark",freq="freq",include.id=TRUE)
## MARK to ...
ex1.M2E <- capHistConvert(ex1.E2M,in.type="MARK",out.type="event",freq="freq")
ex1.M2F <- capHistConvert(ex1.E2M,in.type="MARK",out.type="frequency",freq="freq")
ex1.M2I <- capHistConvert(ex1.E2M,in.type="MARK",out.type="individual",freq="freq")
ex1.M2I2 <- capHistConvert(ex1.E2M,in.type="MARK",out.type="individual",freq="freq",include.id=TRUE)
ex1.M2R <- capHistConvert(ex1.E2M,in.type="MARK",out.type="RMark",freq="freq")
ex1.M2R2 <- capHistConvert(ex1.E2M,in.type="MARK",out.type="RMark",freq="freq",include.id=TRUE)
## RMARK to ...
ex1.R2E <- capHistConvert(ex1.E2R,in.type="RMark",out.type="event",id="fish")
ex1.R2F <- capHistConvert(ex1.E2R,in.type="RMark",out.type="frequency",id="fish")
ex1.R2I <- capHistConvert(ex1.E2R,in.type="RMark",out.type="individual",id="fish")
ex1.R2M <- capHistConvert(ex1.E2R,in.type="RMark",out.type="MARK",id="fish")

