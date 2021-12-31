

# load raw data
raw   <- is.trex(example.data(type="doy"),
                 tz="GMT",
                 time.format="%H:%M",
                 solar.time=TRUE,
                 long.deg=7.7459,
                 ref.add=FALSE)

# adjust time steps
input <- dt.steps(input=raw,
                  start="2013-05-01 00:00",
                  end="2013-11-01 00:00",
                  time.int=15,
                  max.gap=60,
                  decimals=10,
                  df=FALSE)

# remove obvious outliers
input[which(input<0.2)]<- NA

input <- tdm_dt.max(input,
                    methods = c("pd", "mw", "dr"),
                    det.pd = TRUE,
                    interpolate = FALSE,
                    max.days = 10,
                    df = FALSE)

plot(input$input, ylab = expression(Delta*italic("V")))

lines(input$max.pd, col = "green")
lines(input$max.mw, col = "blue")
lines(input$max.dr, col = "orange")


output.data<- tdm_cal.sfd(input,make.plot=TRUE,df=TRUE,wood="Coniferous")

plot(output.data$sfd.pd$sfd[1:1000, ], ylim=c(0,10))
# see estimated uncertainty
lines(output.data$sfd.pd$q025[1:1000, ], lty=1,col="grey")
lines(output.data$sfd.pd$q975[1:1000, ], lty=1,col="grey")
lines(output.data$sfd.pd$sfd[1:1000, ])

sfd_data <- output.data$sfd.dr$sfd

