z <- akj(faithful$eruptions,seq(from=1,to=5,by=0.01),alpha=5)
plot(seq(from=1,to=5,by=0.01), z$dens, ylim=range(0,z$dens), type ="l", col=2)



z <- akj(data_all[,"recorded_rt"],sort(data_all[,"recorded_rt"]),alpha=0.1)
plot(sort(data_all[,"recorded_rt"]), z$dens, ylim=range(0,z$dens), type ="l", col=2)


