#Simplified SEI model for dog rabies 
# KH 11 Aug 2007

ridiculous = 1e-12 # prevents numerical problems...

seivacc = function(t, x, params){
  S=x[1]
  E=x[2]
  I=x[3]
  V=x[4]

  with(as.list(params),{

 		Ex = ifelse(E<ridiculous, 0, E)
		Ix = ifelse(I<ridiculous, 0, I)
 		N = S + Ex + Ix + V

		dS = br*N- dr*S - gamma*S*N - beta*S*Ix - vr*S + il*V
		dV = vr*S - (dr + gamma*N)*V - il*V
		dE = beta*S*Ix - (mu + dr + gamma*N)*Ex
		dI = mu*Ex - (alpha + br + gamma*N)*Ix

		res=c(dS, dE, dI, dV)
		list(res)
	})
}

#Periodicity calculator (def = point when reduce timelength over which period calculated)
fper=function(start, end, series, def){  #function to calculate period
  freqN <- spectrum(series[start:end], plot=FALSE)
  per = 1/freqN$freq[which(freqN$spec==max(freqN$spec))]/52
  end2 = end
  
  while (per>def) {
    end2=end2-((end2-start)/2)
    freqN <- spectrum(series[start:end2], plot=FALSE)
    per = 1/freqN$freq[which(freqN$spec==max(freqN$spec))]/52
  
  }
  per
}




