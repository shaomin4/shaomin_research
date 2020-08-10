library(HMM)
# Initial HMM
hmm = initHMM(c("Sun","Cloud","Rainy"),c("Swin","Basketball","Gym"),
              transProbs=matrix(c(.5,.375,.125,.25,.125,.625,.25,.375,.375),3),
              emissionProbs=matrix(c(.6,.3,.1,.1,.01,.89,.3,.4,.3),3),
              startProbs = matrix(c(1,0,0)))
print(hmm)

# Sequence of observation
a = sample(c(rep("Swin",100),rep("Basketball",200),rep("Gym",300)))
b = sample(c(rep("Swin",300),rep("Basketball",200),rep("Gym",100)))
observation = c(a,b)
# Baum-Welch
# HMM::baumWelch(hmm, observation, maxIterations=100, delta=1E-9, pseudoCount=0)
bw = baumWelch(hmm,observation,50)
print(bw$hmm)

