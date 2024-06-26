cost = function(nprompt, ntok_in, price1k_in, ntok_out, price1k_out, verbose=T){
  
  p_in  = price1k_in / 1000
  p_out = price1k_out / 1000

  dollars = nprompt*(ntok_in * p_in + ntok_out * p_out)

  return(dollars)
}

# pricing: https://openai.com/api/pricing/

# length of scenario description
ntok_in  = 390  
# output
ntok_out = 6    


##################
# Costs per sample
##################

# survey responses from 2097 users
nprompt = 22315 

# gpt-3.5-turbo-0125: 3.57$ for API 22315 calls
c_gpt35 = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.0005, price1k_out = 0.0015)

# gpt-4-turbo: 71.41$ for API 22315 calls
c_gpt4t = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.01, price1k_out = 0.03)

# gpt-4o: 35.7$ for API 22315 calls
c_gpt4o = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
               price1k_in = 0.005, price1k_out = 0.015)

writeLines(paste0("gpt-3.5-turbo-0125: ",round(c_gpt35,2),"$ for ",nprompt," API calls."))
writeLines(paste0("gpt-4-turbo: ",round(c_gpt4t,2),"$ for ",nprompt," API calls."))
writeLines(paste0("gpt-4o: ",round(c_gpt4o,2),"$ for ",nprompt," API calls."))
writeLines(paste0("Total cost: ", round(sum(c(c_gpt35,c_gpt4t,c_gpt4o)),2),"$."))

# gpt-3.5-turbo-0125: 4.55$ for API 22315 calls.
# gpt-4-turbo: 91.05$ for API 22315 calls.
# gpt-4o: 45.52$ for API 22315 calls.
# Total cost: 141.12$.


##################
# Costs per prompt
##################

# gpt-3.5-turbo-0125: 3.57$ for API 22315 calls
c1_gpt35 = cost(nprompt=1, ntok_in=ntok_in, ntok_out=ntok_out, 
                price1k_in = 0.0005, price1k_out = 0.0015)

# gpt-4-turbo: 71.41$ for API 22315 calls
c1_gpt4t = cost(nprompt=1, ntok_in=ntok_in, ntok_out=ntok_out, 
                price1k_in = 0.01, price1k_out = 0.03)

# gpt-4o: 35.7$ for API 22315 calls
c1_gpt4o = cost(nprompt=1, ntok_in=ntok_in, ntok_out=ntok_out, 
                price1k_in = 0.005, price1k_out = 0.015)

avg_c1 = mean(c1_gpt35,c1_gpt4t,c1_gpt4o)

writeLines(paste0("gpt-3.5-turbo-0125: ",c1_gpt35,"$ for one API call."))
writeLines(paste0("gpt-4-turbo: ",c1_gpt4t,"$ for one API call."))
writeLines(paste0("gpt-4o: ",c1_gpt4o,"$ for one API call."))
writeLines(paste0("Average cost: ",avg_c1,"$ for one API call."))

# gpt-3.5-turbo-0125: 0.000204$ for one API call.
# gpt-4-turbo: 0.00408$ for one API call.
# gpt-4o: 0.00204$ for one API call.






###########################
# Cost saving Moral Machine
###########################

# Cost of a 12 minute survey response with California minimum wage $16.00/hour 
c1_human = 12 * 16/60      # $3.20    
c1_llm   = 13*c1_gpt35     # $0.002652

# Function to calculate the percent saved when complementing human with silicon subjects
pcost = function(.rho, .cX, .cf, .cY){
  
  # cost of silicon sampling as a share of sampling human responses
  gamma = (.cX + .cf) / .cY
  
  # check is rho sufficiently large
  is_sufficient = .rho > (2*sqrt(gamma)) / (1+gamma)
  if(!is_sufficient) warning("Rho is not sufficiently large.")
  
  pcost = .rho^2 - gamma * .rho^2 - 2*sqrt(gamma * .rho^2 * (1-.rho^2))
  
  return(pcost)
}

# 10.35% saved
psaving = pcost(.rho=0.35, .cX=0, .cf=c1_llm, .cY=c1_survey)

# 165.63$ saved in a n=500 study
500*c1_human * psaving
