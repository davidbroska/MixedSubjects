cost = function(nprompt, ntok_in, price1k_in, ntok_out, price1k_out, verbose=T){
  
  p_in  = price1k_in / 1000
  p_out = price1k_out / 1000

  dollars = round(nprompt*(ntok_in * p_in + ntok_out * p_out),2)

  return(dollars)
}

# description of scenario 
ntok_in  = 390  
# output
ntok_out = 6    
# survey responses from 2097 users
nprompt = 22315 

# pricing: https://openai.com/api/pricing/

# gpt-3.5-turbo-0125: 3.57$ for API 22315 calls
c1 = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
          price1k_in = 0.0005, price1k_out = 0.0015)

# gpt-4-turbo: 71.41$ for API 22315 calls
c2 = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
          price1k_in = 0.01, price1k_out = 0.03)

# gpt-4o: 35.7$ for API 22315 calls
c3 = cost(nprompt=nprompt, ntok_in=ntok_in, ntok_out=ntok_out, 
          price1k_in = 0.005, price1k_out = 0.015)

writeLines(paste0("gpt-3.5-turbo-0125: ",c1,"$ for API ",nprompt," calls."))
writeLines(paste0("gpt-4-turbo: ",c2,"$ for API ",nprompt," calls."))
writeLines(paste0("gpt-4o: ",c3,"$ for API ",nprompt," calls."))
writeLines(paste0("Total cost: ", sum(c(c1,c2,c3)),"$."))

# gpt-3.5-turbo-0125: 4.55$ for API 22315 calls.
# gpt-4-turbo: 91.05$ for API 22315 calls.
# gpt-4o: 45.52$ for API 22315 calls.
# Total cost: 141.12$.