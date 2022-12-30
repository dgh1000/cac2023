
ps = [0,2,3,5,7,9,11] :: [Int]

main = do
  let scaleIdx = -7
      pIdx = scaleIdx `mod` length ps
      pOct = scaleIdx `div` length ps

  print (pIdx,pOct,60+12*pOct+(ps!!pIdx)) 

  
