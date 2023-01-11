piSequence:: Float -> [Float]
piSequence k = [((-1)**(n+1)) * (4/ ((2*n)-1))| n <- [1..k]]

sampleSeq:: Float -> [Float]
sampleSeq k = [1/(2**n) | n <- [0..k]]

math_series:: (Float -> [Float]) -> Float -> Float
math_series func k = sum (func k) 

testFunc2 x = x * ((x**(-1)) * (x/1000))
testFunc x = x * 0.5

integral:: (Float -> Float) -> Float -> Float -> Float -> Float
integral func start end granularity = sum [(func x) * ((end-start)/granularity)| x <- [start,(start + ((end-start)/granularity))..(end - ((end-start)/granularity))]]