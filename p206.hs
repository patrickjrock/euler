alternate [] = []
alternate (a:b:cs) = a:(alternate cs)
alternate l = id l

candidate n = (alternate $ show $  n^2) == "1234567890" 
