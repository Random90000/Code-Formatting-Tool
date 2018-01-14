let rec huffman p =
 match p with 
  (fx,x) ::(fy,y) ::ls -> huffman (qsort ((fx + fy, Node (x,y)) ::ls))
  |[(_,x)] -> x
  |[] -> failwith "1"
       
       
     
     
       
       
     
     