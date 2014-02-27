let time f x =
let t = Sys.time() in
let f_x = f x in
Printf.printf "Took %fs\n" (Sys.time() -. t);;


module type ARRAY =sig                                                                                                                  
val insort : 'a array -> 'a array 
val bubblesort : 'a array -> 'a array 
val selection_sort : 'a array -> 'a array 
val bin_search : 'a -> 'a array -> int
val quicksort : 'a array -> 'a array 

end;;

module A:ARRAY = struct
          
(*"INPUT SPECIFICATION":'a array which is not possibly sorted*)
(*"OUTPUT SPECIFICATION":'a array which is sorted in ascending order*)
(*"TIME COMPLEXITY":"BEST CASE":O(n) in the case when the array is already sorted*)
(*"TIME COMPLEXITY":"WORST CASE"O(n^2) in the case when the array is sorted in descending order in both cases n=Array length*)

let insort a = let n= (Array.length a)-1   in
               for i= (n-1) downto 0 do(*"FOR LOOP INVARIENT":all elements right to the (!j)th element are sorted in increasing order*)
                       let v_i=a.(i) and 
                       j=ref(i) in
                                 while !j<n & v_i>a.(!j+1) do (*"WHILE LOOP INVARIENT":fo all p a.(p)<=v_i where i<=p<(!j)*)
                                       a.(!j)<-a.(!j+1);
                                       j:=(!j)+1
                                 done;
                       a.(!j)<-v_i
                 done;
         a;;
       
          
(*"INPUT SPECIFICATION":'a array which is not possibly sorted*)
(*"OUTPUT SPECIFICATION":'a array which is sorted in ascending order*)
(*"TIME COMPLEXITY":"BEST CASE":O(n^2) in the case when the array is already sorted*)
(*"TIME COMPLEXITY":"WORST CASE"O(n^2) in the case when the array is sorted in descending order in both cases n=Array length*)

       
let bubblesort a = let n=(Array.length a)-1 in
                    for i= n downto 0 do(*"FOR LOOP INVARIENT":everything right to ith element  is sorted*)
                    let j=ref(0) in 
                    while (!j)<i do(*"WHILE LOOP INVARIENT":a.(!j-1)<=a.(!j) explanation: at the completion of one step think???*) 
                    if a.(!j)>a.((!j)+1) then 
                    let t=a.(!j) in
                    a.(!j)<-a.((!j)+1);
                    a.((!j)+1)<-t
                    else ();
                    j:=((!j)+1)
                    done;
                    done;
                    a;;
                    
          
(*"INPUT SPECIFICATION":'a array which is not possibly sorted*)
(*"OUTPUT SPECIFICATION":'a array which is sorted in ascending order*)
(*"TIME COMPLEXITY":"BEST CASE":O(n) in the case when the array is already sorted*)
(*"TIME COMPLEXITY":"WORST CASE"O(n^2) in the case when the array is sorted in descending order in both cases n=Array length*)
                    
let selection_sort a = let n=(Array.length a)-1 in
                    for i=n downto 0 do (*"FOR LOOP INVARIENT":everything right to ith element  is sorted*) 
                    let max=ref(i) in
                    for j=i downto 0 do(*"FOR LOOP INVARIENT":a.(!max) is maximum in all elements from j to n  *)
                    if a.(j)>a.(!max) then
                    max:=j else ()
                    done;
                    let t=a.(i) in
                     a.(i)<-a.(!max);
                     a.(!max)<-t
                     done;
                     a;;
                     
          
(*"INPUT SPECIFICATION":'a->'a array where 'a array is posiibly sorted*)
(*"OUTPUT SPECIFICATION":'a array which is sorted in ascending order if output is negative then it means it is not found else it gives the position of element*)
(*"TIME COMPLEXITY":"BEST CASE":x is the mid element of array which gives answer in constant time *)
(*"TIME COMPLEXITY":"WORST CASE":log_2(n) where n=array length*)
                                   
let bin_search x a= let  n= (Array.length a)-1  in
                   let found=ref(-1) and left=ref(0) and right=ref(n) in
                   while ((!left)<=(!right)) & (!found)=(-1) do(*"WHILE LOOP INVARIENT":we are comparing x with mid element*)
                   let mid=(!left + (!right))/2 in
                   if x=a.(mid) then  found:=(mid) else if x>a.(mid) then left:=(mid)+1 else right:=(mid)-1
                   done;
                   (!found) ;;
                   
(*helper function of quicksort which gives two arrays first one containing all elements less than a.(0) and other containing >= a.(0)*)
let rec partition_array(a,n,c,d) = match n with 
                                0->(c,d)
                               |n->if a.(0)>a.(n) then partition_array(a,(n-1),(Array.append c [|a.(n)|]),d)    
                                    else partition_array(a,(n-1),c,(Array.append d [|a.(n)|]))           ;;
                                        
          
(*"INPUT SPECIFICATION":'a array which is not possibly sorted*)
(*"OUTPUT SPECIFICATION":'a array which is sorted in ascending order*)
(*"TIME COMPLEXITY":"BEST CASE":n(log_2(n)) in the case when the elements of array distributes to two helper array evenly e.g.-[4;3;2;1;5;6;7] etc.*)
(*"TIME COMPLEXITY":"WORST CASE":O(n^2) in the case when input list is sorted in descending order e.g.-[5;4;3;2;1] etc.*)
                                                                            
let rec quicksort l= match l with
        [||]->[||](*ALREADY a sorted array*)
       |[|x|]->[|x|](*ALREADY a sorted array*)
       |a->let n=(Array.length a)-1 in  let (p,q)=partition_array(a,n,[||],[||]) in
            (Array.append (Array.append (quicksort(p)) [|a.(0)|]) (quicksort(q)))                        ;;
                                                         
                                                         
                                                         
                                 end;;                     
                                                         
          
(*[|100; 99; 98; 97; 96; 95; 94; 93; 92; 91; 90; 89; 88; 87; 86; 85; 84; 83;
  82; 81; 80; 79; 78; 77; 76; 75; 74; 73; 72; 71; 70; 69; 68; 67; 66; 65; 64;
  63; 62; 61; 60; 59; 58; 57; 56; 55; 54; 53; 52; 51; 50; 49; 48; 47; 46; 45;
  44; 43; 42; 41; 40; 39; 38; 37; 36; 35; 34; 33; 32; 31; 30; 29; 28; 27; 26;
  25; 24; 23; 22; 21; 20; 19; 18; 17; 16; 15; 14; 13; 12; 11; 10; 9; 8; 7; 6;
  5; 4; 3; 2; 1|]*)

