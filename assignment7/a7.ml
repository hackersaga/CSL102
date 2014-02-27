module type DYNAMIC =sig                                                                                                                  
 val share : float array -> unit
val reachability : int array array -> int array array 
 val cheapest : int array array -> int array array
 val maxflow :  'a array array -> 'a array array   
 val min_step : int -> int 
end;;

 module D:DYNAMIC = struct
(*Q-1*)
exception No_Profit ;;
(*INPUT SPECIFICATION:a float array which has share prices of the (i+1)th day*)
(*OUTPUT SPECIFICATION:output is profit and  the day on which it should be purchased and sold*)
(*TIME COMPLEXITY: O(n^2)*)
(*SPACE COMPLEXITY:O(n) where n= no. of days*)
let share a= let  n=(Array.length a)-1 and profit = ref(0.,(-1,-1)) and  k=ref(1) in  
              for  i=0 to n do 
              let v_i=a.(i)  in
              while (!k)<(n+1) do 
              let check_profit = (a.(!k)-.v_i)  and (p,q)=(!profit) in 
              if check_profit>=p   then  profit:=(check_profit,(i,(!k)))  else ();
              k:=(!k)+1
             done;
             k:=(i)+2
             done;
let (x,(y,z))=(!profit) in if x<=0. then raise No_Profit else 
print_string "profit  = ";print_float x ; print_newline ();
print_string "bought on the day = ";print_int (y+1) ; print_newline ();
print_string "sold on the day = ";print_int (z+1)
;;

                 

(*Q-2(A)*)
let intersection(a,b) = match (a,b) with
                (1,1)->1
                |(_,_)->0;;
                
(*INPUT SPECIFICATION: adjancy matrix*)
(*OUTPUT SPECIFICATION: adjacency matrix which defines if there is a path from i to j directly or ind*)
(*TIME COMPLEXITY: O(n^3)*)
(*SPACE COMPLEXITY:O(n^2)*) 
let reachability a= let n=(Array.length a)-1 in 
                   for k=0 to (n) do
                   for i=0 to (n) do
                   for j=0 to (n) do 
                   if a.(i).(j)=1 then a.(i).(j)<-a.(i).(j) else a.(i).(j)<-intersection((a.(i).(k)),a.(k).(j))
                   done;
                   done;
                   done;
                   a;;
          

 
(*Q-2(B)*)
                  
let min(a,b)= if a<=b then a else b;;
let max(a,b)= if a>=b then a else b;;

(*TIME COMPLEXITY: O(n^3)*)
(*SPACE COMPLEXITY:O(n^2)*)                      
let cheapest a= let n=(Array.length a)-1 in 
                   for k=0 to (n) do
                   for i=0 to (n) do
                   for j=0 to (n) do 
                   let t=(a.(i).(k)+a.(k).(j)) in
                   a.(i).(j)<-min(a.(i).(j),t)
                   done;
                   done;
                   done;
                   a;;
 (*e.g.-let c=[|[|0;4;1|];[|0;0;0|];[|0;1;0|]|];;*)         
          
 (*Q-2(c)*)   
(*TIME COMPLEXITY: O(n^3)*)
(*SPACE COMPLEXITY:O(n^2)*)             
let maxflow a= let n=(Array.length a)-1 in 
                   for k=0 to (n) do
                   for i=0 to (n) do
                   for j=0 to (n) do 
                   let t= min(a.(i).(k),a.(k).(j)) in
                   a.(i).(j)<-max(a.(i).(j),t)
                   done;
                   done;
                   done;
                   a;;
                   



                 
 (*Q-3*)     
(*function which tells  min. no. of step for "n" floors and "2" eggs*)
(*TIME COMPLEXITY:O(n) where n is min. no. of step*)
(*SPACE COMPLEXITY:k*)
let min_step n = let rec floor(n,d)= match n with
                                  0->(d-1)
                                  |n->if n<0 then (d-1) else floor((n-d),d+1)
                 in floor(n,1) ;;            

   
   end;;
   
                          
                                            
