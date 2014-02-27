(*Q-1*)

(*ASSUMPTION ON  INPUT: n>=0 and r>1;
 REPRESENTATIONAL INVARIANT ON OUTPUT=[a_k-1,a_k-2,...,a_1,a_0] where a_i is (i+1)th element from backside ;
RECURSION INVARIANT IS :"m=n*(r)^p+y where p=no. of elements in the list and y=sum_{i=0 to k-1} (a_i)*r^i"
TIME COMPLEXITY=O(log_(r)n) where n=number and r=base; 
SPACE COMPLEXITY=O(log_(r)n) where n=number and r=base; *)



let  ntolist(m,r) =let rec repnr=function
      (0,r,l)->0::l
|     (n,r,l)->repnr(n/r,r,(n mod r::l))
in repnr(m,r,[]);;





(*Q-2*)
(* using Idea Horner's Rule*)
(*ASSUMPTION ON  INPUT: Input is a list of digit means lists except empty list ;
REPRESENTATIONAL INVARIANT ON OUTPUT="n=sum_{i=0 to k-1} (a_i)*r^i";
RECURSION INVARIANT IS "n=sum_{i=0 to k-1} (a_i)*r^i ";
TIME COMPLEXITY=O(n) where n=number of elements in the input list; 
SPACE COMPLEXITY=O(n) where n=number of elements in the input list *)
exception Notdefined;;

let head=function
[]->raise Notdefined |
(x::xs)->x;;

let tail=function
[]->raise Notdefined |
(x::xs)->xs;;



let rec list_to_n(l,r)= if l=[] then raise Notdefined 
                             else if tail(l)=[] then head(l) 
                                  else if tail(tail(l)) = [] then (head(l)*r+head(tail(l)))    else                          let modifylist(l,r)=[(head(l)*r+head(tail(l)))]@tail(tail(l)) in list_to_n(modifylist(l,r),r);;



(*Q-3-a*)
(*ASSUMPTION ON  INPUT:list should not be empty;
REPRESENTATIONAL INVARIANT ON OUTPUT:[a_k-1,a_k-2,...,a_1,a_0] where a_i is (i+1)th element from backside;
TIME COMPLEXITY=O(log_(2)n+O(log_(10)n)) where n=no. of elements in the input list;
SPACE COMPLEXITY=O(n) where n=no. of elements in the input list i.e.,list with radix 2*)
let convert (l,2,10)=  ntolist(list_to_n(l,2),10);;



(*Q-3-b*)
(*ASSUMPTION ON  INPUT:list should not be empty and r>1;
REPRESENTATIONAL INVARIANT ON OUTPUT:[a_k-1,a_k-2,...,a_1,a_0] where a_i is (i+1)th element from backside;
TIME COMPLEXITY=O(log_(r)n+O(log_(d)n)) where n=no. and r = base;
SPACE COMPLEXITY=O(n) where n=no. of elements in the list with lower radix *)
let convertgen l r d= ntolist(list_to_n(l,r),d);;

