module type SET = sig
type 'a set 
val emptyset: 'a set
val member: 'a -> 'a set -> bool
val subseteq: 'a set -> 'a set -> bool
val seteq: 'a set -> 'a set -> bool
val union: 'a set -> 'a set -> 'a set
val intersection: 'a set -> 'a set -> 'a set
val difference: 'a set -> 'a set -> 'a set
val power: 'a set -> 'a set set
val cartesian: 'a set -> 'b set -> ('a*'b) set
end;;

module SET:SET = struct
(*Q-1*)
type 'a set = S of 'a list ;;
(*Q-2*)let emptyset = S([]);;
(*------------------*)
 exception InvalidSet;;
exception Notdefined;;

let head=function
[]->raise Notdefined |
(x::xs)->x;;

let tail=function
[]->[] |
(x::xs)->xs;;

let rec number =function
      []->0
|     (p::ps)->1+number(ps);;

            
           
(*-------------------------*)
(*THE "MEMBER" FUNCTION:
    INPUT SPECIFICATION:'a -> 'a set               OUTPUT SPECIFICATION: bool
 TIME COMPLEXITY:O(n) where n is no. of elements in l;  SPACE COMPLEXITY:O(n)where n is no. of elements in l;
*)
 let rec mem(x,l) =if l=[] then false else if x=head(l) then true else mem(x,tail(l)) ;;
 (*Q-3*)
let member x (S(l)) =   mem(x,l) ;;
(*---------------------*)
(*TIME COMPLEXITY:O(n) where n is no. of elements in l;
 SPACE COMPLEXITY:O(n) where n is no. of elements in l;*)
let rec checkdup(l) = match l with
           []->false
           |x::xs->if (mem(x,xs))=true then true else checkdup(xs) ;;


 (*-------------------------*)
 
 (*function "seteq" tells us whether two given sets are equal or not:
INPUT SPECIFICATION:'a set -> 'a set               OUTPUT SPECIFICATION:bool
 TIME COMPLEXITY:O(m*n) where m & n is no. of elements in l1 & l2;
 SPACE COMPLEXITY:O(m+n) where m and n is no. of elements in l1 & l2 *)
 let rec check(l1,l2) = match (l1,l2) with 
              ([],[])->true
             |(x::xs,[])->true
             |([],x::xs)->false
             |(l1,y::ys)-> if (mem(y,l1))=false then          false   else check(l1,ys);;
 
 let seteq (S(l1)) (S(l2)) = if checkdup(l1)=true  or checkdup(l2) = true  then raise InvalidSet else check(l1,l2) ;;
 (*------------------------*)
 
 (*it tells whether S(l2) is subset or equal of S(l1)
 INPUT SPECIFICATION:'a set -> 'a set                      
  OUTPUT SPECIFICATION: bool
         TIME COMPLEXITY:O(m*n) where m and n is no. of elements in l1 & l2  ;
         SPACE COMPLEXITY:O(m+n) where m and n is no. of elements in l1 & l2 *)
 let rec sub(l1,l2)= if l2=[] then true else if mem(head(l2),l1)=false then false else sub(l1,tail(l2));;
 
 let subseteq (S(l1)) (S(l2)) = if checkdup(l1)=true or checkdup(l2) = true then raise InvalidSet else sub(l1,l2) ;;
 
 (*------------------------*)
 (*the "union" function returns us union of two sets:
  INPUT SPECIFICATION:'a set -> 'a set                                                 OUTPUT SPECIFICATION: 'a set
         TIME COMPLEXITY:O(m*n) where m and n is no. of elements in l1 & l2 ;
         SPACE COMPLEXITY:O(m+n) where m and n is no. of elements in l1 & l2;*) 
 let rec uni(l1,l2,c) = if l2=[] then (S(c@(l1)))  else if (mem(head(l2),l1)) = false then uni(l1,tail(l2),c@[head(l2)]) else uni(l1,tail(l2),c);;
 
 let union (S(l1)) (S(l2)) = if checkdup(l1)=true or checkdup(l2) = true then raise InvalidSet else uni(l1,l2,[]);;
 (*------------------------------*)
 (*the function "intersection" returns intersection of two sets :
INPUT SPECIFICATION: 'a set -> 'a set                                                 OUTPUT SPECIFICATION:'a set
         TIME COMPLEXITY:O(m*n) where m and n is no. of elements in l1 & l2 ;;
         SPACE COMPLEXITY:O(m+n) where m and n is no. of elements in l1 & l2;*)
 let rec inter(l1,l2,c) = if l2=[] then (S(c@[])) else if (mem((head(l2),l1)))=false then inter(l1,tail(l2),c) else inter(l1,tail(l2),c@[head(l2)]) ;;
 
 let intersection (S(l1)) (S(l2)) = if checkdup(l1)=true or checkdup(l2) = true then raise InvalidSet else inter(l1,l2,[]) ;;
 
 (*---------------------------*)
  

(* ASSUMPTION ON INPUT: number of elements in l1 should be greater than that of l2: 
INPUT SPECIFICATION: 'a set -> 'a set                                                 OUTPUT SPECIFICATION:'a set
         TIME COMPLEXITY:O(m*n) where m and n is no. of elements in l1 & l2;
         SPACE COMPLEXITY:O(m+n) where m and n is no. of elements in l1 & l2;*)
let rec difhelp(l1,l2,c)= match (l1,l2) with
                   ([],[])->[]
                   |([],x::xs)->c
                   |(x::xs,[])->  c@(x::xs)
                   |(x::xs,y::ys)->if (mem(x,y::ys))=true
                             then difhelp(xs,y::ys,c) else  difhelp(xs,y::ys,c@[x]) ;;
                  
 let  difference (S(l1)) (S(l2)) =   if checkdup(l1)=true or checkdup(l2) = true then raise InvalidSet else S(difhelp(l1,l2,[])) ;;
 
 (*-----------------------*)
 (*INPUT SPECIFICATION:'a set                                                OUTPUT SPECIFICATION:'a set set
         TIME COMPLEXITY:O(2^n) where n=no. of elements in input list;
         SPACE COMPLEXITY:*)
 let rec map f l  = match l with 
                   []->[]
                   |x::xs-> f x::(map f xs) ;;
                   
 
 let rec pover l = match l with 
                []->[S[]]
               |x::xs-> let p=pover (xs) and con a (S(l))= (S(a::l)) in (p@(map (con x) (p)))  ;;
               
  let power (S(l)) = (S(pover (l)));;
  
 (*------------------------------*)
 (*INPUT SPECIFICATION: 'a set -> 'b set                                                 OUTPUT SPECIFICATION: ('a * 'b) list
         TIME COMPLEXITY:O(m*n) where m and n is no. of elements in l1 & l2;;
         SPACE COMPLEXITY:O(m*n) where m and n is no. of elements in l1 & l2*)
 (*O(n)*)

 
 let rec pair(x,l,c) = match l with
                  []-> c 
                  |y::ys->pair(x,ys,((x,y)::c)) ;;
       (*O(l1)+O(l2)*)           
 let rec cart(l1,l2,c) = match (l1,l2) with
         (l,[])->[]
         |([],x::xs)->c
         |(x::xs,y::ys)->cart(xs,y::ys,(pair(x,y::ys,[]))@c);;
         
 let cartesian (S(l1)) (S(l2)) = if checkdup(l1) or checkdup(l2) = true then raise InvalidSet else S(cart(l1,l2,[])) ;;
         
 (*-----------------------*)
   
  end ;;
  
 
