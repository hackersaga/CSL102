(*"ALL MAIN FUNCTION INPUT & OUTPUT LISTS ARE IN REVERSED ORDER"*)
type sign = P|N ;;
type l = int list;;
type bigint =  B of sign*l;;
(*helper function*)
exception Notdefined;;

let hd = function
 []->0
|x::xs->x;;


let tl = function
[]->[]
|x::xs->xs;;

(*it gives us reverse of a given list*)
let  rev(l) =let rec rev2=function
       ([],l)->l
|      (x::xs,l)->rev2(xs,x::l)
in rev2(l,[]);;
(*=======================================*)


let rec remzero(l) = match l with
         []->[]
         |x::xs->if x=0 then remzero(xs) else x::xs ;;

(*input&output list are in reverse order*)
let add(l1,l2)=let rec addl=function
([],[],a,b)->b@[a]
|(l1,l2,a,b)->let s=(hd(l1)+hd(l2)+a) in addl(tl(l1),tl(l2),s/2,b@[s mod 2]) in addl(l1,l2,0,[]);;

(*test of addition function:
add([0;1;1],[1;0;1]);;
add([1;0;0;1],[0;1;1]);;*)
 (*=============================================*)
 
 
(* it is helper function of sub*)
let rec modify(l)=if hd(tl(l))=1 then ([1;0]@tl(tl(l))) else            ([1]@modify(tl(l))) ;;


(*input&output list are in reverse order*)
(* THIS GIVE ANSWER OF (L1-L2)*)

let sub(l1,l2)=let rec subl(l1,l2,c)= match (l1,l2) with
          ([],[])->c
          |(x::xs,[])->c@(x::xs)
          |([],y::ys)->let m=rev(y::ys) in let n=remzero(m) in if n=[] then c else raise Notdefined
          |(x::xs,y::ys)->let s=(x-y) in  if s>=0 then subl(xs,ys,c@[s]) else subl(tl(modify(x::xs)),ys,c@[1])
          in subl(l1,l2,[]) ;;

(*test of substraction function:
sub([0;1;1],[1;0;1]);;
sub([1;0;0;1],[0;1;1]);;*) (*================================================*)         
 (* no. of elements in the list*)
let rec length(l) = match l with 
       []->0
       |x::xs->1+length(xs) ;;
       
 (*insert as many zeros to a list as I want*)      
let rec zero(n,l)=if n=0 then l else zero((n-1),[0]@l);;

(*it  is  a helper function of multlist function*)       
let  mult(l1,l2) =let l3=rev(l1) in let rec multl(l1,l2,c) = match (l1,l2) with
              ([],[])->[]
              |([],x::xs)->x::xs
              |(l1,[])->c
              |(x::xs,y::ys)->if y=1 then
multl(x::xs,ys,add(c,zero(length(ys),l3))) else
multl(x::xs,ys,c) 
in multl(l1,l2,[]) ;;

let multlist(l1,l2) = let m=rev(l1) and n=rev(l2) 
                     in (mult(m,n)) ;;
(*test of multplication function:
multlist([0;1;1],[1;0;1]);;
 multlist([1;0;0;1],[0;1;1]);;*)
              
(*======================================*)

    
    let rec number =function
      []->0
|     (p::ps)->1+number(ps);;

let rec equal = function
|(B(P,l1),B(N,l2))->false
|(B(N,l1),B(P,l2))->false 
|(B(P,l1),B(P,l2))-> if number(l1)>number(l2) then false else if number(l1)<number(l2) then false else  if hd(l1)=hd(l2) then true else if  hd(l1)>hd(l2) then false else if  hd(l1)<hd(l2)  then false else                           equal(B(P,tl(l1)),B(P,tl(l2)))
|(B(N,l1),B(N,l2))->equal(B(P,l1),B(P,l2)) ;;



let rec greater = function
(B(P,l1),B(N,l2))->true
|(B(N,l1),B(P,l2))->false
|(B(P,[]),B(P,[]))->false
|(B(N,[]),B(N,[]))->false
|(B(P,l1),B(P,l2))-> if number(l1)>number(l2) then true else if number(l1)<number(l2) then false else if hd(l1)>hd(l2) then true else if hd(l1)<hd(l2) then false  else  greater(B(P,tl(l1)),B(P,tl(l2)))  
|(B(N,l1),B(N,l2))-> if number(l1)>number(l2) then false else if number(l1)<number(l2) then true else  if hd(l1)<hd(l2) then true else if hd(l1)>hd(l2)  then false  else  greater(B(N,tl(l1)),B(N,tl(l2))) ;;


let lesser = function
(B(P,l1),B(N,l2))->false
|(B(N,l1),B(P,l2))->true
|(B(P,l1),B(P,l2))->greater(B(N,l1),B(N,l2))
|(B(N,l1),B(N,l2))->greater(B(P,l1),B(P,l2));;


 (*THIS GIVES ANSWER OF L1/L2*)      
let  div(l1,l2) = let rec divl(l1,l2,c) = match (l1,l2) with
      ([],[])->raise Notdefined
      |(x::xs,[])->raise Notdefined
      |([],x::xs)->[]
      |(l1,l2)->let m=remzero(rev(l1))  and n=remzero(rev(l2)) in if equal(B(P,m),B(P,n)) then c else if  
      lesser(B(P,m),B(P,n)) then c else   
      divl(sub(l1,l2),(l2),add(c,[1]))
      in divl(l1,l2,[1]);;
      
 let  modl(l1,l2) = sub(l1,multlist(div(l1,l2),l2));;



           
