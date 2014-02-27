type sign = P|N ;;
type l = int list;;


module type BIGINT = sig
type bigint =  B of sign*l
val tobigint:  int -> bigint 
val toint: bigint -> int
val addb: bigint * bigint -> bigint  (* addition *)
val subb: bigint * bigint -> bigint (* subtraction *) 
val multb: bigint * bigint ->bigint 
val divb: bigint * bigint ->bigint            
val modb: bigint * bigint ->bigint                                           
val absb: bigint -> bigint (* absolute value *)
val equal: bigint * bigint -> bool
val greater: bigint * bigint -> bool
val lesser: bigint * bigint -> bool


end;;

module B:BIGINT = struct

type bigint =  B of sign*l;; 
(*-------------------------*)

(*"helper function satrts"*)
(*--------------------------------*)




exception Notdefined;;

let head=function
[]->raise Notdefined |
(x::xs)->x;;

let tail=function
[]->raise Notdefined |
(x::xs)->xs;;

let  rev(l) =let rec rev2=function
       ([],l)->l
|      (x::xs,l)->rev2(xs,x::l)
in rev2(l,[]);;



(*--------------------------*)
let rem(l) = let p = rev(l) in let rec remzero(p,c) = match p with
     []->c
     |x::xs->  if x=0 then remzero(xs,c) else p 
     in remzero(p,[]) ;;

let rmdzero(l)= rev(rem(l));;

(*considering list in REVERSED ORDER OF "addb & subb" function*)
(*assuming l1>=l2*)


let hd = function
 []->0
|x::xs->x;;

let tl = function
[]->[]
|x::xs->xs;;

let rec number =function
      []->0
|     (p::ps)->1+number(ps);;

(* "helper function ends"*)
(*=========================================================*)

(*"input  list should be in reverse order"*)
let   lton(l)  = let p=rev(l) in  let  rec      lton2(p,m)  =  if   p=[]  then   m   else lton2(tail(p),(m*2+head(p))) in lton2(p,0);;

 let toint = function
  B(P,l)->1*lton(l)
| B(N,l)->(-1)*lton(l);;


(*=======================================================================*)
(*output list have REVERSED ORDER *)
let  ntolist(m) = let rec repnr=function
      (0,l)->l
      |(n,l)->repnr(n/2,(l@[(n mod 2)])) in repnr(m,[]);;

let tobigint(m) =if m>=0 then  B(P,ntolist(m)) else B(N,ntolist(-m)) ;;

(*================================================================*)
(*input-output lists are in the reversed order*)
let absb = function
 B(P,l)->B(P,l)
|B(N,l)->B(P,l);;
(*=======================================================================*)

(*in the EQUAL function input list is in "NON-REVERSED ORDER"*)

(*in the function EQUAL, GREATER, LESSER input list is in "NON-REVERSED ORDER"*)
let rec equal = function
|(B(P,[]),B(P,[]))->true
|(B(P,l1),B(N,l2))->false
|(B(N,l1),B(P,l2))->false 
|(B(P,l1),B(P,l2))-> let p=number(l1) and q=number(l2) in if p>q then false else if p<q then false else   if  hd(l1)>hd(l2) then false else if  hd(l1)<hd(l2)  then false else  equal(B(P,tail(l1)),B(P,tail(l2)))
|(B(N,l1),B(N,l2))->equal(B(P,l1),B(P,l2));;
(*=========================================================*)

(* gives true if first one is bigger*)
let rec greater = function
(B(P,l1),B(N,l2))->true
|(B(N,l1),B(P,l2))->false
|(B(P,[]),B(P,[]))->false
|(B(N,[]),B(N,[]))->false
|(B(P,l1),B(P,l2))-> let p=number(l1) and q=number(l2) in if p>q then true else if p<q then false else if hd(l1)>hd(l2) then true else if hd(l1)<hd(l2) then false  else  greater(B(P,tl(l1)),B(P,tl(l2)))  
|(B(N,l1),B(N,l2))->let p=number(l1) and q=number(l2) in  if p>q then false else if p<q then true else  if hd(l1)<hd(l2) then true else if hd(l1)>hd(l2)  then false  else  greater(B(N,tl(l1)),B(N,tl(l2))) ;;


let lesser = function
(B(P,l1),B(N,l2))->false
|(B(N,l1),B(P,l2))->true
|(B(P,l1),B(P,l2))->greater(B(N,l1),B(N,l2))
|(B(N,l1),B(N,l2))->greater(B(P,l1),B(P,l2));;


(*=========================================================*)

let rec modify(l)=match l with
         []->[]
         |l->if hd(tl(l))=1 then ([1;0]@tl(tl(l))) else            ([1]@modify(tl(l))) ;;
 
 (*"subtraction of lists"*)
 (*input&output list are in reverse order*)
(* THIS GIVE ANSWER OF (L1-L2)*)
let sub(l1,l2)=let rec subl(l1,l2,c)= match (l1,l2) with
          ([],[])->c
          |(x::xs,[])->c@(x::xs)
          |([],y::ys)->let m=rmdzero(y::ys) in if m=[] then c else raise Notdefined
          |(m::ms,n::ns)->let x::xs=rmdzero(m::ms) and y::ys=rmdzero(n::ns) in let s=(x-y) in  if s>=0 then subl(xs,ys,c@[s]) else subl(tl(modify(x::xs)),ys,c@[1])
          in subl(l1,l2,[]) ;;
(*test of substraction function:
sub([0;1;1],[1;0;1]);;
sub([1;0;0;1],[0;1;1]);;*)  
(*================================================================*)
 
(*" addition of lists"*)
let add(l1,l2)=let rec addl=function
([],[],a,b)->b@[a]
|(l1,l2,a,b)->let s=(hd(l1)+hd(l2)+a) in addl(tl(l1),tl(l2),s/2,b@[s mod 2]) in addl(l1,l2,0,[]);;
(*============================================================*)

(*ASSUMPTION ON INPUT & OUTPUT LISTS ARE IN REVERSED ORDER*)
let addb = function
(B(P,l1),B(P,l2))->B(P,add(l1,l2))
|(B(N,l1),B(N,l2))->B(N,add(l1,l2))           
|(B(N,l1),B(P,l2))->if greater(B(P,l1),B(P,l2)) then  B(N,sub(l1,l2))
                     else B(P,sub(l2,l1))
|(B(P,l1),B(N,l2))->if greater(B(P,l1),B(P,l2)) then B(P,sub(l1,l2))
          else              B(N,sub(l2,l1))    
                       ;;

(*========================================================================*)
  

(*ASSUMPTION ON INPUT & OUTPUT LISTS ARE IN REVERSED ORDER*)
let subb=function
(B(P,l1),B(P,l2))->if greater(B(P,l1),B(P,l2)) then B(P,sub(l1,l2)) else
                   B(N,sub(l2,l1))
|(B(N,l1),B(N,l2))->if greater(B(P,l1),B(P,l2)) then B(N,sub(l1,l2))    else        B(P,sub(l2,l1))
|(B(N,l1),B(P,l2))->B(N,add(l1,l2))
|(B(P,l1),B(N,l2))->B(P,add(l1,l2))
;;

(*=======================================================================*)
(*"input-output lists are in reversed order"*)

(*insert as many zeros to a list as I want*)      
let rec zero(n,l)=if n=0 then l else zero((n-1),[0]@l);;

(*it  is  a helper function of multlist function*)       
let  mult(l1,l2) =let l3=rev(l1) in let rec multl(l1,l2,c) = match (l1,l2) with
              ([],[])->[]
              |([],x::xs)->[]
              |(l1,[])->c
              |(x::xs,y::ys)->if y=1 then
multl(x::xs,ys,add(c,zero(number(ys),l3))) else
multl(x::xs,ys,c) 
in multl(l1,l2,[]) ;;

let multlist(l1,l2) = let m=rev(l1) and n=rev(l2) 
                     in rmdzero(mult(m,n)) ;;
(*test of multplication function:
multlist([0;1;1],[1;0;1]);;
 multlist([1;0;0;1],[0;1;1]);;*)
let multb = function
(B(P,l1),B(P,l2))-> B(P,(multlist(l1,l2)))
|(B(N,l1),B(N,l2))->B(P,(multlist(l1,l2)))          
|(B(N,l1),B(P,l2))->B(N,(multlist(l1,l2)))
|(B(P,l1),B(N,l2))->B(N,(multlist(l1,l2)))
;;

(*==================================================================*)

             
 (*THIS GIVES ANSWER OF L1/L2*)      
let  div(l1,l2) = let rec divl(l1,l2,c) = match (l1,l2) with
      ([],[])->raise Notdefined
      |(x::xs,[])->raise Notdefined
      |([],x::xs)->[]
      |(l1,l2)->let m=rmdzero(l1)  and n=rmdzero(l2) in if equal(B(P,m),B(P,n)) then add(c,[1]) else if  
      lesser(B(P,m),B(P,n)) then c else   
      divl(sub(l1,l2),(l2),add(c,[1]))
      in divl(l1,l2,[0]);;
 
 let divt(l1,l2)=rmdzero(div(l1,l2));;
      
 let divb = function
(B(P,l1),B(P,l2))->B(P,(divt(l1,l2)))
|(B(N,l1),B(N,l2))->B(P,(divt(l1,l2)))           
|(B(N,l1),B(P,l2))->B(N,(divt(l1,l2)))
|(B(P,l1),B(N,l2))->B(N,(divt(l1,l2)))
;;
 
 
 
 (*==============================================================*)
 
          
 let  modl(l1,l2) = sub(l1,multlist(divt(l1,l2),l2));;
 
  let modb = function
(B(P,l1),B(P,l2))->B(P,modl(l1,l2))
|(B(N,l1),B(N,l2))->B(P,modl(l1,l2))           
|(B(N,l1),B(P,l2))->B(N,modl(l1,l2))
|(B(P,l1),B(N,l2))->B(N,modl(l1,l2))
;;

end;;


