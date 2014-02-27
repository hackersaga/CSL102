module type FLOAT= sig
type vector = float list
type matrix = float list list
val length: vector -> int
val dotprod: vector * vector -> float
val scale: float -> vector -> vector
val scalarMult: float -> matrix -> matrix
val numRows: matrix -> int
val numCols: matrix -> int
val isSquare: matrix -> bool
val transpose: matrix -> matrix
val add: matrix -> matrix -> matrix
val id: int -> matrix 
exception NotMultipliable
val multiply: matrix -> matrix -> matrix                                               
val determinant: matrix -> float
exception NotInvertible
val invert: matrix -> matrix

end ;;
(*---------------*)
module F:FLOAT  = struct
(*------------------------*)
type vector = float list 
;;
(*---------------------------*)
type matrix = float list list
;;
(*-----------------------------------------------*)
exception Notdefined
;; 
                            
let head = function
[]->raise Notdefined
|x::xs->x
;;
let tail = function
[]->[]
|x::xs->xs
;;
(*-------------*)
(*" length of vector = no. of elements in flaot list"*)
(*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: vector
OUTPUT SPECIFICATION:int
RECURSION INVARIENT:((no. of elements in l)+c)
TIME COMPLEXITY:O(n) where n=length of input list
SPACE  COMPLEXITY:O(n) where n=length of input list
*)
let  length(l)=let rec lt(l,c) =  match l with
[]->c
|(x::xs)->lt(xs,(c+1))
in lt(l,0)
;;
(*----------------------*)

(*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: vector*vector
OUTPUT SPECIFICATION:float
RECURSION INVARIENT:(c+(sum_{i=0 to k-1}(a_i)*.(b_i)) where k is length of list
TIME COMPLEXITY:O(n) where n=length of shortest input list
SPACE  COMPLEXITY:O(n) where n=sum of length of two input lists
*)

let dotprod(l,m) = let rec dot(l,m,c) = match (l,m) with
          ([],[])-> c
  |(x::xs,[])->c
  |([],y::ys)->c
  |(x::xs,y::ys)->dot(xs,ys,c+.(x*.y))
  in dot(l,m,0.0) ;;
(*--------------------------*)  
  (* "multiplication of a scalar with a vector"*)
(*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: float->vector
OUTPUT SPECIFICATION:vector
RECURSION INVARIENT:(c+(sum_{i=0 to k-1}(a)*.(b_i)) where k is length of list
TIME COMPLEXITY:O(n) where n=length of input list
SPACE  COMPLEXITY:O(n) where n=length of input list
*)

let  scale n l = let rec scalerprod(n,l,c) = match l  with 
[]->c
|x::xs->scalerprod(n,xs,c@[x*.n]) in scalerprod(n,l,[]);;

(*--------------------------*)

(*"multiplication of scalar with a matrix"*)
(*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: float->matrix
OUTPUT SPECIFICATION:matrix
RECURSION INVARIENT:(c+(sum_{i=0 to k-1}(a)*.(b_i)) where k is length of list
TIME COMPLEXITY:O(m*n) where m&n=no. of rows and no. of columns in a matrix
SPACE  COMPLEXITY:O(n) where n=size of matrix
*)

let  scalarMult n l = let rec matscalar(n,l,c) = match l with
[]->c
|[[]]->c
|x::xs->matscalar(n,xs,c@[(scale (n) x)]) 
in matscalar(n,l,[]) ;;
(*--------------------------*)
(*"calculating no. of rows in a matrix"*)
(*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: matrix
OUTPUT SPECIFICATION:int
RECURSION INVARIENT:(c+length(l))
TIME COMPLEXITY: O(m) where m= no. of rows in a matrix
SPACE  COMPLEXITY: O(m) where m= no. of rows in a matrix
*)
  
  let  numRows l = let rec row(l,c) =  match l with
  []->c
  |[[]]->c
  |x::xs->row(xs,(c+1))
  in row(l,0) ;;
(*--------------------------*)  
  (*"calculating no. of columns in a matrix"*)
  (*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: matrix
OUTPUT SPECIFICATION:int
TIME COMPLEXITY:O(n) where n=no. of columns in a matrix
SPACE  COMPLEXITY:O(n) where n=no. of columns in a matrix
*)


  let numCols(l)  = match l with
  []->0
  |[[]]->0                            
  |x::xs->length(x);;
  (*--------------------------*)
  
  (*"finding whether given matrix is a square matrix or not"*)
  (*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: matrix
OUTPUT SPECIFICATION:bool
TIME COMPLEXITY:O(n)+O(m) where n=no. of columns & m = no. of rows in a matrix
SPACE  COMPLEXITY:O(n) where n = no. of columns  + no. of rows in a matrix
*)

let isSquare(l) = if numRows(l) = numCols(l) then true else false;;
(*-----------------------------*)

(*function which takes first column of a given matrix*)
let rec trans(l,c) =match l with
[]->c 
|[[]]->c 
|x::xs->trans(xs,c@[head(x)]) ;;
(* function which gives a matrix leaving first column of a given matrix*)
let rec tl(l,c)= match l with
[]->c
|[[]]->c
|x::xs->tl(xs,c@[tail(x)]) ;;

(*"finding transpose of a given matrix"*)
(*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: matrix
OUTPUT SPECIFICATION:matrix
RECURSION INVARIENT:c@tran(l) 
TIME COMPLEXITY: O(m*(n)^2) where n= no. of rows and m=no. of columns in a matrix
SPACE  COMPLEXITY: O(m) where where m= size of a matrix
*)
  
let transpose(l) = let rec tran(l,c) = match l with
[]->c|
[[]]->c
|x::xs-> if x=[] then c else  tran(tl(l,[]),c@[(trans(x::xs,[]))]) in tran(l,[]);;
(*-------------------------------------------------*)
(*function which adds two vector*)
let rec addvec(l,m,c) = match (l,m) with
 ([],[])->c
 |(x::xs,y::ys)->addvec(xs,ys,c@[x+.y])
 |(_,_)-> raise Notdefined;;
 (*"function which adds two matices"*)
 (*INPUT ASSUMPTION: order of two matrices should be same
INPUT SPECIFICATION: matrix->matrix
OUTPUT SPECIFICATION:matrix
RECURSION INVARIENT:c@ad(l,m,c)
TIME COMPLEXITY: O(m^2) where m= no. of rows in a matrix
SPACE  COMPLEXITY: O(m) where m= size of a matrix
*)
 let add l m = let rec ad(l,m,c) = match (l,m) with
           ([],[])->c
           |([[]],[[]]) ->c
           |((x::xs),(y::ys))->ad(xs,ys,c@[addvec(x,y,[])])
           |(_,_)->raise Notdefined
           in ad(l,m,[]) ;;
      (*--------------------*)
   
 (* identity matrix defined*)  
 (*function which creates a list containing 'n' zeros*)
let rec nzero(n,c) = if n=0 then c else nzero((n-1),(0.::c));;
(*function which can insert '1.0' at desired position in above list*)
let rec changelist(n,l,c,d) = if n=0 then c@l else if n=d then c@((1.)::tail(l)) else changelist(n,tail(l),c@[0.],(d+1)) ;;

(*function creates identity matrix*)
let rec identity(n,p,c) = if n=0 then c else 
identity((n-1),p,[changelist(n,p,[],1)]@c) ;;
 (*"function which create identity matrix of a given order"*)
 (*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: int
OUTPUT SPECIFICATION:matrix
TIME COMPLEXITY: O(m^3) where m= order of a matrix
SPACE  COMPLEXITY: O(m^3) where m= order of a matrix
*)
  
let id(n) = let p = nzero(n,[]) in identity(n,p,[]) ;;
(*------------------------------------*)

exception NotMultipliable ;;

(*function which multiplies a matrix by a vector*)
let rec vecmat(l,m,c) = if head(m)=[] then c else if m=[[]] then c else vecmat(l,tl(m,[]),c@[dotprod(l,trans(m,[]))]);;

(*function which multiplies two matrices*)
 let rec mult(l,m,c) =match (l,m) with
 ([],m)->c|
 ([[]],m)->c|
 (x::xs,m)->mult(xs,m,c@[vecmat(x,m,[])]) ;;
 (*"function which multiplies two matrices"*)
 (*INPUT ASSUMPTION: NONE
INPUT SPECIFICATION: matrix->matrix
OUTPUT SPECIFICATION:matrix
*)
  
 let multiply l m =if numCols(l)=numRows(m) then mult(l,m,[])  else raise NotMultipliable;;
 
  (*----------------------------------------*)
 (*"determinant on the path"*)
  
  exception Empty;;
  (*
  let head = function
  []-> 0.
  |x::xs->x ;;
  *)

 let absf n = match n with
        0.0->0.0
        |n-> if n<0.0 then (-1.0)*.n else n;;
         
  
  let rec pivotrow(m) = match m with 
                       []->raise Empty
                       |[[]]-> raise Empty
                       |[x]->x
                       |x::y::xs-> if (absf(head(x)))>=(absf(head(y))) then pivotrow(x::xs) else  pivotrow(y::xs) ;;
                       
           let rec delpivot(m) = match m with
                       []->raise Empty
                       |[[]]-> raise Empty
                       |[x]->[]
                       |x::xs->if x= pivotrow(x::xs) then xs else x::delpivot(xs) ;;
                       
                  let pivot(m) = let rec pivotr(m,j) =match m with 
                  []->raise Empty
                  |[[]]-> raise Empty
                   |[x]->(x,j)
                   |x::y::xs-> if (absf(head(x)))>=(absf(head(y))) then pivotr(x::xs,j) else  pivotr(y::xs,(j+1)) 
                   in pivotr(m,1);;    
                   
                                  
       exception Zero;;
                     
  let rec guasselm(m,c,d) = match m with
   []->raise Empty
   |[[]]->raise Empty
   |[l]->(c,d@[l])
   |ls->let (p::ps,j) = pivot(ls) in if p=0. then raise Zero else 
                                   let rec elemcol(m) = match m with
                                            []->[]
                                            |(y::ys)::gs -> (addvec(ys,(scale (((0.0)-.(y))/.(p)) (ps)),[]))::elemcol(gs)
          in if ((j-1) mod 2)=0 then  guasselm(elemcol(delpivot(ls)),c,d@[(p::ps)]) else 
          guasselm(elemcol(delpivot(ls)),(c+1),d@[(p::ps)]);;
                            
                            
    let  determinant m = if isSquare(m) then let (p,n)=guasselm(m,0,[]) in let rec det(n,c)= 
                       match n with
                   []->if (p mod 2)=1 then (-1.)*.c else c
                   |[[]]->if (p mod 2)=1 then (-1.)*.c else c
                   |(x::xs)::ps-> det(ps,c*.x)  
                                   in det(n,1.0) else raise Notdefined ;;
       (*removes ith row*)                                                                  
let rem(i,m) =if i>numRows(m) then raise Notdefined else 
              let rec remd(i,m,c,d) = match i with
               0->(c@m,d)
               |1->if numRows(m)>=1 then (c@tail(m),head(m)) else raise Notdefined
               |i-> match m with
               []->raise Notdefined
               |[[]]->raise Notdefined
               |x::xs->remd((i-1),xs,c@[x],d)
               in remd(i,m,[],[]);;
        (*del jth element of a row*)       
let rec del(j,l,c,d) = match j with
        0->l
        |j->match l with
           []->raise Notdefined
           |x::xs->if j=c then d@(xs) else del(j,xs,c+1,d@[x]);;
           
               
 let remcol(j,m)=let rec delcol(j,m,c)= match j with
            0->c@m
            |j->match m with
            []->raise Notdefined
            |[[]]->raise Notdefined
            |[x]->c@[del(j,x,1,[])]
            |(x::xs)->delcol(j,xs,c@[del(j,x,1,[])])
            in delcol(j,m,[]);;


            (*gives matrix without ith reow and jth column*)
let without i j m =  let (a,b)=rem(i,m) in remcol(j,a);;

let cofactor i j m = let n=(i+j) in if (n mod 2)= 0 then determinant(without i j m) else (-1.)*.determinant(without i j m);;

let cof_of_j(j,m)=let n=numRows(m) in  let rec cofj(j,m,n,d)= match n with
                                                              0->d
                                                              |n->cofj(j,m,(n-1),(cofactor n j m)::d)
                                          in cofj(j,m,n,[]);;

let  adj(m) = let n=numCols(m) in let rec help(m,n,c)=match n with
                                                      0->c
                                                      |n->help(m,(n-1),cof_of_j(n,m)::c)
                  in help(m,n,[]);;
                                     
exception NotInvertible;;
                                                            
let invert(m) = let d=determinant(m) in 
                 if d=0.0  then raise NotInvertible else 
                 (scalarMult (1./.d) (adj(m)));;
                 

          

end ;;
