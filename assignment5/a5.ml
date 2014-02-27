type comparision = Equal|Lesser|Greater;;
type 'a tree = Leaf | Node of 'a*'a tree*'a tree;;


module type ORDERED_TYPE= sig
type t
val compare: t->t->comparision
end;;

module Sorted=
functor (T:ORDERED_TYPE)(*as input is of ordered type*)->struct



(*===============================================================================================================================*)
           
                
(*"INSERTION SORT STARTS"*)
(*"INSERTION HELPER FUNCTION which insert an element to a given sorted list"*)
let rec insert x l=match l with
    []->[x]
   |z::xs->match T.compare x z  with
   Equal->x::l
   |Lesser->x::l
   |Greater->z::(insert x xs)
   
 (*"INSERTION sort FUNCTION"*) 
 (*"input specification":'a list
"output specification":'a list
"recursive call invarients":insort(x::xs)=(insert x (insort (xs)))
"time complexity":O(n^2)
"space complexity":O(n) in both cases n is length of list
*) 
let rec insort l = match l with
        []->[](*empty list is already sorted*) 
        |[x]->[x]       
        |x::xs->(insert x (insort (xs)))
(*"INSERTION SORT ENDS"*)    (*=================================================================================================================================*)  
exception Notdefined

(*"MERGE SORT STARTS"*)
(*"FUNCTION WHICH MERGES TWO ALREADY SORTED LIST (*IN ASCENDING ORDER*) TO GIVE A SORTED LIST IN ASCENDING ORDER"*)
(*O(n+m) is T.C.*)
let merge l1 l2 = let rec join(l1,l2,c) = match (l1,l2) with
                   ([],[])->c(*merging two empty list a again a empty list*)
                  |(x::xs,[])->c@(x::xs)
                  |([],y::ys)->c@(y::ys)
                  |(x::xs,y::ys)->match T.compare x y with
                                                   Equal->join(xs,ys,c@[x;y])(*  x=y*)
                                                   |Lesser->join(xs,l2,c@[x])(*x<y*)
                                                   |Greater->join(l1,ys,c@[y]) in join(l1,l2,[])(*x>y*)
       
 (*"FUNCTION WHICH GIVES LENGTH OF THE LIST"*) (*T.C.=O(n)*) 
let rec length =function
      []->0
|     (p::ps)->1+length(ps)
(*"FUNCTION WHICH TAKES FIRST n ELEMENTS OF GIVEN LIST"*)(*T.C.=O(n) *)
 let  take n l = let rec tak(n,l,c) = match l with
                       []-> raise Notdefined
                       |x::xs->if n=0 then c  else tak((n-1),xs,c@[x]) in tak(n,l,[]) ;;
 (*"FUNCTION WHICH drops FIRST n ELEMENT OF THE LIST"*)   (*T.C.=O(n) *)                   
 let rec drop n l  = match l with
                          []->raise Notdefined
                        |x::xs->if n=0 then x::xs  else (drop (n-1) (xs)) ;;
                               
               

                          
(*"REAL MERGESORT FUNCTION WHICH SORTS A GIVEN LIST IN ASCENDING ORDER"*) 
(*"input specification":'a list
"output specification":'a list
"recursive call invarients":mergesort l=(merge (mergesort l1) (mergesort l2)) where l1 list which contains first n/2 elements of the list & l2 contains all elements except first n/2 elements of the input list 
"time complexity":O(nlog_2(n))
"space complexity":O(n) in both cases n= length of list
*)   
let rec mergesort l = match l with
        []->[](*ALREADY a sorted list*)
        |[x]->[x](*ALREADY a sorted list*)
       |x::xs->let m=length(l)/2 in 
               let l1= take m l(*break the list in two approx. equal halves*)
               and l2= drop m l in
               let s1= mergesort l1(* recuring for breaked list*)
               and s2= mergesort l2 in 
               merge s1 s2 (*merging two sorted lists*)
 (*"MERGESORT ENDS"*)
 (*=========================================================================================================================*)
     (*"QUICKSORT STARTS"*)
     (*"input specification":'a list
"output specification":'a list
"recursive call invarients":quicksort(x::xs)=quicksort(l1)@x::quicksort(l2), where l1 contains all elements of xs which are less than x and l2 greater or equal to x
"time complexity":time complexity lies in the interval [O(nlog(n)),O(n^2)] where two extremes are time for best(*elements distributes uniformly e.g.-[4;3;2;1;5;6;7] etc.*) and worst(*input list=rev(outout list) e.g.-[5;4;3;2;1] etc.*) case
"space complexity":O(n) where n= length of list
*)             
let rec quicksort l= match l with
        []->[](*ALREADY a sorted list*)
       |[x]->[x](*ALREADY a sorted list*)
       |x::xs-> let rec partition (l1,l2,l3)= match l3 with(* a helper function defined*)
                   []->quicksort(l1)@x::quicksort(l2)(* defining base case which can recur quicksort on l1 & l2*)
                   |y::ys->match T.compare y x with 
                   Lesser->partition(y::l1,l2,ys) (* comparing elements in such a way that all the element less then a particular element are sent to l1 and others to l2 and recur till  l3 gets empty*)
                   |(_)-> partition(l1,y::l2,ys)
                   in partition([],[],xs) 
  (*"QUICKSORT ENDS"*)
(*============================================================================================================================*)   
   
   (*"SELECTION SORT STARTS"*)
   
   (*this function tells us two things:(1).minimum element in the list & (2).list except that min. element*)
   let rec min(l,c) = match l with
             []->raise Notdefined(*min. don't exist*)
             |x::[]->(x,c)(* min. element is single element*)
             |x::y::xs->match T.compare x y with 
    Lesser-> min((x::xs),c@[y]) (*if x<y then  save y at other place and recur with x & xs as min(x,y)=x 'here'*)                             
             |(_)->  min(y::xs,c@[x])  (* remaining case*)
             
(*"input specification":'a list
"output specification":'a list
"recursive call invarient":selection_sort l= a::(selection_sort (b)) where a= min. element of l and b=list except that min. element
"time complexity":O(n^2)
"space complexity":O(n) in both cases n=length of the list
*)             
 let rec selection_sort l= match l with
                   []->[](*ALREADY a sorted list*)
                   |x::xs->let (a,b)=min(l,[]) in
                           a::(selection_sort (b)) (* keeping the min. element at the first place recurring sorting of list in ascending oreder*)
      (*" SELECTION SORT ENDS"*) 
  (*===============================================================================================================================*)    
 (*"HEAP SORT STARTS"*)
 exception  NotExist

 
 (*"1st FUNCTION" which can tell us leftmost element in a heap & can give a heap which without the leftmost element named as"leftrem"*)
 (*O(log_2(n)) is T.C.*)
 let rec leftrem  t = match t with
         Leaf->raise NotExist
        |Node(x,Leaf,Leaf) -> (x,Leaf)
        |Node(x,t1,t2)->let (y,t3)=leftrem(t1) in
                       (y,Node(x,t2,t3))              
 
 
 
 (*"2nd FUNCTION" which can form a heap from an element & two heaps named as"siftdown"*)
 exception Not_a_heap
 (*T.C.-O(log_2(n) n=max(n1,n2)  where n1 & n2 are no. of elements in two heaps*)       (*S.C.-O(n1+n2)*)
 let rec siftdown x t1 t2 = match (t1,t2) with
            (Leaf,Leaf)->Node(x,Leaf,Leaf)(*we can insert 'x' anywhere in the Empty 'tree'*)
            |(Leaf,_)->raise Not_a_heap(* input does not satisfies the property of a heap*)
            |(Node(y,Leaf,Leaf),Leaf)->( match T.compare y x with(*compareing x & y that whichever be the min. will be the node*)
                                        Lesser(*x<y*)-> Node(y,Node(x,Leaf,Leaf),Leaf) 
                                         |(_)(*x>=y*)-> Node(x,Node(y,Leaf,Leaf),Leaf))
            |(Node(y1,l1,m1),Node(y2,l2,m2)) -> let a=T.compare x y1  and b=T.compare x y2 in  
                                                match (a,b) with(*x<=y1 & x<=y2*)
                                                (Equal,Equal)->Node(x,t1,t2)
                                               |(Lesser,Equal)->Node(x,t1,t2)
                                               |(Equal,Lesser)->Node(x,t1,t2)
                                               |(Lesser,Lesser)->Node(x,t1,t2)
            (*except the case 'x<=y1 & x<=y2'*)|(_,_)-> match T.compare y1 y2 with
                                               (*y1>y2*)Greater->Node(y2,t1,(siftdown x l2 m2))
                                              (*y1<=y2*)|(_)->Node(y1,(siftdown x l1 m1),t2)
                                                        
                                              
 (*"3th FUNCTION" which deletes the min. element in the tree named as"delmin"*)
 (*T.C. - O(log_2(n))*)
 let rec delmin t = match t with
                Leaf->raise NotExist
               |Node(x,Leaf,_)->Leaf
               |Node(x,t1,t2)->let (y,t3)=leftrem(t1)  
               in (siftdown y t2 t3) 
               
               
 (*"4th FUNCTION" which forms a heap of first   'n' elements of a list function named as"heapify"*)
 (*T.C.-O(log_2(n))*)
 let rec heapify n l= match (n,l) with
             (0,_)->(Leaf,l)(*taking no element*)
             |(n,x::xs)->let (t1,vs1)=(heapify (n/2) (xs)) 
                         in let (t2,vs2) = (heapify ((n-1)/2) (vs1))
                         in ((siftdown x t1 t2),vs2)
                         
   (*"5th FUNCTION" which converts a list to a heap function named as"fromlist"*)                       
let fromlist vs=let m=length(vs) in let (t,_) =(heapify m vs) in t

   (*"6th FUNCTION" which converts a tree a to a list function named as"tolist"*) 
   
   
   (*T.C.-O(log_2(n))*)
     let rec tolist t = match t with
     Leaf -> [] (*leaf contains no element*)
     |Node(x,_,_) -> x::(tolist(delmin(t))) (* x cons with function recurring on  a heap except the first node*)
     
   (*"input specification":'a list
"output specification":'a list
"time complexity":O(nlog_2(n))
"space complexity":O(n) in both cases n=length of the list
*)  
     
     let heapsort l = match l with
     []->[]
     |x::xs->tolist(fromlist(l))                            
          
      (*"HEAP SORT ENDS"*) (*===============================================================================================================================*)      
(*input assumption:- list should be sorted in ascending order*)
  
   let rec tobaltree l = match l with
         []->Leaf
         |l->let m=length(l) in
             let l1= take (m/2) l
             and l2=drop (m/2) l
             in let y::ys=l2 in
             Node(y,tobaltree(l1),tobaltree(ys))
                 
  (*" BINARY SEARCH TREE STARTS"*)
(*"input specification":'a->'a tree
"output specification":bool
"space complexity":O(n): n is length of list
*)
(*tree is of ordered type*)
(*we are checking whether an element is a member of a balanced tree or not*)
let rec bin_search x p= match p with
          Leaf->false(*x can't be the member of empty tree*)
         |Node(a,t1,t2)->match T.compare x a with
  Equal-> true(*x=a => x is a member of tree*)
 |Lesser->(bin_search x t1)(*node element are greater then all elements to the left of it and less then all elements to the right of it*)
 |Greater->(bin_search x t2) 
 
 let check x l = let p=tobaltree(l) in bin_search x p
  (*"BINARY SEARCH TREE ENDS"*)              
(*===============================================================================================================================*)                 
                 
                 
                          
end ;;

module OrderedInt =
   struct
     type t = int 
     let compare x y = if x = y then Equal else if x < y then Lesser else Greater
   end;;
      
 module I = Sorted(OrderedInt) 
                  
                  
 
module OrderedFloat =
   struct
     type t = float 
     let compare x y = if x = y then Equal else if x < y then Lesser else Greater
   end;;
      
 module F = Sorted(OrderedFloat) 
                  
 
module OrderedChar =
   struct
     type t = char 
     let compare x y = if x = y then Equal else if x < y then Lesser else Greater
   end;;
      
 module C = Sorted(OrderedChar)                 
module Insort = functor (I:ASS)->struct
 
module OrderedString =
   struct
     type t = string 
     let compare x y = if x = y then Equal else if x < y then Lesser else Greater
   end;;
      
 module S = Sorted(OrderedString) 
   
   
  (*"(1).NUMBERS ARE GIVEN LESS PRIORITY OVER ALPHABATS OR WORDS & negative strings are given less priority over positive string
     (2). FLOATS ARE GIVEN MORE IMPORTANCE OVER INTEGER
     (3).SMALL ALPHABATES are given more preferences then big alphabates "*) 
(*" int list"
[0;0;0;0;0;0;2;2;22;2;2;2;2;-4;-3;55;-0];;

[0;9;7;6;8;2;3;4;5;6;7;1;11;12;15;13;14;16;19;18;17;-5;-4;-3;-2;-1];;

[1;1;1;1;1;2;2;2;2;2;3;3;3;3;3;4;4;4;4;4;5;5;5;5;5;6;6;6;6;6;7;7;7;7;7;8;8;8;8;8;9;9;9;9;9;0;0;0;0;0;11;11;11;11;11;12;
12;12;12;12;13;13;13;13;13;14;14;14;14;14;15;15;15;15;15]

        [-15; -15; 15; 15; 15; 14; 14; -14; -14; 14; 13; -13; -13; 13; 13; -12; 12; -12; 12;
         12; -11; 11; 11; -11; 11; 0; -0; 0; -0; 0; -9; 9; 9; 9; -9; 8; 8; 8;-8; -8; -7; 7;
         7; 7; -7; 6; 6; 6; -6; -6; 5; 5; -5; -5; 5; 4; -4; 4; 4; -4; -3; 3; 3; 3; -3; -2; 2;
         2; -2; 2; 1; -1; 1; -1; 1]*)                  

(*"float list"
[2.;5.;5.;5.;5.;7.;8.;0.;1.;5.;3.;2.;4.;9.;-0.;-55;-1000]
[15.;15.;15.;15.;-15.;-14.;14.;14.;14.;14.;13.;13.;-13.;13.;13.;-12.;12.;12.;12.;12.;11.;-11.;11.;11.;11.;10.;
-10.;10.;10.;10.;9.;-9.;9.;9.;9.;8.;8.;-8.;8.;8.;7.;7.;7.;7.;-7.;6.;6.;-6.;6.;6.;5.;5.;-5.;5.;5.;4.;4.;-4.;4.;
4.;3.;3.;-3.;3.;3.;2.;2.;-2.;2.;2.;1.;-1.;1.;1.;1.;0.;0.;-0.;0.;0.;0.;*)

(*"char list:=>  list of 'whole no.' or 'alphabates' or both"    
['z';'n';'p';'d';'c';'q';'s';'t';'v';'y';'x';'m';'r';'o';'u';'w';'a';'b';'e';'g';'f';'h';'k';'l';'i';'j';
'a';'g';'m';'s';'z';'S';'T';'U';'V';'';'A';'D';'B';'C';'E';'M';'R';'P']

['z';'n';'p';'1';'d';'c';'q';'s';'0';'t';'v';'y';'0';'x';'m';'9';'r';'o';'8';'u';'6';'w';'a';'7';'b';'e';
'5';'g';'f';'h';'2';'k';'l';'4';'i';'j';'3';'a';'g';'m';'s';'z']
*)

(*"string list":=> list of any combnation of "int" or "alphabates" or "float" or "words"

(*["aaaIAMURSUPERHERO";"am";"-am";"aam";"bla-bla";"chill";"1.1";"0";"don";"-a";"9";"0.0";"eagle";"6";"9.9";"Aisa";"8";"football";"5";"7";"ghanshyam";"4";"hurre";"3";"a";"zandubam";"2";"-2";"-2.2";"b";"c";"rocks";"8.8";"7.7";"6.6";"-ksagar";"5.5";"4.4";"king";"3.3";"2.2";"jaiho";"1.1";"vijayho";"champs";"terror";"ur";"prem";"iamdon";"WANNABEMYCHHAMMAKCHALLO";"z";"c";"n";"p";"thanks";"welcome"]*)

(*["A1";"B2";"C3";"D4";"a1"; "a2"s] *)
*)







