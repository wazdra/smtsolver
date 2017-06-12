exception Impossible_action

module type UnionFind = sig
  type t
  val create : int -> t
  val find :  t -> int -> int
  val union : t -> int  -> int -> t
  val disjoin : t -> int -> int -> t
end

module type PersistentArray = sig
  type 'a t
  val init : int -> (int -> 'a) -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
end

module IntSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = int
  end 
)

module Make(A : PersistentArray) : UnionFind = struct
	type t =
	{mutable parent : int A.t; disjoin_class : IntSet.t A.t}
	
	let create n =
		{parent = A.init (n+1) (function i -> i);
		disjoin_class = A.init (n+1) (function _ -> IntSet.empty)}
	
	let rec find ufd i =
		match A.get ufd.parent i with
		| j when j = i -> i
		| j -> 
			let k = find ufd j in 
			begin
			ufd.parent <- A.set ufd.parent i k;
			k
			end	
	
	let union ufd i j =
		let ri = find ufd i in
		let rj = find ufd j in
		if ri = rj then 
			ufd
		else
			let ci = A.get ufd.disjoin_class ri in
			let cj = A.get ufd.disjoin_class rj in
			let (ri, rj, ci, cj) = 
				if IntSet.cardinal cj < IntSet.cardinal ci then
					(rj, ri, cj, ci)
				else
					(ri, rj, ci, cj)
			in
			if IntSet.exists (function x -> find ufd x = rj) ci then
				raise Impossible_action
			else
				{parent = A.set ufd.parent rj ri; disjoin_class = 
				A.set ufd.disjoin_class ri (IntSet.union ci cj)}
	
	let disjoin ufd i j =
		let ri = find ufd i in
		let rj = find ufd j in
		if ri = rj then
			raise Impossible_action
		else
			let ci = A.get ufd.disjoin_class ri in
			let ci' = IntSet.add rj ci in
			let cj = A.get ufd.disjoin_class rj in
			let cj' = IntSet.add ri cj in
			{parent = ufd.parent; disjoin_class =
			A.set (A.set ufd.disjoin_class ri ci') rj cj'}
end

module PersArr : PersistentArray = struct
  type 'a t = 'a data ref
  and 'a data =
    |Arr of 'a array
    |Diff of int * 'a * 'a t

  let init n f = ref (Arr (Array.init n f))
	
	let reroot t =
		let rec aux2 a = function
			| [] ->
				a
			| t::tl ->
				begin match !t with
				| Diff(i,v,t') ->
					let v' = a.(i) in
					begin
					a.(i) <- v;
					t' := Diff(i,v',t);
					aux2 a tl	
					end
				| _ -> assert false
				end
				
		in
		let rec aux1 acc t = match !t with
			| Arr a -> aux2 a acc 
			| Diff(_,_,t') -> aux1 (t::acc) t'
		in
		let a = aux1 [] t in
		t := Arr a
		
  let rec get t i = match !t with
    |Arr a -> a.(i)
    |Diff _ ->
      reroot t;
      begin match !t with
      |Arr a -> a.(i)
      |Diff _ -> assert false
      end
                                  
  let set t i v = 
    reroot t;
    match !t with
    |Arr a as n -> let old = a.(i) in
                   a.(i) <- v;
                   let res = ref n in
                   t := Diff (i,old,res);
                   res
    |Diff _ -> assert false
end

                                     
