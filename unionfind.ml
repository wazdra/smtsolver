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
	  {
            rank : int A.t;
            mutable parent : int A.t;
            mutable disjoin_class : IntSet.t A.t
          }
	
	let create n =
	  {
            rank = A.init (n+1) (function _ -> 0);
            parent = A.init (n+1) (function i -> i);
	    disjoin_class = A.init (n+1) (function _ -> IntSet.empty)
          }
	
	let find ufd i =
		let rec aux acc i j =
			match A.get ufd.parent j with
			| k when k = j -> (List.iter (function k -> ufd.parent <- A.set ufd.parent k j) acc; j)
			| k -> aux (i::acc) j k
		in
		let j = A.get ufd.parent i in aux [] i j
	
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
			let ci' = IntSet.map (find ufd) ci in
			if ufd.disjoin_class <- A.set ufd.disjoin_class ri ci'; IntSet.mem rj ci' then
				raise Impossible_action
			else
				let cj' = IntSet.map (find ufd) cj in
				begin
                                  ufd.disjoin_class <- A.set ufd.disjoin_class rj cj';
                                  if (A.get ufd.rank ri) > (A.get ufd.rank rj) then
                                    {
                                      ufd with 
                                      parent = A.set ufd.parent rj ri;
                                      disjoin_class = A.set ufd.disjoin_class ri (IntSet.union ci' cj')
                                    }
                                  else if (A.get ufd.rank rj) > (A.get ufd.rank rj) then
                                    {
                                      ufd with
                                      parent = A.set ufd.parent ri rj;
                                      disjoin_class = A.set ufd.disjoin_class ri (IntSet.union ci' cj');
                                    }
                                  else (*rank i = rank j *)
                                    {
                                      rank = A.set ufd.rank ri ((A.get ufd.rank ri) +1);
                                      parent = A.set ufd.parent rj ri;
                                      disjoin_class = A.set ufd.disjoin_class ri (IntSet.union ci' cj');
                                    }
				end
	
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
			{
                          ufd with
                          parent = ufd.parent;
                          disjoin_class = A.set (A.set ufd.disjoin_class ri ci') rj cj';
                        }
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

                                     
