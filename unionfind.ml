exception Impossible_action

module type UnionFind = sig
  type t
  val create : int -> t
  val find :  t -> int -> int
  val union : t -> int  -> int -> t
end

module type PersistentArray = sig
  type 'a t
  val init : int -> (int -> 'a) -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
end

module Make(A : PersistentArray) : UnionFind = struct
  type t = {
      rank : int A.t;
      mutable parent : int A.t;
    }
  let create n = {
      rank = A.init n (fun _ -> 0);
      parent = A.init n (fun k -> k);
    }
  let rec find_aux f i acc = match A.get f i with (* Tail-recursive *)
    |finali when i == finali -> (List.fold_left (fun uf s -> A.set uf s finali) f acc),i
    |tempi -> find_aux f tempi (i::acc)


  let find h x =
    let f,cx = find_aux h.parent x [] in
    h.parent <- f;
    cx

  let union h x y =
    let cx = find h x in
    let cy = find h y in
    if cx != cy then begin
        let rx = A.get h.rank cx in
        let ry = A.get h.rank cy in
        if rx > ry then
          {
            h with parent = A.set h.parent cy cx
          }
        else if rx < ry then
          {
            h with parent = A.set h.parent cx cy
          }
        else
          {
            rank = A.set h.rank cx (rx + 1);
            parent = A.set h.parent cy cx
          }
      end else
      h
end

module PersArr : PersistentArray = struct
  type 'a t = 'a data ref
  and 'a data =
    |Arr of 'a array
    |Diff of int * 'a * 'a t

  let init n f = ref (Arr (Array.init n f))
  let rec reroot t = match !t with (* TODO : make it tail-recursive *)
    |Arr _ -> ()
    |Diff (i,v,t') ->
      reroot t';
      begin match !t' with
      |Arr a as n -> let v' = a.(i) in
                     a.(i) <- v;
                     t := n;
                     t' := Diff (i,v',t)
      |Diff _ -> assert false
      end

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

                                     
