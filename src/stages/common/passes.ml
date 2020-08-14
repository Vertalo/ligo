open Types
open Trace

(* Types level *)
let row : ('a -> ('b,_) result) -> 'a sum -> ('b sum,_) result
= fun g row ->
  Helpers.bind_map_lmap 
  (fun {associated_type;michelson_annotation;decl_pos} ->
    let%bind associated_type = g associated_type in
    ok @@ ({associated_type;michelson_annotation;decl_pos}: 'b row_element)
  ) row

let arrow : ('a -> ('b,_) result) -> 'a arrow -> ('b arrow,_) result
= fun g {type1;type2} ->
  let%bind type1 = g type1 in
  let%bind type2 = g type2 in
  ok @@ {type1;type2}

(* Expression level *)

let constant : ('a ->  ('b,_) result) -> 'a constant -> ('b constant,_) result
= fun f {cons_name;arguments} ->
  let%bind arguments = bind_map_list f arguments in
  ok @@ {cons_name;arguments}

let constructor : ('a -> ('b,_) result) -> 'a constructor -> ('b constructor,_) result
= fun f {constructor;element} ->
  let%bind element = f element in
  ok @@ {constructor; element}

let application : ('a -> ('b,_) result) -> 'a application -> ('b application,_) result
= fun f {lamb;args} ->
  let%bind lamb = f lamb in
  let%bind args = f args in
  ok @@ {lamb; args}

and binder : ('a -> ('b, _) result) -> 'a binder -> ('b binder, _) result
= fun f {var; ty} ->
  let%bind ty = f ty in
  ok @@ {var; ty}

let lambda : ('a -> ('b, _) result) -> ('c -> ('d, _) result) -> ('a,'c) lambda -> (('b,'d) lambda , _) result 
= fun f g {binder=b;result}->
  let%bind binder = binder g b in
  let%bind result = f result in
  ok @@ {binder;result}

let path : ('a -> ('b,_) result) -> 'a access list -> ('b access list, _) result
= fun f path ->
  let aux a = match a with
    | Access_record s -> ok @@ Access_record s
    | Access_tuple  i -> ok @@ Access_tuple  i
    | Access_map e ->
      let%bind e = f e in
      ok @@ Access_map e
  in
  bind_map_list aux path

let record : ('a -> ('b,_) result) -> 'a label_map -> ('b label_map,_) result
= fun f record ->
  Helpers.bind_map_lmap f record 

let recursive : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) recursive -> (('b,'d) recursive, _) result
= fun f g {fun_name;fun_type;lambda=l} ->
  let%bind fun_type = g fun_type in
  let%bind lambda = lambda f g l in
  ok @@ {fun_name;fun_type;lambda}

let accessor : ('a -> ('b,_) result) -> 'a accessor -> ('b accessor, _) result
= fun f {record;path=p} ->
  let%bind record = f record in
  let%bind path   = path f p in
  ok @@ ({record;path} : 'b accessor)

let update : ('a -> ('b,_) result) -> 'a update -> ('b update, _) result
= fun f {record;path=p;update} ->
  let%bind record = f record in
  let%bind path   = path f p in
  let%bind update = f update in
  ok @@ ({record;path;update} : 'b update)

let sequence : ('a -> ('b,_) result) -> 'a sequence -> ('b sequence, _) result
= fun f {expr1;expr2} ->
  let%bind expr1 = f expr1 in
  let%bind expr2 = f expr2 in
  ok @@ {expr1;expr2}

let ascription : ('a -> ('b,_) result) -> ('c -> ('d,_) result) -> ('a,'c) ascription -> (('b,'d) ascription, _) result
= fun f g {anno_expr; type_annotation} ->
  let%bind anno_expr = f anno_expr in
  let%bind type_annotation = g type_annotation in
  ok @@ {anno_expr; type_annotation}

let raw_code : ('a -> ('b,_) result) -> 'a raw_code -> ('b raw_code, _) result
= fun f {language;code} ->
  let%bind code = f code in
  ok @@ {language;code}

let conditional : ('a -> ('b,_) result) -> 'a conditional -> ('b conditional, _) result
= fun f {condition;then_clause;else_clause} ->
  let%bind condition   = f condition in
  let%bind then_clause = f then_clause in
  let%bind else_clause = f else_clause in
  ok @@ {condition;then_clause;else_clause}
