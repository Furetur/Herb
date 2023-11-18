open Base
open Ir

let build_map blocks =
  let blocks_by_label =
    Map.of_list_with_key
      (module Label.Comparator)
      blocks
      ~get_key:(fun b -> b.label)
  in
  match blocks_by_label with
  | `Duplicate_key label ->
      Printf.failwithf "There are multiple blocks with the label '%s'"
        (Label.show_label label) ()
  | `Ok x -> x

let remove_unreachable_blocks ({ entry_block; blocks; _ } as func_body) =
  let blocks_by_label = build_map (entry_block :: blocks) in
  let get_block label = Map.find_exn blocks_by_label label in

  let rec dfs visited block =
    if Set.mem visited block.label then visited
    else
      let visited = Set.add visited block.label in
      match block.terminator with
      | Jump label -> dfs visited (get_block label)
      | CondBranch { if_true; if_false; _ } ->
          let visited = dfs visited (get_block if_true) in
          let visited = dfs visited (get_block if_false) in
          visited
      | _ -> visited
  in

  let reachable = dfs (Set.empty (module Label.Comparator)) entry_block in
  let in_order = List.filter blocks ~f:(fun b -> Set.mem reachable b.label) in
  { func_body with blocks = in_order }

let cleanup { entry } = { entry = remove_unreachable_blocks entry }
