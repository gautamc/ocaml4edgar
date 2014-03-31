val sort_and_group_file :
  skip_rows:int ->
  master_file:string ->
  field_index:int -> field_to_pair:int -> string list list
val merge_via_collapsing_same_groups :
  accum:string list list -> in_list:string list list -> string list list
