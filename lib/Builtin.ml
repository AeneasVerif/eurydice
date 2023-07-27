module K = struct
  include Krml.Ast
end

let slice: K.lident = ["Eurydice"], "slice"
let slice_len: K.lident = ["Eurydice"], "slice_len"
let slice_new: K.lident = ["Eurydice"], "slice_new"
let slice_of_array: K.lident = ["Eurydice"], "slice_of_array"

let range: K.lident = ["Eurydice"], "range"

let array_copy = ["Eurydice"], "array_copy"
