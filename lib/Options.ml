let log_level = ref ""
let config = ref ""
type cg = Runtime | Static
let cg = ref Static
let runtime_cg () = !cg = Runtime
