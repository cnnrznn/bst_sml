(* tree type of int (non-generic) *)
datatype tree = null | node of (tree * int * tree);

(* tree functions *)
fun tree_insert(u:tree, i:int):tree =
    case u of
        null => node(null, i, null)
        | node(l, k, r) => 
            if i < k then
                node(tree_insert(l, i), k, r)
            else
                node(l, k, tree_insert(r, i))

fun tree_print(u: tree):unit =
    case u of
        null => ()
        | node(l, k, r) =>
            (
              tree_print(l);
              print(Int.toString(k) ^ ", ");
              tree_print(r)
            )

fun tree_from_file(r:tree, f:string):tree =
  let
    val ins:TextIO.instream = TextIO.openIn("data.txt");
    fun helper(u:tree, s:string option, ins:TextIO.instream) =
        case s of
          NONE => u
          | SOME l =>
            case Int.fromString(l) of
              NONE => u
              | SOME i => helper(tree_insert(u, i),
                                  TextIO.inputLine(ins), ins)
  in
    helper(null, TextIO.inputLine(ins), ins)
  end

(* SCRIPT *)
val root:tree = tree_from_file(null, "data.txt");
tree_print(root)
