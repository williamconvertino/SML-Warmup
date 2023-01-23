
structure S : ORD_SET = ListSetFn (
    struct 
        type ord_key = string
        val compare = String.compare
    end
);

structure M : ORD_MAP = ListMapFn (
    struct 
        type ord_key = string
        val compare = String.compare
    end
);

fun proc (nameList: string list) =
    let
        val myMap = M.empty

        fun processLine (line) = 
            String.tokens Char.isSpace line

        fun processFile (fileName) =
            let
                val file = TextIO.openIn fileName
                val read = while not (TextIO.endOfStream file) do 
                    case (TextIO.inputLine file) of
                        SOME line => processLine(line)
                    |   NONE => []
                val closeFile = TextIO.closeIn file
            in
                ()
            end
            
        val iterateFileNames = map processFile nameList
        
    in
        ()
    end;

proc(["a.txt", "b.txt", "c.txt"]);