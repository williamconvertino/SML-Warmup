functor F (M: ORD_MAP where type Key.ord_key = string) (S:ORD_SET where type Key.ord_key = string) :>
    sig
        val proc: string list -> S.set M.map
    end =
    struct
        val proc = 
            fn (nameList: string list) =>
                let
                    val finalMap: S.set M.map ref = ref M.empty

                    

                    fun processLine (line, fileName) = 
                        let
                            val words = String.tokens Char.isSpace line
                            fun processWord word =
                                case M.find(!finalMap, word) of
                                    SOME set => finalMap := M.insert(!finalMap, word, S.add(set, fileName))
                                |   NONE => finalMap := M.insert(!finalMap, word, S.add(S.empty, fileName))
                        in
                            map processWord words
                        end

                    fun processFile (fileName) =
                        let
                            val file = TextIO.openIn fileName
                            val read = while not (TextIO.endOfStream file) do 
                                case (TextIO.inputLine file) of
                                    SOME line => processLine(line, fileName)
                                |   NONE => []
                            val closeFile = TextIO.closeIn file
                        in
                            ()
                        end
                        
                    val iterateFileNames = map processFile nameList
                    
                in
                    !finalMap
                end
    end

structure STR_ORD = 
    struct 
        type ord_key = string
        val compare = String.compare
    end

structure mySet : ORD_SET = ListSetFn (STR_ORD)
structure myMap : ORD_MAP = ListMapFn (STR_ORD)

structure myStructure = F (myMap) (mySet)

val test = myStructure.proc ["a.txt", "b.txt"]