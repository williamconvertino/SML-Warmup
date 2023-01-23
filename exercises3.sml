functor F (M: ORD_MAP where type Key.ord_key = string) (S:ORD_SET where type Key.ord_key = string) :>
    sig
        val proc: string list -> S.set M.map
    end =
    struct
        fun proc (nameList: string list) =
            let
                fun addLineToMap (line, fileName, initialMap) =
                    let
                        val words = String.tokens Char.isSpace line
                        fun addWordToMap (word, currentMap) =
                            case M.find(currentMap, word) of
                                    SOME set => M.insert(currentMap, word, S.add(set, fileName))
                                |   NONE => M.insert(currentMap, word, S.add(S.empty, fileName))
                    in
                        foldl addWordToMap initialMap words
                    end

                fun readFile (file, fileName, currentMap) =
                    case (TextIO.inputLine file) of
                        SOME line => readFile(file, fileName, addLineToMap (line, fileName, currentMap))
                    |   NONE => currentMap

                fun processFile (fileName, initialMap) =
                    let
                        val file = TextIO.openIn fileName
                        val finalMap = readFile(file, fileName, initialMap)
                        val () = TextIO.closeIn file
                    in
                        finalMap
                    end
            in
                foldl processFile M.empty nameList
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

val test = myStructure.proc ["a.txt", "b.txt", "c.txt"]