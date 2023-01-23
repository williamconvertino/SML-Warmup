functor F (M: ORD_MAP where type Key.ord_key = string) (S:ORD_SET where type Key.ord_key = string) :>
    sig
        val proc: string list -> S.set M.map
    end =
    struct
        fun proc nameList =
            let
                    
                val finalMap: S.set M.map ref = ref M.empty

                fun processLine (line, fileName, initialMap) = 
                let
                    val words = String.tokens Char.isSpace line
                    fun processWord(word, finalMap) =
                        case M.find(finalMap, word) of
                            SOME set => M.insert(finalMap, word, S.add(set, fileName))
                        |   NONE => M.insert(finalMap, word, S.add(S.empty, fileName))
                    in
                        foldl processWord initialMap words
                    end
                fun processFile (fileName, initialMap) =
                    let
                    val file = TextIO.openIn fileName
                    fun helper currMap = 
                       case TextIO.inputLine file of
                            SOME line => helper(processLine(line, fileName, currMap))
                        |   NONE => currMap
                    val ans = helper initialMap
                    val () = TextIO.closeIn file
                    in
                        ans
                    end
                in
                    foldl processFile M.empty nameList
                end
    end 

structure StrOrd = 
struct 
    type ord_key = string
    val compare = String.compare
end                    

structure mySet : ORD_SET = ListSetFn (StrOrd)

structure myMap : ORD_MAP = ListMapFn (StrOrd)

structure myStructure = F (myMap) (mySet)

val x = myStructure.proc ["a.txt", "b.txt"]