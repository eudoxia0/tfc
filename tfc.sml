structure TFC = struct
    (* Utilities *)

    fun enumerate list =
        ListPair.zip (list, List.tabulate (List.length list, fn i => i))

    (* AST types *)

    type name = string

    datatype escaped_string = EscapedString of string

    datatype word = BoolWord of bool
                  | IntWord of string
                  | NamedWord of name

    datatype type_spec = NamedTypeSpec of name
                       | WordTypeSpec of type_spec list * type_spec list

    datatype definition = WordDefinition of name * type_spec * word list

    type program = definition list

    (* Parsing *)

    structure Parser = struct
        structure ps = Parsimony(ParsimonyStringInput)

        val singleLineComment = ps.seqR (ps.pchar #"\\")
                                        (ps.seqR (ps.many (ps.noneOf [#"\n"]))
                                             (ps.pchar #"\n"))

        val whitespaceParser = ps.choice [ps.pchar #" ",
                                          ps.pchar #"\n",
                                          singleLineComment]

        val ws = ps.many whitespaceParser

        val ws1 = ps.many1 whitespaceParser

        (* Integer constants *)

        val digitParser = ps.anyOf [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]

        val naturalParser = ps.pmap String.implode (ps.many1 digitParser)

        datatype sign = Positive | Negative

        val signParser = let val posParser = ps.seqR (ps.opt (ps.pchar #"+")) (ps.preturn Positive)
                             val negParser = ps.seqR (ps.pchar #"-") (ps.preturn Negative)
                         in
                             ps.or negParser posParser
                         end

        fun applySign (Positive, int) = int
          | applySign (Negative, int) = "-" ^ int

        val integerTextParser = ps.pmap applySign (ps.seq signParser naturalParser)

        val integerParser = ps.pmap IntWord integerTextParser

        (* Symbols *)

        val alpha = "abcdefghijklmnopqrstuvwxyz"
        val alphaup = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        val num = "0123456789"
        val sym = "$#?@~^'."

        val symbolFirstChar = ps.anyOfString (alpha ^ alphaup)
        val symbolChar = ps.anyOfString (alpha ^ alphaup ^ num ^ sym)

        val symbolParser = ps.pmap (fn (c, rest) => String.implode ([c] @ rest))
                                   (ps.seq symbolFirstChar (ps.many symbolChar))

        (* Type specifiers *)

        fun defineTypeSpecParser wordTypeParser =
            ps.choice [ps.pmap NamedTypeSpec symbolParser,
                       wordTypeParser]

        val wordTypeParser =
            let val (typeSpecParser: type_spec ps.parser, r: type_spec ps.parser ref) = ps.wrapper ()
            in
                let val wordTypeParser = ps.pmap WordTypeSpec
                                                 (ps.between (ps.seq (ps.pchar #"(") ws)
                                                             (ps.seq (ps.many (ps.seqL typeSpecParser ws))
                                                                     (ps.seqR (ps.seq (ps.pstring "--") ws)
                                                                              (ps.many (ps.seqL typeSpecParser ws))))
                                                             (ps.pchar #")"))
                in
                    r := defineTypeSpecParser wordTypeParser;
                    wordTypeParser
                end
            end

        val typeSpecParser = defineTypeSpecParser wordTypeParser

        (* Definitions *)

        val wordParser =
            ps.choice [integerParser,
                       ps.pmap NamedWord symbolParser]

        val definitionParser =
            let val namep = ps.seqR (ps.pchar #":")
                                    (ps.seqR ws1
                                             (ps.seqL symbolParser ws1))
                and typep = (ps.seqL wordTypeParser ws1)
                and deflp = ps.many1 (ps.seqL wordParser ws1)
                and endp = (ps.pchar #";")
            in
                ps.pmap (fn (ts, (name, words)) => WordDefinition (ts, name, words))
                        (ps.seq namep (ps.seq typep (ps.seqL deflp endp)))
            end

        val programParser =
            ps.seqR ws
                    (ps.many (ps.seqR ws1 definitionParser))

        fun parseString s =
            case (ps.run definitionParser (ParsimonyStringInput.fromString s)) of
                (ps.Success r) => r
              | f => raise Fail ("Bad parse: " ^ (ps.explain f))
    end

    (* Type checking

    The grammar of types:

      tau = bool
          | int
          | *tau
          | ( tau* -- tau* )
          | named type

    We have a word environment which maps word names to word types.

    A stack effect type ( a b c -- b c ) has two components, which correspond to
    arguments and results in traditional programming languages. Call [a b c] the
    argument types and [b c] the result types.

    To compile a word definition `W`:

    1. Start with the word's stack effect type, `T`. Let the argument types be
       the initial value of the "current stack state". The rest of the type
       checking algorithm is essentially tracking how words affect the stack
       state.

    2. Iterate over the words in the definition in the order of
       appearance. Then, for each word `C`:

       2.1. Get the type of `C` (literals are self-identifying, named words have
            to be searched in the word environment).

       2.2. The argument types of `C` must be a suffix of the current stack
            state. For example, if the type of `C` is ( b c -- c ) and the
            current stack state is [a b c], this is ok. But if the stack state
            is [a b] and the type of `C` is ( a -- c ), then this is a type
            error.

       2.3. Update the current stack state by removing the last `n` types, where
            `n` is the number of argument types in the type of `C`. Then, add
            the result types of `C` to the current word.

            For example, if the stack state was [a b c], and the word had the
            type ( b c -- c c ), then after removing the arguments, the stack
            state looks like [a], and after adding the results, the stack state
            looks like [a c c].

    3. After typechecking the words in the definition, the final stack state
       must be the same as the result types in `T`.

     *)

    datatype ty = Bool
                | Int
                | WordType of ty list * ty list

    datatype word_type = WordTypeSig of ty list * ty list

    (* Rendering types *)

    fun typeToString Bool = "bool"
      | typeToString Int = "int"
      | typeToString (WordType (ps, rs)) =
        "( "
        ^ (String.concatWith " " (map typeToString ps))
        ^ " -- "
        ^ (String.concatWith " " (map typeToString rs))
        ^ " )"

    fun stackStateToString tys =
       "[" ^ (String.concatWith " " (map typeToString tys)) ^ "]"

    (* Type environment *)

    datatype tenv = TEnv of (name * ty) list

    fun findType (TEnv tenv) name =
        case (List.find (fn (n, _) => n = name)
                        tenv) of
            (SOME (_, ty)) => SOME ty
          | NONE => NONE

    fun hasType tenv name =
        Option.isSome (findType tenv name)

    val defaultTypeEnv = TEnv [
        ("bool", Bool),
        ("int", Int)
    ]

    (* Word environment *)

    datatype wenv = WEnv of (name * word_type) list

    fun findWordType (WEnv wenv) name =
        case (List.find (fn (n, _) => n = name)
                        wenv) of
            (SOME (_, ty)) => SOME ty
          | NONE => NONE

    fun hasWord (wenv: wenv) name =
        Option.isSome (findWordType wenv name)

    fun addWord (WEnv wenv) name ty =
        WEnv ((name, ty) :: wenv)

    val defaultWordEnv = WEnv [
            ("NOT", WordTypeSig ([Bool], [Bool])),
            ("+", WordTypeSig ([Int, Int], [Int]))
        ]

    (* Type specifiers to types *)

    fun typeSpecToType (tenv: tenv) (NamedTypeSpec name) =
        (case findType tenv name of
             (SOME ty) => ty
           | NONE => raise Fail "No such type")
      | typeSpecToType tenv (WordTypeSpec (args, res)) =
        WordType (map (typeSpecToType tenv) args,
                  map (typeSpecToType tenv) res)

    (* Type of words *)

    fun wordType _ (BoolWord _) = litType Bool
      | wordType _ (IntWord _) = litType Int
      | wordType wenv (NamedWord name) =
        case findWordType wenv name of
            (SOME ty) => ty
          | NONE => raise Fail "No such word"
    and litType ty =
        WordTypeSig ([], [ty])

    fun typeCheck (wenv: wenv) (word_type: word_type) (words: word list) =
        let val (WordTypeSig (initial_stack, final_stack)) = word_type
        in
            let val final_stack' = typeCheckWords wenv initial_stack words
            in
                if final_stack' = final_stack then
                    ()
                else
                    raise Fail ("Type error: final state of the stack doesn't match stack effect signature. The final state of the stack, according to the stack effect signature, is\n\n    "
                                ^ (stackStateToString final_stack)
                                ^ "\n\nWhereas the final state of the stack, according to the code, is\n\n    "
                                ^ (stackStateToString final_stack'))
            end
        end

    and typeCheckWords (wenv: wenv) (current_stack: ty list) (words: word list) =
        case words of
            (word::rest) => let val (WordTypeSig (word_args, word_res)) = wordType wenv word
                            in
                                if argsCompatible current_stack word_args then
                                    typeCheckWords wenv
                                                   (addResults (consumeArgs current_stack word_args)
                                                               word_res)
                                                   rest
                                else
                                    raise Fail "Type error"
                            end
         | nil => current_stack

    and argsCompatible (stack: ty list) (args: ty list) =
        stack = args

    and consumeArgs stack arglist =
        List.take (stack, (List.length stack) - (List.length arglist))

    and addResults stack results =
        stack @ results

    (* Backend *)

    fun sepBy sep strings = String.concatWith sep strings

    fun commaSep strings = sepBy ", " strings

    fun compileType Bool =
        "i1"
      | compileType (Int ) =
        "i32"
      | compileType (WordType (args, res)) =
        "{" ^ (commaSep (map compileType res)) ^ "} (" ^ (commaSep (map compileType args))

    val registerCount = ref 0

    fun freshRegister () =
        (registerCount := !registerCount + 1;
         !registerCount)

    fun lastNRegisters n =
        List.tabulate (n, fn i => !registerCount - i)

    fun renderRegister r =
        "%" ^ "_r" ^ (Int.toString r)

    datatype tast = TBoolWord of bool * ty
                  | TIntWord of string * ty
                  | TNamedWord of name * word_type

    fun augment _ (BoolWord b) =
        TBoolWord (b, Bool)
      | augment _ (IntWord i) =
        TIntWord (i, Int)
      | augment wenv (NamedWord name) =
        TNamedWord (name, wordType wenv (NamedWord name))

    fun augmentWords wenv words =
        map (augment wenv) words

    fun compileDefinition name word_type words =
        let val (WordTypeSig (args, res)) = word_type
        in
            let val rtype = "{" ^ (commaSep (map compileType res)) ^ "}"
                and argtypes = commaSep (map makeArg args)
            in
                let val header = "define " ^ rtype ^ " @" ^ name ^ "(" ^ argtypes ^ ") {\n  "
                    and compiledWords = map compileWord words
                    and footer = "\n}"
                in
                    let val retregs = map (fn ((ty, idx), reg) =>
                                              let val retreg = freshRegister ()
                                              in
                                                  "  "
                                                  ^ (renderRegister retreg)
                                                  ^ " = insertvalue "
                                                  ^ rtype
                                                  ^ " "
                                                  ^ (if idx = 0 then "undef" else (renderRegister (!registerCount - 1)))
                                                  ^ ", "
                                                  ^ (compileType ty)
                                                  ^ " "
                                                  ^ (renderRegister reg)
                                                  ^ ", "
                                                  ^ (Int.toString idx)
                                              end)
                                          (ListPair.zip (enumerate res, lastNRegisters (List.length res)))
                    in
                        let val ret = "\n  ret { "
                                      ^ (String.concatWith ", "
                                                           (map compileType res))
                                      ^ " } "
                                      ^ (renderRegister (!registerCount))
                        in
                            header
                            ^ (String.concatWith "\n  " compiledWords)
                            ^ "\n"
                            ^ (String.concatWith "\n  " retregs)
                            ^ ret
                            ^ footer
                        end
                    end
                end
            end
        end

    and makeArg t =
        (compileType t) ^ " " ^ (renderRegister (freshRegister ()))

    and compileWord (TBoolWord (b, ty)) =
        compileConstant ty
                        (if b then "true" else "false")
      | compileWord (TIntWord (i, ty)) =
        compileConstant ty i
      | compileWord (TNamedWord (name, ty)) =
        compileNamedWord name ty

    and compileConstant ty value =
        let val reg = freshRegister ()
            and ty' = compileType ty
        in
            (renderRegister reg) ^ " = select i1 true, " ^ ty' ^ " " ^ value ^ ", " ^ ty' ^ " " ^ value
        end

    and compileNamedWord name ty =
        let val (WordTypeSig (args, res)) = ty
        in
            let val retTy = "{" ^ (commaSep (map compileType res)) ^ "}"
                and args' = String.concatWith ", "
                                              (map (fn (ty, reg) =>
                                                       (compileType ty) ^ " " ^ (renderRegister reg))
                                                   (ListPair.zip (args, (lastNRegisters (List.length args)))))
            in
                let val tupres = freshRegister ()
                in
                    let val call = (renderRegister tupres) ^ " = call " ^ retTy ^ " @" ^ name ^ "(" ^ args' ^ ")"
                    in
                        (* Extract result values *)
                        let val extracted = map (fn (_, idx) =>
                                                    let val fresh = freshRegister()
                                                    in
                                                        (renderRegister fresh)
                                                        ^ " = extractvalue "
                                                        ^ retTy
                                                        ^ " "
                                                        ^ (renderRegister tupres)
                                                        ^ ", "
                                                        ^ (Int.toString idx)
                                                    end)
                                                (enumerate res)
                        in
                            call ^ "\n  " ^ (String.concatWith "\n  " extracted)
                        end
                    end
                end
            end
        end

    (* Compiler *)

    fun forceWordType (WordType wt) = WordTypeSig wt
      | forceWordType t = WordTypeSig (nil, [t])

    fun compile (tenv: tenv) (wenv: wenv) (WordDefinition (name, type_spec, words)) =
        let val word_ty = forceWordType (typeSpecToType tenv type_spec)
        in
            if not (hasWord wenv name) then
                (typeCheck wenv word_ty words;
                 let val wenv' = addWord wenv name word_ty
                 in
                     let val words' = augmentWords wenv words
                     in
                         let val code = compileDefinition name word_ty words'
                         in
                             (wenv', code)
                         end
                     end
                 end)
            else
                raise Fail ("Word '" ^ name ^ "' is already defined")
        end

    (* Interface *)

    fun readUntilBlank () =
        case (TextIO.inputLine TextIO.stdIn) of
            (SOME s) => if s = "\n" then
                            ""
                        else
                            (s ^ (readUntilBlank ()))
          | NONE => OS.Process.terminate OS.Process.success

    fun repl () =
        let fun repl' (tenv: tenv) (wenv: wenv) =
                (print "> ";
                 let val s = readUntilBlank ()
                 in
                     let val (def, _) = Parser.parseString s
                     in
                         let val (wenv', code) = compile tenv wenv def
                         in
                             print code;
                             print "\n";
                             repl' tenv wenv';
                             ()
                         end
                     end
                 end
                 handle (Fail s) => (print s; print "\n\n"; repl' tenv wenv))
        in
            repl' defaultTypeEnv defaultWordEnv
        end
end
