namespace rec Brahma.FSharp.OpenCL.Translator

open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.AST

[<AbstractClass>]
type Method(var: Var, expr: Expr) =
    member this.FunVar = var
    member this.FunExpr = expr

    member internal this.AddReturn(subAST) =
        let rec adding (stmt: Statement<'lang>) =
            match stmt with
            | :? StatementBlock<'lang> as sb ->
                let listStaments = sb.Statements
                let lastStatement = listStaments.[listStaments.Count - 1]
                sb.Remove(listStaments.Count - 1)
                sb.Append(adding lastStatement)
                sb :> Statement<_>
            | :? Expression<'lang> as ex -> Return ex :> Statement<_>
            | :? IfThenElse<'lang> as ite ->
                let newThen = adding ite.Then :?> StatementBlock<_>

                let newElse =
                    if Option.isNone ite.Else then
                        None
                    else
                        Some(adding ite.Else.Value :?> StatementBlock<_>)

                IfThenElse(ite.Condition, newThen, newElse) :> Statement<_>
            | _ -> failwithf $"Unsupported statement to add Return: %A{stmt}"

        adding subAST

    abstract TranslateBody: Var list * Expr -> State<TargetContext, StatementBlock<Lang>>

    default this.TranslateBody(args, body) =
        translation {
            let! context = State.get

            context.Namer.LetIn()
            args |> List.iter (fun v -> context.Namer.AddVar v.Name)

            let! newBody = Body.translate body

            return
                match newBody with
                | :? StatementBlock<Lang> as sb -> sb
                | :? Statement<Lang> as s -> StatementBlock <| ResizeArray [ s ]
                | _ -> failwithf $"Incorrect function body: %A{newBody}"
        }

    abstract TranslateArgs: Var list * string list * string list -> State<TargetContext, FunFormalArg<Lang> list>

    abstract BuildFunction: FunFormalArg<Lang> list * StatementBlock<Lang> -> State<TargetContext, ITopDef<Lang>>

    abstract GetTopLevelVarDecls: unit -> State<TargetContext, ITopDef<Lang> list>

    default this.GetTopLevelVarDecls() =
        translation {
            let! context = State.get

            return context.TopLevelVarsDecls |> Seq.cast<_> |> List.ofSeq
        }

    abstract Translate: string list * string list -> State<TargetContext, ITopDef<Lang> list>

    default this.Translate(globalVars, localVars) =
        translation {
            // TODO move it to translator?
            do! State.modify (fun context -> context.WithNewLocalContext())

            match expr with
            | DerivedPatterns.Lambdas(args, body) ->
                let args = List.concat args
                let! translatedArgs = this.TranslateArgs(args, globalVars, localVars)
                let! translatedBody = this.TranslateBody(args, body)
                let! func = this.BuildFunction(translatedArgs, translatedBody)
                let! topLevelVarDecls = this.GetTopLevelVarDecls()

                return topLevelVarDecls @ [ func ]

            | _ -> return failwithf $"Incorrect OpenCL quotation: %A{expr}"
        }

    override this.ToString() = $"%A{var}\n%A{expr}"

type KernelFunc(var: Var, expr: Expr) =
    inherit Method(var, expr)

    override this.TranslateArgs(args, _, _) =
        translation {
            let! context = State.get

            let brahmaDimensionsTypes = [ Range1D_; Range2D_; Range3D_ ]

            return
                args
                |> List.filter (fun variable ->
                    brahmaDimensionsTypes
                    |> (not << List.contains (variable.Type.Name.ToLowerInvariant())))
                |> List.map (fun variable ->
                    let vType = Type.translate variable.Type |> State.eval context
                    let declSpecs = DeclSpecifierPack(typeSpecifier = vType)

                    if vType :? RefType<_> then
                        declSpecs.AddressSpaceQualifier <- Global

                    FunFormalArg(declSpecs, variable.Name))
        }

    override this.BuildFunction(args, body) =
        translation {
            let retFunType = PrimitiveType Void :> Type<_>
            let declSpecs = DeclSpecifierPack(typeSpecifier = retFunType, funQualifier = Kernel)
            return FunDecl(declSpecs, var.Name, args, body) :> ITopDef<_>
        }

type Function(var: Var, expr: Expr) =
    inherit Method(var, expr)

    override this.TranslateArgs(args, globalVars, localVars) =
        translation {
            let! context = State.get

            return
                args
                |> List.map (fun variable ->
                    let vType = Type.translate variable.Type |> State.eval context
                    let declSpecs = DeclSpecifierPack(typeSpecifier = vType)

                    if vType :? RefType<_> && globalVars |> List.contains variable.Name then
                        declSpecs.AddressSpaceQualifier <- Global
                    elif vType :? RefType<_> && localVars |> List.contains variable.Name then
                        declSpecs.AddressSpaceQualifier <- Local

                    FunFormalArg(declSpecs, variable.Name))
        }

    override this.BuildFunction(args, body) =
        translation {
            let! context = State.get

            let retFunType = Type.translate var.Type |> State.eval context
            let declSpecs = DeclSpecifierPack(typeSpecifier = retFunType)

            let partAST =
                match retFunType with
                | :? PrimitiveType<Lang> as t when t.Type = Void -> body :> Statement<_>
                | _ -> this.AddReturn(body)

            return FunDecl(declSpecs, var.Name, args, partAST) :> ITopDef<_>
        }
