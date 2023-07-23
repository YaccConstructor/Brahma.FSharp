namespace Brahma.FSharp.OpenCL.Translator.QuotationTransformers

open Brahma.FSharp.OpenCL.Translator
open FSharp.Quotations

type Context =
    { FreeVariables: Map<Var, List<Var>>
      Substitution: Map<Var, Expr> }

    member this.Update(oldFun, newFunVar, freeVars) =
        let newApplication =
            freeVars |> List.map Expr.Var |> Utils.makeApplicationExpr (Expr.Var newFunVar)

        { FreeVariables = this.FreeVariables.Add(oldFun, freeVars)
          Substitution = this.Substitution.Add(oldFun, newApplication) }

    static member empty =
        { FreeVariables = Map.empty
          Substitution = Map.empty }

module Lift =
    module Parameters =
        let private collectFreeVars ctx expr =
            let localFreeVars = Utils.collectFreeVars expr

            Utils.collectFreeFunctionVars expr
            |> Set.map (ctx.FreeVariables.TryFind >> Option.defaultValue List.empty >> Set.ofList)
            |> Set.unionMany
            |> Set.union localFreeVars
            |> Set.toList

        /// head: t, args: [x1: t1; x2: t2; x3: t3]
        /// newHead: t1 -> t2 -> t3 -> t
        let private createFunctionVar (source: Var) (args: List<Var>) =
            args
            |> List.map (fun x -> x.Type)
            |> Utils.makeFunctionType source.Type
            |> fun t -> Var(source.Name, t, source.IsMutable)

        let lift =
            let rec run (ctx: Context) =
                function
                | Patterns.LetFunc(f, definition, inExp) ->
                    let freeVars = collectFreeVars ctx definition

                    let definition' =
                        run ctx definition // body
                        |> Utils.makeLambdaExpr freeVars

                    let f' = createFunctionVar f freeVars

                    let inExp' = run (ctx.Update(f, f', freeVars)) inExp

                    Expr.Let(f', definition', inExp')
                | Patterns.LetVar(v, definition, inExp) ->
                    let definition' = run ctx definition
                    let inExp' = run ctx inExp

                    Expr.Let(v, definition', inExp')
                | ExprShape.ShapeVar var as expr -> ctx.Substitution.TryFind var |> Option.defaultValue expr
                | ExprShape.ShapeLambda(x, body) -> Expr.Lambda(x, run ctx body)
                | ExprShape.ShapeCombination(o, exprList) ->
                    ExprShape.RebuildShapeCombination(o, List.map (run ctx) exprList)

            run Context.empty

    module UnitArguments =
        let inline private unitExpFilter (args: list< ^a > when ^a: (member Type: System.Type)) =
            match args with
            | [] -> failwith "Arguments cannot be empty"
            | [ _ ] -> args
            | _ ->
                // TODO() if several units ???
                args |> List.filter (fun arg -> arg.Type <> typeof<unit>)

        /// args: [x1: t1; x2: t2; x3: t3], boyd: t4
        /// newVar: t1 -> t2 -> t3 -> t4
        let private createFunctionVar (body: Expr) (args: Var list) (var: Var) =
            args
            |> List.map (fun var -> var.Type)
            |> Utils.makeFunctionType body.Type
            |> fun t -> Var(var.Name, t, var.IsMutable)

        // application like <@ f () @> represented as Application(f, Value(<null>));
        // Value(<null>) in Applications patterns go to []
        // Then i think we should map [] -> [ Value((), typeof<unit>) ] in exps
        let private mapExpressions =
            List.map (function
                | [] -> [ Expr.Value((), typeof<unit>) ]
                | x -> x)
            >> List.concat

        let cleanUp (expr: Expr) =
            let rec parse (subst: Map<Var, Var>) =
                function
                | Patterns.LetFuncUncurry(var, args, body, inExpr) ->
                    let args' = unitExpFilter args
                    let var' = createFunctionVar body args' var
                    let body' = parse subst body |> Utils.makeLambdaExpr args'
                    let inExpr' = parse (subst.Add(var, var')) inExpr

                    Expr.Let(var', body', inExpr')
                | DerivedPatterns.Applications(Patterns.Var var, exps) as source ->
                    subst.TryFind var
                    |> Option.map (fun var' ->
                        // TODO(what about exp with unit type???)
                        let exps' = mapExpressions exps |> unitExpFilter

                        Utils.makeApplicationExpr <| Expr.Var var' <| List.map (parse subst) exps')
                    |> Option.defaultValue source
                | ExprShape.ShapeLambda(var, body) -> Expr.Lambda(var, parse subst body)
                | ExprShape.ShapeVar var as source ->
                    subst.TryFind var
                    |> Option.bind (fun _ -> failwithf "First-Order functions (just like curring) is not supported.")
                    |> Option.defaultValue source
                | ExprShape.ShapeCombination(o, exprList) ->
                    let exprList' = List.map <| parse subst <| exprList
                    ExprShape.RebuildShapeCombination(o, exprList')

            parse Map.empty expr

    module Lambda =
        let rec lift =
            function
            | Patterns.LetFunc(var, body, inExpr) ->
                let body', bodyMethods = lift body
                let inExpr', inExprMethods = lift inExpr

                inExpr', bodyMethods @ [ (var, body') ] @ inExprMethods
            | ExprShape.ShapeLambda(var, body) ->
                let body', methods = lift body

                Expr.Lambda(var, body'), methods
            | ExprShape.ShapeVar var -> Expr.Var(var), List.empty
            | ExprShape.ShapeCombination(o, exprList) ->
                let exprList', methods = exprList |> List.map lift |> List.unzip
                ExprShape.RebuildShapeCombination(o, exprList'), List.concat methods

    let parse (expr: Expr) =
        expr |> Parameters.lift |> UnitArguments.cleanUp |> Lambda.lift
